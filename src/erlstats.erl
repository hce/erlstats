%%%-------------------------------------------------------------------
%%% File    : erlstats.erl
%%% Author  : Hans-Christian Esperer <hc@hcesperer.org>
%%% Description : erlstats main server
%%%
%%% Created :  5 Feb 2012 by hc <hc@hc-laptop>
%%%-------------------------------------------------------------------
-module(erlstats).

-behaviour(gen_server).

-include("erlstats.hrl").

%% API
-export([start_link/1]).

%% API functions
-export([
	 irc_kill/3,
	 irc_kline/4,
	 irc_cmode/3,
	 irc_notice/3
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  socket,
	  sid,
	  usertable,
	  ntuidtable,
	  servertable,
	  channeltable,
	  me,
	  connectiondetails,
	  remotepassword,
	  uplinkcapabilities,
	  uplinkuid,
	  plugins,
	  initialized,
	  pending_plugin_inits,
	  burst
	 }).

-define(SERVER, ?MODULE).

-define(DEBUG, esmisc:log).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, Port, Nodename, SID, Password, Remotepassword, Node_Description]=Connectiondetails) ->
    process_flag(trap_exit, true),
    
    {ok, S} = gen_tcp:connect(Host, Port,
			      [binary,
			       {active, true},
			       {packet, line}]),

    ts6:sts_login(S, SID, Nodename, Password, Node_Description),

    Usertable   = ets:new(usertable,      [set, protected, {keypos, 2}]),
    Chantable   = ets:new(chantable,      [set, protected, {keypos, 2}]),
    Servertable = ets:new(servertable,    [set, protected, {keypos, 2}]),
    NTUIDtable  = ets:new(nicktouidtable, [set, protected, {keypos, 1}]),
    
    ME = #ircserver{
      sid=SID,
      hostname=Nodename,
      distance=0,
      description=Node_Description,
      uplink=undefined},
    ets:insert(Servertable, ME),
    
    {ok, #state{
       socket=S,
       sid=SID,
       usertable=Usertable,
       ntuidtable=NTUIDtable,
       servertable=Servertable,
       channeltable=Chantable,
       me=ME,
       connectiondetails=Connectiondetails,
       remotepassword=Remotepassword,
       plugins=[],
       initialized=false,
       pending_plugin_inits=[],
       burst=true
      }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({register_plugin, Handledcommands}, {PID, _Tag}, State) ->
    Plugins = State#state.plugins,
    PPI = State#state.pending_plugin_inits,

    Plugin = #ircplugin{
      pid=PID,
      users=[],
      handledcommands=Handledcommands
     },
    link(PID),                   %% Link to the plugin process so that we notice if it crashes.
                                 %% And also so that it crashes if we crash (which god forbid)
    Plugins_U = [Plugin|Plugins],

    %% If we are already initialised,
    %% our plugin can initialise right away.
    %% Otherwise, postpone the plugin initialisation
    PPI_U = case State#state.initialized of
		true ->
		    gen_server:cast(PID, initialize),
		    PPI;
		false ->
		    [PID|PPI]
	    end,
    
    {reply, ok,
     State#state{
       plugins=Plugins_U,
       pending_plugin_inits=PPI_U
      }
    };
handle_call({register_user, NID, Nick, Ident, Host, Gecos, MiscDescription, Sourcemodule}, {PID, _Tag}, State) ->
    Plugin = findplugin(State, PID),
    Users = Plugin#ircplugin.users,
    SID = State#state.sid,
    UID = << SID/binary,
	     NID/binary >>,
    Serverdata_e = dict:new(),
    Serverdata_1 = dict:store(description, MiscDescription, Serverdata_e),
    Serverdata_2 = dict:store(pluginpid, PID, Serverdata_1),
    Serverdata   = dict:store(pluginmodule, Sourcemodule, Serverdata_2),
    User = #ircuser{
      uid=UID,
      sid=SID,
      nick=Nick,
      hop=0,    % Our own
      ts=1337,  % Some time definitely lower than any real user, so we always have precedence
      modes="", % No modes for our user
      ident=Ident,
      host=Host,
      ip= << "127.0.0.1" >>,
      realname=Gecos,
      channels=sets:new(),
      away=undefined,
      authenticated=server,
      serverdata=Serverdata
     },
    ets:insert(State#state.usertable, User),
    ets:insert(State#state.ntuidtable, {Nick, UID}),
    ts6:sts_newuser(State#state.socket, User),
    Plugin_U = Plugin#ircplugin{
		 users=[User|Users]
		 },
    Plugins_U = updateplugin(State, Plugin_U),
    {reply, {ok, User},
     State#state{
       plugins=Plugins_U
      }
    };

handle_call({irc_kill, Killer, Killee, Reason}, _From, State) ->
    Killername = (resolveuser(State, Killer))#ircuser.nick,
    ts6:sts_kill(State#state.socket, Killer, Killername, Killee, Reason),
    delfromntuidtable(State, Killee),
    ets:delete(State#state.usertable, Killee),
    {reply, ok, State};

handle_call({irc_kline, Kliner, Host, Expiry, Reason}, _From, State) ->
    ts6:sts_kline(State#state.socket, Kliner,
		  Host, Expiry, Reason),
    {reply, ok, State};

handle_call({irc_cmode, Modesetter, Channel, Modes}, _From, State) ->
    [Channel_P] = ets:lookup(State#state.channeltable, Channel),
    ts6:sts_cmode(State#state.socket,
		  if Modesetter == server ->
			  (State#state.me)#ircserver.hostname;
		     true -> Modesetter end,
		  Channel,
		  Channel_P#ircchannel.ts,
		  Modes),

    Channel_U = esmisc:parsecmode(Channel_P, Modes),
    ?DEBUG("Updating channel ~p with ~p.", [Channel, Modes]),
    ets:insert(State#state.channeltable, Channel_U),
    {reply, ok, State};

handle_call({irc_raw, Command}, _From, State) ->
    gen_tcp:send(State#state.socket, [Command, 10]),
    {reply, ok, State};

handle_call(getusertable, _From, State) ->
    {reply, {ok, State#state.usertable}, State};

handle_call(getservertable, _From, State) ->
    {reply, {ok, State#state.servertable}, State};

handle_call(getchanneltable, _From, State) ->
    {reply, {ok, State#state.channeltable}, State};

handle_call({getuserfromnick, Usernick}, _From, State) ->
    Result = case ets:lookup(State#state.ntuidtable, Usernick) of
		 [] ->
		     [];
		 [{Usernick, UID}] ->
		     ets:lookup(State#state.usertable, UID)
	     end,
    {reply, {ok, Result}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({irccmd_notice, Noticer, Noticee, Notice}, State) ->
    ts6:sts_notice(State#state.socket,
		   Noticer, Noticee, Notice),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp, _Socket, Data}, State) ->
    Len = size(Data) - 2,  % Substract the length of \r\n
    << Data_wr:Len/binary, _CRLF/binary >> = Data,

    ?DEBUG("Parsing: ~s", [Data_wr]),
    Newstate = case parseline(Data_wr) of
		   [Instigator, Command|Params] ->
		       Command_atom = esmisc:atomorunknown(Command),
		       irccmd(Command_atom, State, Instigator, Params);
		   Else ->
		       error_logger:error_msg("Unable to handle received line ~p", [Else]),
		       State
    end,
    
    {noreply, Newstate};

handle_info({'EXIT', FromPid, Reason}, State) ->
    ?DEBUG("~p has died for reason ~p.", [FromPid, Reason]),
    State1 = case findplugin(State, FromPid) of
		 #ircplugin{users=Users}=Plugin ->
		     Delres = deleteplugin(State, FromPid),
		     ?DEBUG("~p was a registered plugin with ~p user(s); removing it",
			    [FromPid, length(Users)]),
		     remove_plugin_users(State, FromPid, Plugin,
					 "Plugin's gen_server process died"),
		     Delres;
		 noplugin ->
		     ?DEBUG("~p was not a registered plugin", [FromPid]),
		     State
	     end,
    {noreply, State1};

handle_info(_Info, State) ->
    {noreply, State}.


irc_kill(Killer, Killee, Reason) ->
    gen_server:call(erlstats, {irc_kill, Killer, Killee, Reason}).

irc_kline(Kliner, Host, Timeout, Reason) ->
    gen_server:call(erlstats, {irc_kline, Kliner, Host, Timeout, Reason}).

irc_cmode(Modesetter, Channel, Modes) ->
    gen_server:call(erlstats, {irc_cmode, Modesetter, Channel, Modes}).

irc_notice(Noticer, Noticee, Notice) ->
    gen_server:cast(erlstats, {irccmd_notice, Noticer, Noticee, Notice}).


%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ?DEBUG("Terminating!"),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
parseline(<< Firstbyte:8, Rest/binary >>=Line)  ->
    if
	Firstbyte == $: ->
	    parseline(rest, Rest);
	true ->
	    [[]|parseline(rest, Line)]
    end.

parseline(rest, Line) ->
    [NormalParams|Textparam] = binary:split(Line, << " :" >>),
    NormalParamList = binary:split(NormalParams, << " " >>, [global]),
    NormalParamList ++ Textparam.

irccmd(notice, State, Instigator, [<<"AUTH">>, Authprocess]) ->
    ?DEBUG("Authentication Handshake with ~p: ~p", [Instigator, Authprocess]),
    State;

irccmd(notice, State, Instigator, Params) ->
    ?DEBUG("Got ~p from ~p", [Params, Instigator]),
    State;

irccmd(ping, State, [], [Pongparam]) ->
    ?DEBUG("Sending PONG ~p", [Pongparam]),
    ts6:sts_pong(State#state.socket, Pongparam),
    State;

irccmd(pong, State, _Someone, _Someparams) ->
    ?DEBUG("~p replied to our PING: ~p", [_Someone, _Someparams]),
    if
	State#state.burst ->
	    error_logger:info_msg("Synchronized with network."),
	    State#state{burst=false};
	true ->
	    State
    end;
    
irccmd(uid, State, SID, [Nick, Hops, TS,
			 Usermode, Ident, Hostname,
			 Ipaddr, UID, Gecos]) ->
    TS_I = list_to_integer(binary_to_list(TS)),
    Parsedmodes = esmisc:parseumode(Usermode),
    User = #ircuser{
      uid=UID,
      sid=SID,
      nick=Nick,
      hop=Hops,
      ts=TS_I,
      modes=Parsedmodes,
      ident=Ident,
      host=Hostname,
      ip=Ipaddr,
      realname=Gecos,
      channels=sets:new(),
      authenticated=false,
      away=undefined
     },

    ets:insert(State#state.ntuidtable, {Nick, UID}),
    ets:insert(State#state.usertable, User),

    ?DEBUG("New user: ~p", [User]),

    Pluginparams = #irccmduid{
      server=SID,
      nick=Nick,
      hops=Hops,
      ts=TS_I,
      modes=Parsedmodes,
      ident=Ident,
      hostname=Hostname,
      ip=Ipaddr,
      uid=UID,
      gecos=Gecos,
      burst=State#state.burst
     },

    plugincommand(State, uid, Pluginparams),
    
    State;

irccmd(kill, State, Killer, [Killee, Reason]) ->
    ?DEBUG("Received KILL for ~s from ~s (Reason ~s)",
			  [(resolveuser(State, Killee))#ircuser.nick,
			   (resolveuser(State, Killer))#ircuser.nick,
			   Reason]),
    MYSID = (State#state.me)#ircserver.sid,
    Lookitupfirst = ets:lookup(State#state.usertable, Killee),
    delfromntuidtable(State, Killee),
    ets:delete(State#state.usertable, Killee),
    Newstate = case Lookitupfirst of
		   [#ircuser{sid=MYSID}=User] ->
		       handle_plugin_nick_kill(State, User);
		   _Else ->
		       State
	       end,
    Newstate;

irccmd(quit, State, Quitter, [Reason]) ->
    ?DEBUG("User ~s quit (Reason: ~s)",
			  [(resolveuser(State, Quitter))#ircuser.nick,
			   Reason]),
    channel_handlequit(State, Quitter),
    delfromntuidtable(State, Quitter),
    ets:delete(State#state.usertable, Quitter),
    State;

irccmd(pass, State, [], [RemotePassword, TS, TS_Version, Uplink_UID]) ->
    if
	State#state.remotepassword =/= RemotePassword ->
	    error_logger:info_msg("Server ~p authenticated with the wrong password!", [Uplink_UID]),
	    exit(error); %% Let OTP restart us or whatever :-)
	true ->
	    error_logger:info_msg("Server ~p correctly authenticated with us.", [Uplink_UID])
    end,
    ?DEBUG("TS params of server: ~p ~p",
			  [TS, TS_Version]),
    State#state{
      uplinkuid=Uplink_UID
     };

irccmd(capab, State, [], [CapabilityList]) ->
    Capabilities = binary:split(CapabilityList, << " " >>, [global]),
    ?DEBUG("Our uplink's capabilities: ~p", [Capabilities]),
    State#state{
      uplinkcapabilities=Capabilities
     };

irccmd(server, State, [], [ServerHostname, Hops, ServerDescription]) ->
    Hops_I = list_to_integer(binary_to_list(Hops)),
    Server = #ircserver{
      sid=State#state.uplinkuid,
      hostname=ServerHostname,
      distance=Hops_I,
      description=ServerDescription,
      uplink=undefined % Direct connection
     },
    error_logger:info_msg("New IRC server: ~p", [Server]),
    ets:insert(State#state.servertable, Server),
    State;

irccmd(sid, State, Uplink, [Servername, Hops, SID, Serverdescription]) ->
    Hops_I = list_to_integer(binary_to_list(Hops)),
    Server = #ircserver{
      sid=SID,
      hostname=Servername,
      distance=Hops_I,
      description=Serverdescription,
      uplink=Uplink
     },
    error_logger:info_msg("New IRC server: ~p", [Server]),
    ets:insert(State#state.servertable, Server),
    State;

irccmd(squit, State, [], [SID, Reason]) ->
    Affected_servers = serversbehind(State, SID),
    Affected_users = usersaffected(State, Affected_servers),
    error_logger:info_msg("Server ~p split, reason: ~p. Affected servers: ~p. Affected users: ~p",
			  [SID, Reason, Affected_servers,
			   length(Affected_users)]),
    lists:foreach(fun(E) ->
			  ets:delete(State#state.servertable, E)
		  end, Affected_servers),
    lists:foreach(fun(E) ->
			  channel_handlequit(State, E),
			  delfromntuidtable(State, E),
			  ets:delete(State#state.usertable, E)
		  end, Affected_users),
    ?DEBUG("Servertable now ~p entries, user table ~p entries.",
	   [ets:info(State#state.servertable, size),
	    ets:info(State#state.usertable, size)]),
    State;

irccmd(svinfo, State, [], _Parameters) ->
    lists:foreach(fun(PID) ->
			  gen_server:cast(PID, initialize)
		  end, State#state.pending_plugin_inits),
    ts6:sts_ping(State#state.socket, (State#state.me)#ircserver.sid),
    State#state{initialized=true,
		pending_plugin_inits=[]};

irccmd(nick, State, NickChanger, [Newnick, TS]) ->
    User = resolveuser(State, NickChanger),
    delfromntuidtable(State, User#ircuser.nick),
    ets:insert(State#state.ntuidtable, {Newnick, NickChanger}),
    ?DEBUG("~s (~s) is changing their nick to ~s at ~p",
			  [User#ircuser.nick, NickChanger,
			   Newnick, TS]),
    User_Updated = User#ircuser{
		     nick=Newnick,
		     ts=TS
		    },
    ets:insert(State#state.usertable, User_Updated),
    State;

irccmd(mode, State, _Changer, [Changee, Newmodes]) ->
    User = resolveuser(State, Changee),
    Resultingmodes = esmisc:parseumode(User#ircuser.modes, Newmodes),
    User_Updated = User#ircuser{
		     modes=Resultingmodes
		    },
    ets:insert(State#state.usertable, User_Updated),
    ?DEBUG("Mode change for user ~s: ~s => ~s",
			  [User#ircuser.nick, Newmodes, Resultingmodes]),
    State;

irccmd(sjoin, State, _Introducer, [TS, Name|ModesAndUserUIDs]) ->
    {Modes, [UserUIDs]} = lists:split(length(ModesAndUserUIDs) - 1,
				      ModesAndUserUIDs),

    Users = esmisc:parsejjusers(UserUIDs),

    Channel = case ets:lookup(State#state.channeltable, Name) of
		  [] -> #ircchannel{
		    channame=Name,
		    bans=[],
		    banexps=[],
		    invexps=[],
		    chankey=undefined,
		    modes=[],
		    users=dict:new(),
		    topic=[],
		    ts=list_to_integer(binary_to_list(TS))
		   };
		  [A_Channel] ->
		      A_Channel
	      end,
    Channel_U = esmisc:parsecmode(Channel, list_to_integer(binary_to_list(TS)), normal, Modes),
    Channel_U2 = esmisc:addchanusers(Channel_U, Users),
    ets:insert(State#state.channeltable, Channel_U2),
    channel_addusers(State, Users, Name),
    State;

irccmd(join, State, UID, [TS, Channelname, Chanmodes]) ->
    [Newuser] = esmisc:parsejjusers(UID),
    Channel = case ets:lookup(State#state.channeltable, Channelname) of
		  [] -> #ircchannel{
		    channame=Channelname,
		    bans=[],
		    banexps=[],
		    invexps=[],
		    chankey=undefined,
		    modes=[],
		    users=dict:new(),
		    topic=[],
		    ts=list_to_integer(binary_to_list(TS))
		   };
		  [A_Channel] ->
		      A_Channel
	      end,
    Channel_U = esmisc:addchanusers(Channel, [Newuser]),
    Channel_U2 = esmisc:parsecmode(Channel_U, TS, normal, [Chanmodes]),
    ets:insert(State#state.channeltable, Channel_U2),
    channel_addusers(State, [Newuser], Channelname),
    State;

irccmd(part, State, UID, [Channelname]) ->
    channel_removeusers(State, [#ircchanuser{uid=UID}], Channelname),
    State;

irccmd(tmode, State, Issuer, [TS, Channame|Modestring]) ->
    [Channel] = ets:lookup(State#state.channeltable, Channame),
    ?DEBUG("Updating channel ~s: ~p", [Channame, Modestring]),
    Channel_U = esmisc:parsecmode(Channel, TS, simple, Modestring),
    ets:insert(State#state.channeltable, Channel_U),

    PluginParams = #irccmdtmode{
      issuer=Issuer,
      ts=TS,
      channame=Channame,
      modechanges=Modestring,
      burst=State#state.burst
     },

    plugincommand(State, tmode, PluginParams),
    
    State;

irccmd(privmsg, State, Messager, [<< ChanType:8, ChanName/binary >>=Target, Message]) ->
    case lists:member(ChanType, "#&+!") of
	true ->
	    handle_channel_privmsg(State, Messager, ChanName, Message);
	false ->
	    handle_nick_privmsg(State, Messager, Target, Message)
    end;

irccmd(whois, State, Inquirer, [_Requestedserver, Inquirednick]) ->
    S = State#state.socket,
    ME = State#state.me,
    Hostname = ME#ircserver.hostname,
    case find_plugin_user(State, Inquirednick) of
	user_not_found ->
	    ts6:sts_whoisnotfound(S, Hostname, Inquirer,
				  Inquirednick, "Not on this server");
	User ->
	    ts6:sts_whoisuser(S, Hostname, Inquirer,
			      Inquirednick, User#ircuser.ident,
			      User#ircuser.host, User#ircuser.realname),
	    ts6:sts_whoisserver(S, Hostname, Inquirer,
				Inquirednick, Hostname, ME#ircserver.description),
	    ts6:sts_whoisopinfo(S, Hostname, Inquirer,
				Inquirednick,
				dict:fetch(description, User#ircuser.serverdata))
    end,
    ts6:sts_whoisfinished(S, Hostname, Inquirer, Inquirednick),
    State;

irccmd(encap, State, SourceSID, [_Targets, <<"SU">>, UID, Accountname]) ->
    case ets:lookup(State#state.usertable, UID) of
	[] ->
	    error_logger:info_msg("Error: ~p reports ~p authenticated as ~p, "
				  "but ~p is not in our user table!",
				  [SourceSID, UID, Accountname, UID]);
	[User] ->
	    ?DEBUG("~s authenticates ~s as ~s",
		   [SourceSID, UID, Accountname]),
	    User_U = User#ircuser{authenticated={SourceSID, Accountname}},
	    ets:insert(State#state.usertable, User_U)
    end,
    State;

irccmd(away, State, UID, [Awaymsg]) ->
    case ets:lookup(State#state.usertable, UID) of
	[User] ->
	    User_U = User#ircuser{away=Awaymsg},
	    ets:insert(State#state.usertable, User_U),
	    ?DEBUG("~s ~s is now away: ~s.", [UID, User#ircuser.nick, Awaymsg]);
	[] ->
	    ?DEBUG("UID ~s does not exist!", [UID])
    end,
    State;

irccmd(away, State, UID, []) ->
    case ets:lookup(State#state.usertable, UID) of
	[User] ->
	    User_U = User#ircuser{away=undefined},
	    ets:insert(State#state.usertable, User_U),
	    ?DEBUG("~s ~s is not away anymore.", [UID, User#ircuser.nick]);
	[] ->
	    ?DEBUG("UID ~s does not exist!", [UID])
    end,
    State;

irccmd(topic, State, Setter, [Channelname, Newtopic]) ->
    [Channel] = ets:lookup(State#state.channeltable, Channelname),
    Channel_U = Channel#ircchannel{topic=Newtopic},
    ets:insert(State#state.channeltable, Channel_U),
    ?DEBUG("~s changed ~s's topic to ~s", [Setter, Channelname, Newtopic]),
    State;

irccmd(Command, State, Instigator, Params) ->
    ?DEBUG("Unknown command ~p with instigator ~p and params ~p", [Command, Instigator, Params]),
    State.


resolveuser(#state{usertable=UT}, UID) ->
    [User] = ets:lookup(UT, UID),
    User.

findplugin(#state{plugins=P}, PID) ->
    findplugin(P, PID);
findplugin([#ircplugin{pid=PID}=Plugin|_R], PID) ->
    Plugin;
findplugin([_P|R], PID) ->
    findplugin(R, PID);
findplugin([], _PID) ->
    noplugin.


updateplugin(#state{plugins=P}, #ircplugin{pid=PID}=Updated_Plugin) ->
    updateplugin(P, PID, Updated_Plugin).

updateplugin([P|R], PID, Updated_Plugin) ->
    [if
	 P#ircplugin.pid == PID ->
	     Updated_Plugin;
	 true ->
	     P
     end|updateplugin(R, PID, Updated_Plugin)];
updateplugin([], _PID, _Updated_Plugin) ->
    [].

deleteplugin(#state{plugins=P}=State, PID) ->
    P_U = [Plugin || #ircplugin{pid=PluginPID}=Plugin <- P,
		     PluginPID =/= PID],
    State#state{plugins=P_U}.

plugincommand(#state{plugins=P}, Command, Params) ->
    %% Get a list of all plugins that have requested to
    %% be called back for that message
    Handlingplugins = [Plugin || Plugin <- P,
				 lists:member(Command, Plugin#ircplugin.handledcommands)],

    %% Cast the message to all appropriate plugins
    lists:foreach(fun(#ircplugin{pid=PID}) ->
			  gen_server:cast(PID, {irccmd, Command, Params})
		  end, Handlingplugins).


serversbehind(#state{servertable=Servertable}, SID) ->
    Servers = ets:foldl(fun(E, A) ->
				[E|A]
			end, [], Servertable),
    serversbehind(Servers, [SID]);

serversbehind(Servers, SIDs) ->
    Matching_Servers = serversbehind_filter(Servers, SIDs),
    if
	Matching_Servers =:= SIDs ->
	    Matching_Servers;
	true ->
	    serversbehind(Servers, Matching_Servers)
    end.

serversbehind_filter(Servers, SIDs) when is_list(SIDs) ->
    lists:usort(
      lists:foldl(fun(Server, A) ->
			  case lists:member(Server#ircserver.uplink, SIDs) of
			      true ->
				  [Server#ircserver.sid|A];
			      false ->
				  A
			  end
		  end, [], Servers) ++ SIDs).
    
usersaffected(#state{usertable=Usertable}, SIDs) ->
    ets:foldl(fun(E, A) ->
		      case lists:member(E#ircuser.sid, SIDs) of
			  true ->
			      [E#ircuser.uid|A];
			  false ->
			      A
		      end
	      end, [], Usertable).

			      
		      
handle_channel_privmsg(State, _Messager, _ChanName, _Message) ->
    State.

handle_nick_privmsg(State, Messager, Nickname, Message) ->
    User = find_plugin_user(State, Nickname),
    Messager_User = resolveuser(State, Messager),
    Nickname_Atom = esmisc:atomorunknown(Nickname),
    ?DEBUG("Message to ~p from ~p: ~p", [Nickname_Atom,
					 Messager_User#ircuser.nick,
					 Message]),

    [Message_Cmd_B|Params] = binary:split(Message, <<" ">>, [global]),
    Message_Cmd_A = esmisc:atomorunknown(Message_Cmd_B),

    PID = dict:fetch(pluginpid, User#ircuser.serverdata),
    Pluginmodule = dict:fetch(pluginmodule, User#ircuser.serverdata),
    case {Message_Cmd_A, Params} of
	{help, [Subcommand_I|_]} ->
	    Subcommand = esmisc:atomorunknown(Subcommand_I),
	    handle_plugin_help(User,
			       Messager_User,
			       Pluginmodule,
			       Nickname_Atom,
			       Subcommand);
	 {help, []} ->
	    handle_plugin_help(User, Messager_User,
			       Pluginmodule,
			       Nickname_Atom);
	_Else ->
	    case check_command_permission(Pluginmodule, Nickname_Atom,
					  Messager_User, Message_Cmd_A) of
		true ->
		    gen_server:cast(PID, {privmsg, Nickname_Atom,
					  User,
					  Message_Cmd_A,
					  Messager_User,
					  Params});
		false ->
		    irc_notice(User#ircuser.uid, Messager,
			       "Permission denied")
	    end
    end,
    
    State.

handle_plugin_help(Pluginuser,
		   Askeruser, Pluginmodule,
		   Nickname, Command) ->
    case check_command_permission(Pluginmodule, Nickname,
				  Askeruser, Command) of
	true ->
	    handle_plugin_help(checked, Pluginuser, Askeruser,
			       Pluginmodule, Nickname, Command);
	false ->
	    irc_notice(Pluginuser#ircuser.uid, Askeruser#ircuser.uid,
		       io_lib:format("No help available for ~p.", [Command]))
    end.

handle_plugin_help(checked, Pluginuser,
		   Askeruser, Pluginmodule,
		   Nickname, Command) ->
    AskerUID = Askeruser#ircuser.uid,
    Nickname_U = string:to_upper(atom_to_list(Nickname)),
    Command_U  = string:to_upper(atom_to_list(Command)),
    try erlang:apply(Pluginmodule, cmdhelp, [Nickname, Command]) of
	Proplist when is_list(Proplist) ->
	    {longdesc, Longdesc} = lists:keyfind(longdesc, 1, Proplist),
	    MSG = ["***** \^b", Nickname_U, " Help\^b *****\n"
		   "Help for \^b", Command_U, "\^b:\n \n",
		   Longdesc,
		  "\n***** \^bEnd of Help\^b *****"],
	    irc_notice(Pluginuser#ircuser.uid,
		       AskerUID,
		       MSG);
	Else ->
	    error_logger:info_msg("Erraneous command help ~p", [Else])
    catch _:_ ->
	    irc_notice(Pluginuser#ircuser.uid,
		       AskerUID,
		       io_lib:format("No help available for ~p", [Command]))
    end.

handle_plugin_help(Pluginuser,
		   Askeruser, Pluginmodule,
		   Nickname) ->
    AskerUID = Askeruser#ircuser.uid,
    Nickname_U = string:to_upper(atom_to_list(Nickname)),
    try erlang:apply(Pluginmodule, cmdgenericinfo, [Nickname]) of
	Desc ->
	    Commands = case help_format_commands(Pluginmodule, Nickname,
						 Askeruser) of
			   [] -> "";
			   List ->
			       ["\n \nThe following commands are available:\n", List]
		       end,
	    MSG = ["***** \^b", Nickname_U, " Help\^b *****\n",
		   Desc,
		   Commands,
		   "\n***** \^bEnd of Help\^b *****"],
	    irc_notice(Pluginuser#ircuser.uid,
		       AskerUID,
		       MSG)
    catch _:_ ->
	    irc_notice(Pluginuser#ircuser.uid,
		       AskerUID,
		       "No generic help available for this plugin - REPORT THIS AS A BUG!")
    end.

help_format_commands(Pluginmodule, Nickname, Asker) ->
    try help_format_commands(unsafe, Pluginmodule,
			     Nickname, Asker) of
	Result when is_list(Result) ->
	    Result;
	_Else ->
	    []
    catch _:_ ->
	    []
    end.

help_format_commands(unsafe, Pluginmodule,
		     Nickname, Asker) ->
    List = erlang:apply(Pluginmodule, cmdlist, [Nickname]),
    lists:foldl(
      fun(Cmd_A, Acc) ->
	      case check_command_permission(Pluginmodule, Nickname,
					    Asker, Cmd_A) of
		  true ->
		      Cmd_P = erlang:apply(Pluginmodule, cmdhelp, [Nickname, Cmd_A]),
		      {shortdesc, Cmd_D} = lists:keyfind(shortdesc, 1, Cmd_P),
		      Cmd_U = string:to_upper(atom_to_list(Cmd_A)),
		      Formatted = io_lib:format("\^b~-15s\^b ~s", [Cmd_U, Cmd_D]),
		      if Acc =:= [] ->
			      [Formatted];
			 true ->
			      [Formatted,10|Acc]
		      end;
		  false ->
		      Acc
	      end
      end, [], List).


check_command_permission(Pluginmodule, Nickname,
			 Command_giver, Command) ->
    try erlang:apply(Pluginmodule, cmdperm, [Nickname, Command]) of
	[] ->
	    true;
	Res when is_integer(Res) ->
	    lists:member(Res, Command_giver#ircuser.modes);
	Res when is_function(Res) ->
	    try Res(Command_giver) of
		Result when is_boolean(Result)->
		    Result;
		Else ->
		    error_logger:info_msg("Erraneous result of permission checking function: ~p", [Else]),
		    false
	    catch _:_ ->
		    false
	    end;
	authed ->
	    is_tuple(Command_giver#ircuser.authenticated);
	Else2 ->
	    error_logger:info_msg("Invalid permission function return value: ~p", [Else2])
    catch _:_ ->
	    false
    end.

find_plugin_user(State, Usernick) ->
    Pluginusers =
	lists:flatten(
	  lists:foldl(fun(E, A) ->
			      [E#ircplugin.users|A]
		      end, [], State#state.plugins)),
    find_plugin_user(search, Pluginusers, Usernick).

find_plugin_user(search, [#ircuser{nick=Usernick}=User|_Rest], Usernick) ->
    User;
find_plugin_user(search, [_Someuser|Rest], Usernick) ->
    find_plugin_user(search, Rest, Usernick);
find_plugin_user(search, [], _Usernick) ->
    user_not_found.

remove_plugin_users(State, FromPid, Plugin, Quitreason) ->
    lists:foreach(fun(User) ->
			  remove_plugin_user(State, FromPid, Plugin,
					     User, Quitreason)
		  end, Plugin#ircplugin.users).

remove_plugin_user(State, FromPid, Plugin, User, Quitreason) ->
    Newusers = [U || U <- Plugin#ircplugin.users,
		     U =/= User],
    case ets:lookup(State#state.usertable, User#ircuser.uid) of
	[_Someuser] ->
	    ts6:sts_quituser(State#state.socket, User, Quitreason),
	    delfromntuidtable(State, User#ircuser.uid),
	    ets:delete(State#state.usertable, User#ircuser.uid),
	    ?DEBUG("Removing user ~p from plugin with pid ~p.",
		   [User#ircuser.nick, FromPid]);
	_Else ->
	    ?DEBUG("User ~p from plugin ~p seems to be already removed",
		   [User#ircuser.nick, FromPid])
    end,
    Plugin#ircplugin{users=Newusers}.

handle_plugin_nick_kill(State, User) ->
    PluginPID = dict:fetch(pluginpid, User#ircuser.serverdata),
    exit(PluginPID, irc_killed),
    State.

channel_addusers(#state{usertable=UT}, Users, Channelname) ->    
    lists:foreach(fun(ICU) ->
			  [User] = ets:lookup(UT, ICU#ircchanuser.uid),
			  Channels = User#ircuser.channels,
			  Channels_U = sets:add_element(Channelname, Channels),
			  User_U = User#ircuser{
				     channels=Channels_U
				    },
			  ets:insert(UT, User_U)
		  end, Users).

channel_removeusers(#state{usertable=UT}=State, Users, Channelname) ->
    lists:foreach(fun(ICU) ->
			  UID = ICU#ircchanuser.uid,

			  [Channel] = ets:lookup(State#state.channeltable, Channelname),
			  Channel_U = Channel#ircchannel{
					users=esmisc:removeuser(UID, Channel#ircchannel.users)
				       },
			  case Channel_U#ircchannel.users of
			      [] ->
				  ets:delete(State#state.channeltable, Channelname),
				  ?DEBUG("Channel PART destroys channel ~p.", [Channelname]);
			      _Else ->
				  ets:insert(State#state.channeltable, Channel_U),
				  ?DEBUG("~s got removed from ~s", [ICU#ircchanuser.uid, Channelname])
			  end,
			  
			  [User] = ets:lookup(UT, UID),
			  Channels = User#ircuser.channels,
			  Channels_U = sets:del_element(Channelname, Channels),
			  User_U = User#ircuser{
				     channels=Channels_U
				    },
			  ets:insert(UT, User_U)
		  end, Users).

channel_handlequit(State, UID) ->
    [User] = ets:lookup(State#state.usertable, UID),
    User_R = [#ircchanuser{uid=UID}],
    C = sets:fold(fun(Channelname, Count) ->
			  channel_removeusers(State, User_R, Channelname),
			  Count + 1
		  end, 0, User#ircuser.channels),
    ?DEBUG("User ~s quit; removed them from ~p channel(s).", [User#ircuser.nick, C]).

delfromntuidtable(State, Nick) ->			    
    case ets:lookup(State#state.usertable, Nick) of
	[User] ->
	    ets:delete(State#state.ntuidtable, User#ircuser.nick);
	_Else ->
	    ok
    end.

