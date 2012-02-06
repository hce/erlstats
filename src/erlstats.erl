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
	 irc_kline/3
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  socket,
	  sid,
	  usertable,
	  servertable,
	  channeltable,
	  me,
	  connectiondetails,
	  remotepassword,
	  uplinkcapabilities,
	  uplinkuid,
	  plugins,
	  initialized,
	  pending_plugin_inits
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

    Usertable   = ets:new(usertable,   [set, protected, {keypos, 2}]),
    Chantable   = ets:new(chantable,   [set, protected, {keypos, 2}]),
    Servertable = ets:new(servertable, [set, protected, {keypos, 2}]),
    
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
       servertable=Servertable,
       channeltable=Chantable,
       me=ME,
       connectiondetails=Connectiondetails,
       remotepassword=Remotepassword,
       plugins=[],
       initialized=false,
       pending_plugin_inits=[]
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

handle_call({register_user, NID, Nick, Ident, Host, Gecos, MiscDescription}, {PID, _Tag}, State) ->
    Plugin = findplugin(State, PID),
    Users = Plugin#ircplugin.users,
    SID = State#state.sid,
    UID = << SID/binary,
	     NID/binary >>,
    Serverdata_e = dict:new(),
    Serverdata   = dict:store(description, MiscDescription, Serverdata_e),
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
    ets:delete(State#state.usertable, Killee),
    {reply, ok, State};

handle_call({irc_kline, Host, Expiry, Reason}, _From, State) ->
    ts6:sts_kline(State#state.socket, (State#state.me)#ircserver.hostname,
		  Host, Expiry, Reason),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
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

    ?DEBUG("Parsing ~p", [Data_wr]),
    Newstate = case parseline(Data_wr) of
		   [Instigator, Command|Params] ->
		       Command_lower = string:to_lower(binary_to_list(Command)),
		       Command_atom  = try list_to_existing_atom(Command_lower) of
					   Someatom when is_atom(Someatom) ->
					       Someatom
				       catch _:_ ->
					       unknown
				       end,
						%io:format("Command ~p[~p] Inst ~p Params ~p~n", [Command_atom, Command_lower, Instigator, Params]),
		       irccmd(Command_atom, State, Instigator, Params);
		   Else ->
		       error_logger:error_msg("Unable to handle received line ~p", [Else]),
		       State
    end,
    
    {noreply, Newstate};

handle_info({'EXIT', FromPid, Reason}, State) ->
    error_logger:info_msg("~p has died for reason ~p.", [FromPid, Reason]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


irc_kill(Killer, Killee, Reason) ->
    gen_server:call(erlstats, {irc_kill, Killer, Killee, Reason}).

irc_kline(Host, Timeout, Reason) ->
    gen_server:call(erlstats, {irc_kline, Host, Timeout, Reason}).


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
    io:format("Got ~p from ~p", [Params, Instigator]),
    State;

irccmd(ping, State, [], [Pongparam]) ->
    ?DEBUG("Sending PONG ~p", [Pongparam]),
    ts6:sts_pong(State#state.socket, Pongparam),
    State;

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
      gecos=Gecos
     },

    plugincommand(State, uid, Pluginparams),
    
    State;

irccmd(kill, State, Killer, [Killee, Reason]) ->
    ?DEBUG("Received KILL for ~p from ~p (Reason ~p)",
			  [(resolveuser(State, Killee))#ircuser.nick,
			   (resolveuser(State, Killer))#ircuser.nick,
			   Reason]),
    ets:delete(State#state.usertable, Killee),
    State;

irccmd(quit, State, Quitter, [Reason]) ->
    ?DEBUG("User ~p quit (Reason: ~p)",
			  [(resolveuser(State, Quitter))#ircuser.nick,
			   Reason]),
    ets:delete(State#state.usertable, Quitter),
    State;

irccmd(pass, State, [], [RemotePassword, TS, TS_Version, Uplink_UID]) ->
    if
	State#state.remotepassword =/= RemotePassword ->
	    error_logger:info_msg("Server ~p authenticated with the wrong password!", [Uplink_UID]);
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
    State#state{initialized=true,
		pending_plugin_inits=[]};

irccmd(nick, State, NickChanger, [Newnick, TS]) ->
    User = resolveuser(State, NickChanger),
    ?DEBUG("~p (~p) is changing their nick to ~p at ~p",
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
    ?DEBUG("Mode change for user ~p: ~p => ~p",
			  [User#ircuser.nick, Newmodes, Resultingmodes]),
    State;

irccmd(sjoin, State, _Introducer, [TS, Name|ModesAndUserUIDs]) ->
    {Modes, [UserUIDs]} = lists:split(length(ModesAndUserUIDs) - 1,
				      ModesAndUserUIDs),
    Users = lists:foldl(fun(<< UT:8, UN/binary >>=FullName, A) ->
				[case UT of
				     $@ ->
					 #ircchanuser{uid=UN, privs=op};
				     $% ->
					 #ircchanuser{uid=UN, privs=halfop};
				     $+ ->
					 #ircchanuser{uid=UN, privs=voice};
				     _Else ->
					 #ircchanuser{uid=FullName, privs=undefined}
				 end|A]
			end, [], binary:split(UserUIDs, <<" ">>, [global])),
    Channel = #ircchannel{
      channame=Name,
      bans=[],
      banexps=[],
      invexps=[],
      chankey=undefined,
      modes=[],
      users=Users,
      topic=[],
      ts=list_to_integer(binary_to_list(TS))
     },
    Channel_U = esmisc:parsecmode(Channel, Modes),
    ets:insert(State#state.channeltable, Channel_U),
    ?DEBUG("New channel: ~p", [Channel_U]),
    State;

irccmd(tmode, State, Issuer, [TS, Channame|Modestring]) ->
    [Channel] = ets:lookup(State#state.channeltable, Channame),
    Channel_U = esmisc:parsecmode(Channel, Modestring),
    ets:insert(State#state.channeltable, Channel_U),
    ?DEBUG("Updating channel ~p ~p: ~p", [Channame, Modestring, Channel_U]),
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
    findplugin(R, PID).

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

			      
		      
