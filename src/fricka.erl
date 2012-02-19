%%%-------------------------------------------------------------------
%%% File    : fricka.erl
%%% Author  : hc <hc@hc-laptop>
%%% Description : Miscellaneous functionality user bot
%%%
%%% Created :  6 Feb 2012 by hc <hc@hc-laptop>
%%%-------------------------------------------------------------------
-module(fricka).

-behaviour(gen_server).

-include("erlstats.hrl").

-define(DEBUG, esmisc:log).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Plugin command information
-export([cmdlist/1, cmdhelp/2,
	 cmdperm/2, cmdgenericinfo/1]).

-record(state, {
	  frickauser    %% Out IRC "nick" fricka
	 }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
init([]) ->
    Handledcommands = [
		       uid,   % We need to see who's using bad gecos'
		       tmode  % We need to see all channel mode changes
		      ],
    gen_server:call(erlstats, {register_plugin, Handledcommands}),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(initialize, State) ->
    {ok, User} = gen_server:call(erlstats, {register_user,
					    << "FRICKA" >>,
					    << "fricka" >>,
					    << "hs" >>,
					    << "services.hackint.org" >>,
					    << "Fricka" >>,
					    << "Frech wacht Fricka ueber's HackINT" >>, ?MODULE}),
    timer:send_interval(60000, checkbans), %% Check for bans that we might want to remove every minute
    {noreply, State#state{frickauser=User}};
handle_cast({irccmd, tmode, Params}, State) ->
    Channame = Params#irccmdtmode.channame,
    [Modes|Modeparams] = Params#irccmdtmode.modechanges,
    Nicks_to_unhalfop = find_nicks_to_unhalfop(Channame, Modes, Modeparams),
    Modechars = list_to_binary(["h" || _X <- Nicks_to_unhalfop]),
    erlstats:irc_cmode(server, Channame, [<< "-", Modechars/binary >>|
					  Nicks_to_unhalfop]),
    if
	Nicks_to_unhalfop =/= [] ->
	    erlstats:irc_notice((State#state.frickauser)#ircuser.uid,
				Params#irccmdtmode.issuer,
				<< "HackINT does not support HalfOP. Please do not use them. ChanServ offers a very granular permission system, see \^b/msg chanserv help flags\^b for details or ask in \^b#hackint\^b" >>),
	    error_logger:info_msg("Nicks to unhalfop: ~p", [Nicks_to_unhalfop]);
	true ->
	    ok
    end,
    {noreply, State};
handle_cast({irccmd, uid, #irccmduid{burst=false}=Params}, State) ->
    case binary:match(Params#irccmduid.gecos, <<"http://">>) of
	nomatch ->
	    allok;
	_Else ->
	    erlstats:irc_notice((State#state.frickauser)#ircuser.uid,
				Params#irccmduid.uid,
				<< "Your \^breal name\^b contains a URL. This is discouraged on hackint and some hackint servers might reject your connection because of this. It is recommended you change your real name to one that does not contain a URL." >>)
    end,
    {noreply, State};

handle_cast({privmsg, fricka, I, whoami, User, []}, State) ->
    Msg = case User#ircuser.authenticated of
	      {_Authenticator, Authname} ->
		  [<<"They are known to \^bFricka\^b as ">>,
		   Authname, <<".">>];
	      _Else ->
		  <<"\^bFricka\^b does not recognize them.">>
	  end,
    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
			Msg),
    {noreply, State};

handle_cast({privmsg, fricka, I, info, User, []}, State) ->
    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
			io_lib:format("\^b*****\^b Here's some info about you \^b*****\^b~n~p~n\^b***** END of info *****\^b",
				      [User])),
    {noreply, State};

handle_cast({privmsg, fricka, I, wallop, User, Message}, State) ->
    {ok, Usertable} = gen_server:call(erlstats, getusertable),
    Message_F = [binary_to_list(Word) || Word <- Message],
    Message2 = io_lib:format("WALLOPS [~s] - ~s",
			     [User#ircuser.nick, string:join(Message_F, " ")]),
    C = ets:foldl(fun(Usr, Count) ->
			  case lists:member($w, Usr#ircuser.modes) of
			      true ->
				  erlstats:irc_notice(I#ircuser.sid, %% SID here
						      Usr#ircuser.uid,
						      Message2),
				  Count + 1;
			      false ->
				  Count
			  end
		  end, 0, Usertable),
    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
			io_lib:format("Message sent to ~p users.", [C])),
    {noreply, State};

handle_cast({privmsg, fricka, I, uinfo, User, [Usernick]}, State) ->
    case gen_server:call(erlstats, {getuserfromnick, Usernick}) of
	{ok, [User_R]} ->
	    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
				io_lib:format("\^b*****\^b Here's some info about ~s \^b*****\^b~n~p~n\^b***** END of info *****\^b",
					      [Usernick, User_R]));
	{ok, []} ->
	    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
				io_lib:format("User ~s does not seem to exist.", [Usernick]))
    end,
    {noreply, State};

handle_cast({privmsg, fricka, I, cinfo, User, [Channelname]}, State) ->
    {ok, Channeltable} = gen_server:call(erlstats, getchanneltable),
    case ets:lookup(Channeltable, Channelname) of
	[Channel] ->
	    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
				io_lib:format("\^b*****\^b Here's some info about ~s \^b*****\^b~n~p~n\^b***** END of info *****\^b",
					      [Channelname, Channel]));
	[] ->
	    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
				io_lib:format("Channel ~s does not seem to exist.", [Channelname]))
    end,
    {noreply, State};

handle_cast({privmsg, fricka, I, fautounban, User, Furtherstuff}, State) ->
    User_U = User#ircuser{
	       authenticated={<< "000" >>, << "OPER_OVERRIDE" >>}
	      },
    Parms = {privmsg, fricka, I, autounban, User_U, Furtherstuff},
    handle_cast_ap(Parms, State);

handle_cast({privmsg, fricka, I, autounban, User, [Channelname|_Maybesomething]}=Parms, State) ->
    case User#ircuser.authenticated of
	{_Authenticator, Nickservuser} ->
	    case erlstats:irc_get_chanacs(Nickservuser, Channelname) of
		{ok, nochannel} ->
		    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
					"Channel ~s is not registered with ChanServ.",
					[Channelname]),
		    {noreply, State};
		{ok, noaccess} ->
		    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
					"You do not seem to be on ~s's access list.",
					[Channelname]),
		    {noreply, State};
		{ok, Accessflags} ->
		    case binary:match(Accessflags, << "f" >>) of
			nomatch ->
			    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
						"You need the +f flag for ~s",
						[Channelname]),
			    {noreply, State};
			_Somematch ->
			    handle_cast_ap(Parms, State)
		    end;
		_Else ->
		    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
					"Permission denied for channel ~s",
					[Channelname]),
		    {noreply, State}
	    end;
	false ->
	    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
				"You are not identified to \^bNickServ\^b"),
	    {noreply, State}
    end;

handle_cast(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(checkbans, State) ->
    {ok, Channeltable} = gen_server:call(erlstats, getchanneltable),
    ets:foldl(fun(Channel, _Ignore) ->
		      %% ?DEBUG("Checkbans: ~p", [Channel]),
		      Trans = fun() ->
				      mnesia:read(mchansettings, {Channel#ircchannel.channame, autounban})
			      end,
		      case mnesia:transaction(Trans) of
			  {atomic, [Autounbaninfo]} ->
			      %% ?DEBUG("Checkbans: ~p ~p", [Channel, Autounbaninfo]),
			      Minage = esmisc:curtime() - orddict:fetch(secstounban, Autounbaninfo#mchansettings.value),
			      Unbans = lists:foldl(fun(E, Acc) ->
							   if E#ircban.time < Minage ->
								   [E#ircban.banmask|Acc];
							      true -> Acc
							   end
						   end, [], Channel#ircchannel.bans),
			      lists:foreach(fun(Banmask) ->
						    erlstats:irc_cmode((State#state.frickauser)#ircuser.uid, Channel#ircchannel.channame, [<< "-b" >>, Banmask])
					    end, Unbans);
			  _Else ->
			      ok
		      end
	      end, undefined, Channeltable),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

cmdlist(fricka) ->
    [
     whoami,
     info,
     wallop,
     uinfo,
     cinfo,
     autounban,
     fautounban
    ].

cmdhelp(fricka, autounban) ->
    [
     {params,      []},
     {shortdesc,   <<"Enable/Disable/Lookup AUTOUNBAN status on a channel">>},
     {longdesc,    <<"\^bFricka\^b can automatically unban users in a channel\n"
		     "after a certain amount of time if so requested by the channel\n"
		     "owner.\n \n"
		     "Syntax: AUTOUNBAN #channel\n"
		     "Syntax: AUTOUNBAN #channel off\n"
		     "Syntax: AUTOUNBAN #channel DURATION\n \n"
		     "Duration:\n"
		     "    100      100 Seconds\n"
		     "    5m       300 Seconds\n"
		     "    1h       3600 Seconds\n"
		     "    1d       86400 Seconds\n \n"
		     "Examples:\n"
		     "    /msg Fricka AUTOUNBAN #hackint 1d">>}
    ];

cmdhelp(fricka, fautounban) ->
    [
     {params,      []},
     {shortdesc,   <<"Enable/Disable/Lookup AUTOUNBAN status on a channel">>},
     {longdesc,    <<"\^bFricka\^b can automatically unban users in a channel\n"
		     "after a certain amount of time if so requested by the channel\n"
		     "owner.\n"
		     "Oper Force variant.\n \n"
		     "Syntax: FAUTOUNBAN #channel\n"
		     "Syntax: FAUTOUNBAN #channel off\n"
		     "Syntax: FAUTOUNBAN #channel DURATION\n \n"
		     "Duration:\n"
		     "    100      100 Seconds\n"
		     "    5m       300 Seconds\n"
		     "    1h       3600 Seconds\n"
		     "    1d       86400 Seconds\n \n"
		     "Examples:\n"
		     "    /msg Fricka FAUTOUNBAN #hackint 1d">>}
    ];

cmdhelp(fricka, whoami) ->
    [
     {params,      []},
     {shortdesc,   <<"Check if \^bFricka\^b recognises you">>},
     {longdesc,    <<"When you are identified to \^bNickServ\^b, \^bFricka\^b "
		     "should be aware of that fact. With this command you can "
		     "find out if she actually is.">>}
    ];

cmdhelp(fricka, info) ->
    [
     {params,     []},
     {shortdesc,  <<"Show all \^bFricka\^b knows about you">>},
     {longdesc,   <<"\^bFricka\^b, being implemented as a network service, knows\n"
		    "quite a bit about hackint's inhabitants. With this command you\n"
		    "can find out what information she has on you.">>}
    ];

cmdhelp(fricka, wallop) ->
    [
     {params,    []},
     {shortdesc, <<"Send a message to users with mode +w">>},
     {longdesc,  <<"Hybrid does not support USERWALLOPs out-of-the-box,\n"
		   "thus, this functionality is provided by \^bFricka\^b instead.\n \n"
		   "Syntax: WALLOP text\n \n"
		   "Examples:\n"
		   "    /msg Fricka WALLOP Hi all, try out our new channel mode +S">>}
    ];

cmdhelp(fricka, uinfo) ->
    [
     {params,    []},
     {shortdesc, <<"Show information \^bFricka\^b has about a user">>},
     {longdesc,  <<"Show information \^bFricka\^b has about a user\n \n"
		   "Syntax: UINFO usernick\n \n"
		   "Examples:\n"
		   "    /msg Fricka UINFO hc\n"
		   "    /msg Fricka UINFO fricka">>}
    ];

cmdhelp(fricka, cinfo) ->
    [
     {params,    []},
     {shortdesc, <<"Show information \^bFricka\^b has about a channel">>},
     {longdesc,  <<"Show information \^bFricka\^b has about a channel\n \n"
		   "Syntax: CINFO channelname\n \n"
		   "Examples:\n"
		   "    /msg Fricka CINFO #ircnets\n"
		   "    /msg Fricka CINFO #hackint">>}
    ].

cmdperm(fricka, autounban) ->
    []; %% No permission required

cmdperm(fricka, fautounban) ->
    $o; %% Operators only

cmdperm(fricka, whoami) ->
    []; %% No permission required

cmdperm(fricka, info) ->    
    []; %% No permission required

cmdperm(fricka, wallop) ->
    $o; %% Operators only

cmdperm(fricka, uinfo) ->
    $o; %% Operators only

cmdperm(fricka, cinfo) ->
    $o. %% Operators only

cmdgenericinfo(fricka) ->
    <<
      "\^bFricka\^b heiß' ich, und \^bFricka\^b bin ich!\n \n"
      "\^bFricka\^b is a goddess; on hackint she contributes to the\n"
      "network's stability.\n \n"
      "For example, she prevents people from acquiring halfop status\n"
      "in channels, as halfops are still supported by some hackint\n"
      "servers, but the network as a whole does not support them\n"
      "anymore."
    >>.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
find_nicks_to_unhalfop(Channel, Modechanges, Params) ->    
    find_nicks_to_unhalfop(Channel, Modechanges, Params, undecided).
find_nicks_to_unhalfop(Channel, << Modechange:8, R/binary >>, [Param|Rest]=Params, Addrem) ->    
    case Modechange of
        $+ ->	    
            find_nicks_to_unhalfop(Channel, R, Params, add);
	$- ->
            find_nicks_to_unhalfop(Channel, R, Params, remove);
        $h ->
            if Addrem == add -> [Param];
               true -> []
            end	++ find_nicks_to_unhalfop(Channel, R, Rest, Addrem);
        _Else ->
            NewParams = case lists:member(Modechange, "klvhobeI") of
			    true -> Rest;
			    false -> Params
			end,
            find_nicks_to_unhalfop(Channel, R, NewParams, Addrem)
    end;
find_nicks_to_unhalfop(_Channel, _Modechanges, _Shouldbeempty, _Addrem) ->
    [].


handle_cast_ap({privmsg, fricka, I, autounban, User, [Channelname]}, State) ->
    Trans = fun() ->
		    mnesia:read(mchansettings, {Channelname, autounban})
	    end,
    case mnesia:transaction(Trans) of
	{atomic, [Autounbaninfo]} ->
	    Numsecs = orddict:fetch(secstounban, Autounbaninfo#mchansettings.value),
	    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
				"Autounban is activated for ~s. Bans will be removed " %% No \n for command results
				"after ~p seconds.~n", [Channelname, Numsecs]);
	_Else ->
	    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
				"Autounban is not enabled for ~s.",
				[Channelname])
    end,
    {noreply, State};

handle_cast_ap({privmsg, fricka, I, autounban, User, [Channelname, <<"off">>]}, State) ->
    {_Auther, Nickservusername} = User#ircuser.authenticated,
    Trans = fun() ->
		    mnesia:delete({mchansettings, {Channelname, autounban}})
	    end,
    {atomic, ok} = mnesia:transaction(Trans),
    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
			"Autounban is now \^boff\^b for \^b~s\^b.",
			[Channelname]),
    erlstats:irc_notice(I#ircuser.sid, Channelname,
			"(\^bFricka\^b) ~s (NickServ ~s) DISABLED autounban for ~s",
			[User#ircuser.nick, Nickservusername,
			 Channelname]),
    {noreply, State};

handle_cast_ap({privmsg, fricka, I, autounban, User, [Channelname, Duration]}, State) ->
    {_Auther, Nickservusername} = User#ircuser.authenticated,
    case esmisc:parseduration(Duration) of
	{error, Reason} ->
	    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
				"Illegal duration: ~s", [Reason]);
	{ok, Duration_Secs} when Duration_Secs < 300 ->
	    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
				"Duration must be at least 300 seconds.");
	{ok, Duration_Secs} ->
	    Dict = [{channelname, Channelname},
		    {secstounban, Duration_Secs},
		    {setter, User#ircuser.authenticated}],
	    Record = #mchansettings{
	      chans={Channelname, autounban},
	      value=Dict
	     },
	    Trans = fun() ->
			    mnesia:write(Record)
		    end,
	    {atomic, ok} = mnesia:transaction(Trans),
	    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
				"Successfully set autounban for ~s to ~p seconds.",
				[Channelname, Duration_Secs]),
	    erlstats:irc_notice(I#ircuser.sid, Channelname,
				"(\^bFricka\^b) ~s (NickServ ~s) ENABLED autounban after ~p seconds for ~s",
				[User#ircuser.nick, Nickservusername,
				 Duration_Secs, Channelname])
    end,
    {noreply, State}.
