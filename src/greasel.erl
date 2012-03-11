%%%-------------------------------------------------------------------
%%% File    : greasel.erl
%%% Author  : hc <hc@hc-laptop>
%%% Description : Hackint's security greasel
%%%
%%% Created :  6 Feb 2012 by hc <hc@hc-laptop>
%%%-------------------------------------------------------------------
-module(greasel).

-behaviour(gen_server).

-include("erlstats.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Plugin command information
-export([cmdlist/1, cmdhelp/2,
	 cmdperm/2, cmdgenericinfo/1]).


-record(state, {
	  greaseluser,    %% Our IRC "nick" greasel
	  blacklistdb,
	  torlistdb
	 }).

-define(DEBUG, esmisc:log).

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
		       uid  % We need to keep track of all users connecting to the server
		      ],
    gen_server:call(erlstats, {register_plugin, Handledcommands}),
    Blacklistdb = ets:new(blacklistdb, [set, protected, {keypos, 2}]),
    Torlistdb   = ets:new(blacklistdb, [set, protected, {keypos, 2}]),
    timer:send_interval(3600 * 1000, purgeoldentries),  %% Swipe through cache once per hour
    {ok, #state{
       blacklistdb=Blacklistdb,
       torlistdb=Torlistdb
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
					   << "GREASL" >>,
					   << "greasel" >>, 
					   << "aeoe" >>,
					   << "services.hackint.org" >>,
					   << "HackINT's Security Greasel" >>,
					   << "IRC Security Greasel" >>, ?MODULE}),
    {noreply, State#state{greaseluser=User}};

handle_cast({irccmd, uid, #irccmduid{burst=true}=_Params}, State) ->
    %% Do not check users against BL during burst
    {noreply, State};
handle_cast({irccmd, uid, #irccmduid{burst=false}=Params}, State) ->
    Newstate = check_blacklist(State, Params),
    {noreply, Newstate};

handle_cast({privmsg, greasel, I, checkhost, User, [Hosttocheck]}, State) ->
    Result = case inet:getaddr(binary_to_list(Hosttocheck), inet) of
		 {ok, {A, B, C, D}} ->
		     checkblacklist(State#state.blacklistdb,
				    iolist_to_binary(io_lib:format("~p.~p.~p.~p", [A, B, C, D])));
		 _Else ->
		     cantresolve
	     end,
    case Result of
	{true, Reason} ->
	    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
				io_lib:format("~s is listed for the following reason: ~s.",
					      [Hosttocheck, Reason]));
	false ->
	    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
				io_lib:format("~s is not listed.", [Hosttocheck]));
	cantresolve ->
	    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
				io_lib:format("~s can not be resolved.", [Hosttocheck]))
    end,

    {noreply, State};

handle_cast({privmsg, greasel, I, iscached, User, [IPtocheck]}, State) ->
    Lookupresult = case ets:lookup(State#state.blacklistdb, IPtocheck) of
		       [#blacklistentry{result={true,Reason}}] ->
			   [<< "The specified IP is listed with the reason: " >>, Reason];
		       _Else ->
			   << "The specified IP is not listed." >>
		   end,
    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
			Lookupresult),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(purgeoldentries, State) ->
    Count1 = purgeoldentries(State#state.blacklistdb),
    ?DEBUG("Purged ~p entries from the blacklist DB.", [Count1]),
    Count2 = purgeoldentries(State#state.torlistdb),
    ?DEBUG("Purged ~p entries from the tor DB.", [Count2]),
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




%% A list of plugin commands accessible through IRC
cmdlist(greasel) ->
    [
     checkhost,
     iscached
    ].

%% The help for each command accessible through IRC
cmdhelp(greasel, checkhost) ->
    [
     {params,    [<<"HOSTNAME">>]},
     {shortdesc, <<"Check if \^bHOSTNAME\^b is blacklisted">>},
     {longdesc,  <<"The \^bCHECKHOST\^b command checks if \^bHOSTNAME\^b\n"
		   "is blacklisted. The way this check is performed is the same\n"
		   "as when a user connects to hackint.">>}
    ];
cmdhelp(greasel, iscached) ->
    [
     {params,    [<<"IPv4">>]},
     {shortdesc, [<<"Look up \^bIPv4\^b in the local DNSBL cache">>]},
     {longdesc,  [<<"The \^bISCACHED\^b command checks if \^bIPv4\^b\n"
		    "is in the local DNSBL cache. If it is in the cache \^band\^b\n"
		    "if it is marked as listed, this is indicated. In all other\n"
		    "case this command reports the entry as not cached.\n \n"
		    "You must specify a valid IPv4 address; hostnames won't work.">>]}
    ].


%% The permissions required to use a certain command
cmdperm(greasel, checkhost) ->
    $o; %% checkhost needs operator permissions
cmdperm(greasel, iscached) ->
    []. %% No special permissions required



%% Generic information to be displayed when HELP
%% is called without parameters
cmdgenericinfo(greasel) ->
    <<
      "I'm HackINT's security \^bgreasel\^b!\n \n"
     "äö.\n \n"
     "I do not provide any direct user services. If you notice there\n"
     "is little spam in hackint, it is partly due to my vigilance."
    >>.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% THE NEXT LINE ON DEBUGGING SERVERS ONLY!!1!
%%%checkblacklist(_, _) -> {true, "Well, Test"};

checkblacklist(lookup, IP) ->
    IP_2 = case binary:split(IP, <<".">>, [global]) of
	       [A, B, C, D] ->
		   [D, ".", C, ".", B, ".", A];
	       Else ->
		   Else
	   end,
    Hosttocheck = binary_to_list(iolist_to_binary(IP_2)) ++ ".rbl.efnetrbl.org.",
    case inet:getaddr(Hosttocheck, inet) of
	{ok, {_, _, _, 4}} ->
	    false; %% TOR
	{ok, {_, _, _, Reasoncode}}->
	    {true, "EFNet RBL " ++ reason(efnetrbl, Reasoncode)};
	{error, _} ->
	    false %% Not listed
    end;
checkblacklist(check, IP) ->
    case is_special(IP) of
	true ->
	    false;
	false ->
	    checkblacklist(lookup, IP)
    end;

checkblacklist(DB, IP) ->
    Curtime = esmisc:curtime(),
    case ets:lookup(DB, IP) of
	[Entry] when Entry#blacklistentry.expirytime > Curtime ->
	    Entry#blacklistentry.result;
	_Else ->
	    Result = checkblacklist(check, IP),
	    Entry = #blacklistentry{
	      ip=IP,
	      expirytime=Curtime + 3600 * 24,
	      result=Result
	     },
	    ets:insert(DB, Entry),
	    Result
    end.

checktorlist(lookup, IP) ->
    IP_2 = case binary:split(IP, <<".">>, [global]) of
	       [A, B, C, D] ->
		   [D, ".", C, ".", B, ".", A];
	       Else ->
		   Else
	   end,
    Hosttocheck = binary_to_list(iolist_to_binary(IP_2)) ++ ".tor.dnsbl.sectoor.de.",
    case inet:getaddr(Hosttocheck, inet) of
	{ok, {_, _, _, 1}} ->
	    true;  %% Tor
	{ok, _Else}->
	    false; %% Unknown listing type
	{error, _} ->
	    false %% Not listed
    end;
checktorlist(check, IP) ->
    case is_special(IP) of
	true ->
	    false;
	false ->
	    checktorlist(lookup, IP)
    end;
checktorlist(DB, IP) ->
    Curtime = esmisc:curtime(),
    case ets:lookup(DB, IP) of
	[Entry] when Entry#blacklistentry.expirytime > Curtime ->
	    Entry#blacklistentry.result;
	_Else ->
	    Result = checktorlist(check, IP),
	    Entry = #blacklistentry{
	      ip=IP,
	      expirytime=Curtime + 3600 * 24 * 7,  %% 7 Days
	      result=Result
	     },
	    ets:insert(DB, Entry),
	    Result
    end.


reason(efnetrbl, 1) -> "Open Proxy";
reason(efnetrbl, 2) -> "spamtrap666";
reason(efnetrbl, 3) -> "spamtrap50";
reason(efnetrbl, 5) -> "drones / flooding";
reason(_, _) -> "Unknown reason".

check_blacklist(State, Params) ->
    case checkblacklist(State#state.blacklistdb,
			Params#irccmduid.ip) of
	{true, Reason} ->
	    esmisc:log("Need to KLINE/KILL user ~s!~s@~s (~p).", [Params#irccmduid.nick,
								  Params#irccmduid.ident,
								  Params#irccmduid.hostname,
								  Params#irccmduid.gecos]),
	    Reason_B = list_to_binary(Reason),
	    erlstats:irc_kill((State#state.greaseluser)#ircuser.uid,
			      Params#irccmduid.uid,
			      << "You are blacklisted: ",
			     Reason_B/binary >>),
	    erlstats:irc_kline((State#state.greaseluser)#ircuser.uid,
			       Params#irccmduid.ip, 5760,
			       << "To resolve your K-Line-issue, please send a mail "
				  "klines@hackint.org with details about your IP and so forth." >>);
	false ->
	    ok
    end,
    case checktorlist(State#state.torlistdb,
		      Params#irccmduid.ip) of
	true ->
	    erlstats:irc_notice((State#state.greaseluser)#ircuser.uid,
				Params#irccmduid.uid,
				[<< "You are welcome to connect via tor, but it is recommended you use our Tor hidden service. Please read \^bhttp://blog.hackint.eu/blog/display?id=19\^b" >>]),
	    erlstats:irc_chghost((State#state.greaseluser)#ircuser.uid,
				 Params#irccmduid.uid,
				 io_lib:format("tor/direct/~s", [esmisc:uidtodomain(Params#irccmduid.uid)])),
	    esmisc:log("New user ~s!~s@~s *is* connecting via tor; "
		       "sending them a warning message to use our "
		       "Tor hidden service.", [Params#irccmduid.nick,
					       Params#irccmduid.ident,
					       Params#irccmduid.hostname]);
	false ->
	    ok
    end,

    State.

purgeoldentries(ETSTable) ->
    Curtime = esmisc:curtime(),
    ets:foldl(fun(#blacklistentry{ip=IP,
				  expirytime=ET},
		  Count) ->
		      if
			  ET < Curtime ->
			      ets:delete(ETSTable, IP),
			      Count + 1;
			  true ->
			      Count
		      end
	      end, 0, ETSTable).
		      
is_special(IP) ->
    lists:member(IP, [<<"127.0.0.1">>,
		      <<"0">>]).
