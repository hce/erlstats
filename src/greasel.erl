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

-record(state, {
	  greaseluser,    %% Our IRC "nick" greasel
	  blacklistdb,
	  checkblacklist
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
		       uid  % We need to keep track of all users connecting to the server
		      ],
    gen_server:call(erlstats, {register_plugin, Handledcommands}),
    Blacklistdb = ets:new(blacklistdb, [set, protected, {keypos, 2}]),
    timer:send_after(4200, start_to_check),
    {ok, #state{
       blacklistdb=Blacklistdb,
       checkblacklist=false
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
					   << "~aeoe" >>,
					   << "greasellab.oceanlab.research.hackint.org" >>,
					   << "HackINT's Security Greasel" >>,
					   << "IRC Security Greasel" >>}),
    {noreply, State#state{greaseluser=User}};

handle_cast({irccmd, uid, Params}, State) ->
    esmisc:log("New user - need to check host ~p", [Params#irccmduid.ip]),

    Newstate = case State#state.checkblacklist of
		   true ->
		       check_blacklist(State, Params);
		   false ->
		       esmisc:log("Not checking agains blacklist yet (IP ~p)",
				  [Params#irccmduid.ip]),
		       State
	       end,
	
    {noreply, Newstate};

handle_cast({privmsg, greasel, I, help, User, []}, State) ->
    erlstats:irc_notice(I#ircuser.uid, User#ircuser.uid,
                        <<
                          "***** ", 2, "Greasel Help", 2, " *****", 10,
                          "I'm HackINT's security ", 2, "greasel", 2, "!", 10, 32, 10,
                          "äö.", 10, 32, 10,
                          "I do not provide any direct user services. If you notice there", 10,
                          "is little spam in hackint, it is partly due to my vigilance.", 10,
                          "***** ", 2, "End of Help", 2, " *****"
                        >>),
        {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(start_to_check, State) ->
    error_logger:info_msg("The greasel is now sharp."),
    {noreply, State#state{checkblacklist=true}};
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
    esmisc:log("Resolving ~s", [Hosttocheck]),
    case inet:getaddr(Hosttocheck, inet) of
	{ok, {_, _, _, 4}} ->
	    false; %% TOR
	{ok, {_, _, _, Reasoncode}}->
	    {true, "EFNet RBL " ++ reason(efnetrbl, Reasoncode)};
	{error, _} ->
	    false %% Not listed
    end;
checkblacklist(check, IP) ->
    case lists:member(IP, [<<"127.0.0.1">>,
			   <<"0">>]) of
	true ->
	    false;
	_false ->
	    checkblacklist(lookup, IP)
    end;
checkblacklist(DB, IP) ->
    Curtime = esmisc:curtime(),
    case ets:lookup(DB, IP) of
	[Entry] when Entry#blacklistentry.expirytime > Curtime ->
	    esmisc:log("HIT for IP ~p", [IP]),
	    Entry#blacklistentry.result;
	_Else ->
	    Result = checkblacklist(check, IP),
	    Entry = #blacklistentry{
	      ip=IP,
	      expirytime=Curtime + 3600 * 24,
	      result=Result
	     },
	    ets:insert(DB, Entry),
	    esmisc:log("Added a new cache entry for IP ~p", [IP]),
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
	    erlstats:irc_kline(Params#irccmduid.ip, 5760,
			       << "To resolve your K-Line-issue, please send a mail "
				  "klines@hackint.org with details about your IP and so forth." >>);
	false ->
	    esmisc:log("New user ~s!~s@~s (~p) is not blacklistsed.", [Params#irccmduid.nick,
								       Params#irccmduid.ident,
								       Params#irccmduid.hostname,
								       Params#irccmduid.gecos])
    end,
    State.
