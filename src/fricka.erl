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
					    << "~hs" >>,
					    << "services." >>,
					    << "Fricka" >>,
					    << "Frech wacht Fricka ueber's HackINT" >>, ?MODULE}),
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

%handle_cast({privmsg, fricka, I, help, User, []}, State) ->

handle_cast(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
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
    ].

cmdhelp(fricka, foo) ->
    << >>.

cmdperm(fricka, foo) ->
    "".

cmdgenericinfo(fricka) ->
    <<
      "\^bFricka\^b heiß' ich, und \^bFricka\^b bin ich!\n \n"
      "\^bFricka\^b is a utility bot that can perform various services.\n"
      "For example, it prevents people from acquiring halfop status\n"
      "in channels, as halfops are still supported by some hackint\n"
      "servers, but the network as a whole does not support them\n"
      "anymore.\n \n"
      "No further information is available ATM, but more\n"
      "is to come."
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
