%%%-------------------------------------------------------------------
%%% File    : stats.erl
%%% Author  : hc <hc@hc-laptop>
%%% Description : munin stats generator
%%%
%%% Created :  6 Feb 2012 by hc <hc@hc-laptop>
%%%-------------------------------------------------------------------
-module(stats).

-include("erlstats.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {timer,
		listenprocesses,
		curdata}).

%% Hot code swapping / servers
-export([doaccept/3]).

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
handle_call(get_stats, _From, State) ->
    {reply, {ok, State#state.curdata}, State};

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
    Timer = timer:send_interval(120000, update_stats),
    LP = listenprocesses(self()),
    {noreply, State#state{
		timer=Timer,
		listenprocesses=LP,
		curdata=orddict:new()
      }};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(update_stats, State) ->
    {ok, Usertable} = gen_server:call(erlstats, getusertable),
    {ok, Servertable} = gen_server:call(erlstats, getservertable),
    Newdict = handle_stats(Usertable, Servertable, State#state.curdata),
    {noreply, State#state{curdata=Newdict}};

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
handle_stats(Usertable,
	     Servertable,
	     Orddict) ->
    {Stats, Totalusers} = ets:foldl(fun(User, {Dict, Total}) ->
					    SID = User#ircuser.sid,
					    UID = User#ircuser.uid,
					    NV = case dict:find(SID, Dict) of
						     {ok, Users} ->
							 [UID|Users];
						     error ->
							 [UID]
						 end,
					    {dict:store(SID, NV, Dict), Total + 1}
				    end, {dict:new(), 0}, Usertable),

    

    Totalusers_B = list_to_binary(integer_to_list(Totalusers)),

    {_, A_Config, A_Stats} =
	dict:fold(fun(SID, UIDs, {A, IOList_C, IOList_S}) ->
			  Length_B = list_to_binary(integer_to_list(length(UIDs))),
			  FON = if A == first -> << "AREA" >>;
				   true -> << "STACK" >> end,
			  [Server] = ets:lookup(Servertable, SID),
			  SID_n = Server#ircserver.hostname,
			  SID_h = hashit(SID_n),
			  F_config = << SID_h/binary, ".draw ",
					FON/binary, 10,
					SID_h/binary, ".label ", SID_n/binary, 10 >>,
			  F_stats = << SID_h/binary, ".value ",
				       Length_B/binary, 10 >>,
			  {notfirst, IOList_C ++ [F_config], IOList_S ++ [F_stats]}
		  end, {first, [], []}, Stats),

    Buf_data   = [A_Stats,
		  << "total.value ", Totalusers_B/binary, 10 >>],
    Buf_config = [<< "graph_category irc", 10,
		     "graph_args -l 0", 10,
		     "graph_title hackint.org User stats", 10,
		     "graph_vlabel users", 10 >>,
		  A_Config,
		  << "total.draw LINE2", 10,
		     "total.label Total users", 10 >>],
    
    OD1 = orddict:store(data, Buf_data, Orddict),
    orddict:store(config, Buf_config, OD1).

hashit(N) ->
    <<R:32, _/binary>> = crypto:sha(N),
    <<RB/binary>> = <<R:32>>,
    iolist_to_binary(hex:list_to_hex(binary_to_list(RB))).


listenprocesses(Handlingserver) ->    
    Options = [binary, {ip, {127, 0, 0, 1}},
	       {active, false},
	       {packet, raw},
	       inet, {reuseaddr, true}],
    {ok, LS}  = gen_tcp:listen(9182, Options),
    {ok, LS2} = gen_tcp:listen(9183, Options),
    Configserver = spawn_link(?MODULE, doaccept, [config, LS, Handlingserver]),
    Dataserver   = spawn_link(?MODULE, doaccept, [data, LS2, Handlingserver]),
    [{configserver, Configserver},
     {dataserver, Dataserver}].

doaccept(What, LS, Handlingserver) ->
    case gen_tcp:accept(LS) of
	{ok, CS} ->
	    spawn(fun() ->
			  {ok, Stuff} = gen_server:call(Handlingserver, get_stats),
			  case orddict:find(What, Stuff) of
			      {ok, Val} ->
				  gen_tcp:send(CS, Val);
			      _Else ->
				  gen_tcp:send(CS, << "Come back later" >>)
			  end,
			  gen_tcp:close(CS)
		  end);
	_Else ->
	    ok
    end,
    ?MODULE:doaccept(What, LS, Handlingserver).    
