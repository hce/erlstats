%%%-------------------------------------------------------------------
%%% File    : erlstats_supervisor.erl
%%% Author  : hc <hc@hc-laptop>
%%% Description : 
%%%
%%% Created :  6 Feb 2012 by hc <hc@hc-laptop>
%%%-------------------------------------------------------------------
-module(erlstats_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(Startargs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Startargs).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    AChild = {'Erlstats',{'erlstats',start_link,[["192.168.178.20", 6667, <<"stats.hackint.org">>, <<"00F">>, <<"barkbark!">>, <<"barkbark!">>, <<"HC's Erlang Modular Services Framework">>]]},
	      permanent,2000,worker,['erlstats']},
    {ok,{{one_for_all,0,1}, [AChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
