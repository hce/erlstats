%%%-------------------------------------------------------------------
%%% @author Hans-Christian Esperer <hc-git@hcesperer.org>
%%% @copyright (C) 2012, Hans-Christian Esperer
%%% @doc Supervise all of erlstats' plugins
%%%
%%% @end
%%% Created :  7 Feb 2012 by Hans-Christian Esperer <hc-git@hcesperer.org>
%%%-------------------------------------------------------------------
-module(erlstats_plugins_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 900,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Greasel = {'Greasel',
	       {'greasel',
		start_link,[
			   ]
	       },
	      permanent,2000,worker,['greasel']},
    Fricka = {'Fricka',
	      {'fricka',
	       start_link, [
			   ]
	      },
	      permanent,2000,worker,['fricka']},
    Stats = {'Stats',
	     {'stats',
	      start_link, [
			  ]
	      },
	     permanent,2000,worker,['stats']},
    

    Plugins = [
	       Greasel,
	       Fricka,
	       Stats
	      ],

    {ok, {SupFlags, Plugins}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
