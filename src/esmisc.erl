%%%-------------------------------------------------------------------
%%% File    : esmisc.erl
%%% Author  : hc <hc@hc-laptop>
%%% Description : Miscellaneous functions
%%%
%%% Created :  5 Feb 2012 by hc <hc@hc-laptop>
%%%-------------------------------------------------------------------
-module(esmisc).

%% API
-export([
	 curtime/0,
	 parseumode/1,
	 parseumode/2,
	 openlog/0,
	 log/1,
	 log/2
	]).

%% Spawn functions
-export([
	 logger/1
	]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

curtime() ->
    {MS, S, _} = now(),
    MS * 1000000 + S.

parseumode(Umodes) ->
    parseumode(add, [], Umodes).

parseumode(Curmodes, Newmodes) ->
    parseumode(add, Curmodes, Newmodes).

parseumode(Operation, Curmodes, << Modechar:8, Newmodes/binary >>) ->
    case {Modechar, Operation} of
        {$+, _} ->
	    parseumode(add, Curmodes, Newmodes);
	{$-, _} ->
	    parseumode(remove, Curmodes, Newmodes);
        {Char, add} ->
            parseumode(Operation, [Char|Curmodes], Newmodes);
	{Char, remove} ->
	    parseumode(Operation, Curmodes -- [Char], Newmodes)
    end;
parseumode(_Operation, Curmodes, << >>) ->
    Curmodes.



%%====================================================================
%% Internal functions
%%====================================================================
openlog() ->
    %%Logpath = code:priv_dir(erlstats),
    Logpath = "priv",
    Logfile = filename:join(Logpath, "erlstats.log"),
    {ok, F} = file:open(Logfile, [write]),
    LP = spawn(?MODULE, logger, [F]),
    register(logger, LP).

log(S) ->
    logger ! {log, S}.

log(S, F) ->
    R = io_lib:format(S, F),
    logger ! {log, R}.

logger(F) ->
    receive
	{log, Stuff} ->
	    file:write(F, Stuff),
	    file:write(F, << "\n" >>),
	    logger(F);
	Else ->
	    error_logger:info_msg("Unknown log message ~p!", [Else])
    end.
