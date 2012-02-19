-module(genmnesiatables).

-include("erlstats.hrl").

-export([start/0]).

start() ->
    try mnesia:create_schema([node()]) of
	Something ->
	    io:format("~p~n", [Something])
    catch E:F ->
	    io:format("~p:~p~n", [E, F])
    end,
    application:start(mnesia),
    try mnesia:create_table(mchansettings,
			    [{access_mode, read_write},
			     {attributes, record_info(fields, mchansettings)},
			     {disc_copies, [node()]},
			     {record_name, mchansettings},
			     {type, set}]) of
	Something2 ->
	    io:format("~p~n", [Something2])
    catch E2:F2 ->
	    io:format("~p:~p~n", [E2, F2])
    end.
    
			 
