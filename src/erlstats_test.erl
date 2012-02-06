-module(erlstats_test).

-export([start/0]).

start() ->
    erlstats:start_link(["192.168.178.20", 6667, <<"stats.hackint.org">>, <<"00F">>, <<"barkbark!">>, <<"barkbark!">>, <<"HC's Erlang Modular Services Framework">>]).
