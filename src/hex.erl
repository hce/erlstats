-module(hex).

-export([list_to_hex/1]).

% Taken from http://sacharya.com/md5-in-erlang/

list_to_hex(L) ->
       lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
       [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
       $0+N;
hex(N) when N >= 10, N < 16 ->
       $a + (N-10).
