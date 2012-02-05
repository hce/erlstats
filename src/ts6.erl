%%%-------------------------------------------------------------------
%%% File    : ts6.erl
%%% Author  : hc <hc@hc-laptop>
%%% Description : TS6 Server-to-server functions
%%%
%%% Created :  5 Feb 2012 by hc <hc@hc-laptop>
%%%-------------------------------------------------------------------
-module(ts6).

%% API
-export([sts_login/5]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
sts_login(S, SID, Nodename, Password, Node_Description) ->
    Curtime = list_to_binary(integer_to_list(esmisc:curtime())),
    gen_tcp:send(S, <<
		      "PASS ", Password/binary, " TS 6 :", SID/binary, 10,
		      "CAPAB :QS EX IE KLN UNKLN ENCAP TB SERVICES HOPS EUID EOPMOD", 10,
		      "SERVER ", Nodename/binary, " 1 :", Node_Description/binary, 10,
		      "SVINFO 5 5 0 :", Curtime/binary, 10
		    >>).


%%====================================================================
%% Internal functions
%%====================================================================
