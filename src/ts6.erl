%%%-------------------------------------------------------------------
%%% File    : ts6.erl
%%% Author  : hc <hc@hc-laptop>
%%% Description : TS6 Server-to-server functions
%%%
%%% Created :  5 Feb 2012 by hc <hc@hc-laptop>
%%%-------------------------------------------------------------------
-module(ts6).

-include("erlstats.hrl").

%% API
-export([sts_login/5,
	 sts_pong/2,
	 sts_newuser/2]).

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

sts_pong(S, Pongparam) ->
    gen_tcp:send(S, <<
		      "PONG :", Pongparam/binary, 10
		    >>).    

sts_newuser(S, Ircuser) ->
    Hops_B     = list_to_binary(integer_to_list(Ircuser#ircuser.hop)),
    TS_B       = list_to_binary(integer_to_list(Ircuser#ircuser.ts)),
    Ircmodes_B = list_to_binary(Ircuser#ircuser.modes),
    gen_tcp:send(S, <<
		      ":", (Ircuser#ircuser.sid)/binary,
		      " UID ",
		      (Ircuser#ircuser.nick)/binary, " ",
		      Hops_B/binary, " ",
		      TS_B/binary, " ",
		      "+", Ircmodes_B/binary, " ",
		      (Ircuser#ircuser.ident)/binary, " ",
		      (Ircuser#ircuser.host)/binary, " ",
		      (Ircuser#ircuser.ip)/binary, " ",
		      (Ircuser#ircuser.uid)/binary, " ",
		      ":", (Ircuser#ircuser.realname)/binary, 10
		    >>).


%%====================================================================
%% Internal functions
%%====================================================================
