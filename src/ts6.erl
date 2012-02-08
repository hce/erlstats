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
-export([
	 sts_login/5,
	 sts_pong/2,
	 sts_newuser/2,
	 sts_kill/5,
	 sts_kline/5,
	 sts_cmode/5,
	 sts_notice/4,
	 sts_quituser/3,
	 sts_whoisuser/7,
	 sts_whoisserver/6,
	 sts_whoisopinfo/5,
	 sts_whoisaddconninfo/6,
	 sts_whoisfinished/4,
	 sts_whoisnotfound/5
	]).

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

sts_quituser(S, Ircuser, Quitreason) ->
    gen_tcp:send(S, [<< ":" >>, Ircuser#ircuser.uid,
		     << " QUIT :" >>, Quitreason, 10]).

sts_kill(S, Killer, Killername, Killee, Reason) ->
    gen_tcp:send(S,
		 <<
		   ":", Killer/binary, " KILL ",
		   Killee/binary, " :", Killername/binary,
		   " (", Reason/binary, ")", 10
		 >>).

sts_kline(S, Kliner, Host, Expiry, Reason) ->
    Expiry_B = integer_to_list(Expiry),
    gen_tcp:send(S,
		 [
		  ":", Kliner, " KLINE * ",
		  Expiry_B, " * ",
		  Host,
		  " :", Reason, 10
		 ]),
    gen_tcp:send(S,
		 [
		  ":", Kliner, " ENCAP * KLINE ",
		  Expiry_B, " * ",
		  Host,
		  " :", Reason, 10
		 ]).

sts_cmode(S, Modesetter, Channel, TS, Modes) ->
    TS_B = list_to_binary(integer_to_list(TS)),
    Modes_F = lists:foldl(fun(E, A) ->
				  if A == [] ->
					  E;
				     true ->
					  << A/binary, " ", E/binary >>
				  end
			  end, << >>, Modes),
    gen_tcp:send(S,
		 <<
		   ":", Modesetter/binary, " TMODE ",
		   TS_B/binary, " ", Channel/binary, " ",
		   Modes_F/binary, 10
		 >>).

sts_notice(S, Noticer, Noticee, Notice) ->
    Notice_lines = binary:split(iolist_to_binary(Notice), << 10 >>, [global]),
    lists:foreach(fun(Line) ->
			  sts_notice(brokenup, S, Noticer, Noticee, Line)
		  end, Notice_lines).

sts_notice(brokenup, S, Noticer, Noticee, Notice) ->
    gen_tcp:send(S,
		 [":", Noticer, " NOTICE ",
		  Noticee, " :", Notice, 10
		 ]).

sts_whoisuser(S, Nodename, Askernick, Nick, User, Host, Gecos) ->
    gen_tcp:send(S,
		 io_lib:format(":~s 311 ~s ~s ~s ~s * :~s~n",
			       [Nodename, Askernick, Nick, User, Host, Gecos])).

sts_whoisserver(S, Nodename, Askernick, Nick, Server, Serverinfo) ->
    gen_tcp:send(S,
		 io_lib:format(":~s 312 ~s ~s ~s :~s~n",
			       [Nodename, Askernick, Nick, Server, Serverinfo])).

sts_whoisopinfo(S, Nodename, Askernick, Nick, Opinfo) ->
    gen_tcp:send(S,
		 io_lib:format(":~s 313 ~s ~s :~s~n",
			       [Nodename, Askernick, Nick, Opinfo])).

sts_whoisaddconninfo(S, Nodename, Askernick, Nick, IP, Comment) ->
    gen_tcp:send(S,
		 io_lib:format(":~s 338 ~s ~s ~s :~s~n",
			       [Nodename, Askernick, Nick, IP, Comment])).
    
sts_whoisfinished(S, Nodename, Askernick, Nick) ->
    gen_tcp:send(S,
		 io_lib:format(":~s 318 ~s ~s :End of /WHOIS list.~n",
			       [Nodename, Askernick, Nick])).
    
sts_whoisnotfound(S, Nodename, Askernick, Nick, Reason) ->
    gen_tcp:send(S,
		 io_lib:format(":~s 401 ~s ~s :~s~n",
			       [Nodename, Askernick, Nick, Reason])).
    
		   

%%====================================================================
%% Internal functions
%%====================================================================
