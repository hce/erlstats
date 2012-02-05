%%%-------------------------------------------------------------------
%%% File    : erlstats.hrl
%%% Author  : hc <hc@hc-laptop>
%%% Description : Erlstats header file
%%%
%%% Created :  5 Feb 2012 by hc <hc@hc-laptop>
%%%-------------------------------------------------------------------
-module(erlstats).


-record(ircuser, {uid,
		  sid,
		  nick,
		  hop,
		  ts,
		  modes,
		  ident,
		  host,
		  ip,
		  realname,
		  channels,
		  away,
		  authenticated}).

-record(ircchannel, {channame,
		     bans,
		     banexps,
		     invexps,
		     chankey,
		     modes,
		     users,
		     topic}).

-record(ircserver, {sid,
		    hostname,
		    distance,
		    description,
		    uplink}).

