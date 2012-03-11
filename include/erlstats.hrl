
%%%-------------------------------------------------------------------
%%% File    : erlstats.hrl
%%% Author  : hc <hc@hc-laptop>
%%% Description : Erlstats header file
%%%
%%% Created :  5 Feb 2012 by hc <hc@hc-laptop>
%%%-------------------------------------------------------------------

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
		  realhost,
		  authenticated,
		  serverdata}).

-record(ircchannel, {channame,
		     bans,
		     banexps,
		     invexps,
		     quiets,
		     chankey,
		     chanlimit,
		     jointhrottle,
		     forward,		     
		     modes,
		     users,
		     topic,		     
		     ts}).

-record(ircchanuser, {uid, privs}).

-record(ircchanmodeparam, {setter, ts, param}).

-record(ircserver, {sid,
		    hostname,
		    distance,
		    description,
		    uplink}).

-record(ircplugin, {pid,
		    users,
		    handledcommands}).
		  

-record(irccmduid,	{burst,
			 server,
			 nick,
			 hops,
			 ts,
			 modes,
			 ident,
			 hostname,
			 realhostname,
			 ip,
			 uid,
			 gecos}).

-record(irccmdtmode, {burst,
		      issuer,
		      ts,
		      channame,
		      modechanges}).

-record(ircban, {time,
		 user,
		 banmask}).
		 
-record(blacklistentry, {ip,
			 expirytime,
			 result}).

-record(mchansettings, {chans,
			value}).
