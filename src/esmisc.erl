%%%-------------------------------------------------------------------
%%% File    : esmisc.erl
%%% Author  : hc <hc@hc-laptop>
%%% Description : Miscellaneous functions
%%%
%%% Created :  5 Feb 2012 by hc <hc@hc-laptop>
%%%-------------------------------------------------------------------
-module(esmisc).

-include("erlstats.hrl").

%% API
-export([
	 curtime/0,
	 parseumode/1,
	 parseumode/2,
	 openlog/0,
	 log/1,
	 log/2,
	 parsecmode/2
	]).

%% Spawn functions
-export([
	 logger/2
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

parsecmode(Channel, Modes) ->
    [Modechars|Modeparams] = Modes, %binary:split(Modes, <<" ">>, [global]),
    parsecmode(add, Channel, Modechars, Modeparams).

parsecmode(_Operation, Channel, << >>, []) ->
    Channel;

parsecmode(Operation, Channel, << Modechar:8, MRest/binary >>, Pall) ->
    case {lists:member(Modechar, "ntpsmi"), Operation, Modechar} of
	{_, _, $+} ->
	    parsecmode(add, Channel, MRest, Pall);
	{_, _, $-} ->
	    parsecmode(remove, Channel, MRest, Pall);
	{true, _, _} ->
	    Newmodes = case Operation of
			   add ->
			       [Modechar|Channel#ircchannel.modes];
			   remove ->
			       Channel#ircchannel.modes -- [Modechar]
		       end,
	    CU = Channel#ircchannel{modes=Newmodes},
	    parsecmode(Operation, CU, MRest, Pall);
	{false, remove, $k} ->
	    CU = Channel#ircchannel{chankey=undefined},
	    parsecmode(Operation, CU, MRest, Pall);
	{false, add, $k} ->
	    [Param|PRest] = Pall,
	    CU = Channel#ircchannel{chankey=Param},	    
	    parsecmode(Operation, CU, MRest, PRest);
	{false, remove, $l} ->
	    CU = Channel#ircchannel{chanlimit=0},
	    parsecmode(Operation, CU, MRest, Pall);
	{false, add, $l} ->
	    [Param|PRest] = Pall,
	    CU = Channel#ircchannel{chanlimit=list_to_integer(binary_to_list(Param))},
	    parsecmode(Operation, CU, MRest, PRest);
	{false, Operation, $v} ->
	    [Param|PRest] = Pall,
	    Users_U = updateuser(Param, Operation, voice, Channel#ircchannel.users),
	    CU = Channel#ircchannel{users=Users_U},
	    parsecmode(Operation, CU, MRest, PRest);
	{false, Operation, $h} ->
	    [Param|PRest] = Pall,
	    Users_U = updateuser(Param, Operation, halfop, Channel#ircchannel.users),
	    CU = Channel#ircchannel{users=Users_U},
	    parsecmode(Operation, CU, MRest, PRest);
	{false, Operation, $o} ->
	    [Param|PRest] = Pall,
	    Users_U = updateuser(Param, Operation, op, Channel#ircchannel.users),
	    CU = Channel#ircchannel{users=Users_U},
	    parsecmode(Operation, CU, MRest, PRest);
	{false, add, $b} ->
	    [Param|PRest] = Pall,
	    List = Channel#ircchannel.bans,
	    List_U = [Param|List],
	    CU = Channel#ircchannel{bans=List_U},
	    parsecmode(Operation, CU, MRest, PRest);
	{false, remove, $b} ->
	    [Param|PRest] = Pall,
	    List = Channel#ircchannel.bans,
	    List_U = List -- [Param],
	    CU = Channel#ircchannel{bans=List_U},
	    parsecmode(Operation, CU, MRest, PRest);
	{false, add, $e} ->
	    [Param|PRest] = Pall,
	    List = Channel#ircchannel.banexps,
	    List_U = [Param|List],
	    CU = Channel#ircchannel{banexps=List_U},
	    parsecmode(Operation, CU, MRest, PRest);
	{false, remove, $e} ->
	    [Param|PRest] = Pall,
	    List = Channel#ircchannel.banexps,
	    List_U = List -- [Param],
	    CU = Channel#ircchannel{banexps=List_U},
	    parsecmode(Operation, CU, MRest, PRest);
	{false, add, $I} ->
	    [Param|PRest] = Pall,
	    List = Channel#ircchannel.invexps,
	    List_U = [Param|List],
	    CU = Channel#ircchannel{invexps=List_U},
	    parsecmode(Operation, CU, MRest, PRest);
	{false, remove, $I} ->
	    [Param|PRest] = Pall,
	    List = Channel#ircchannel.invexps,
	    List_U = List -- [Param],
	    CU = Channel#ircchannel{invexps=List_U},
	    parsecmode(Operation, CU, MRest, PRest)
    end.

updateuser(UID, Operation, Privilege, Users) ->
    Users_R = [E || {UID_E,_Privs}=E <- Users,
		    UID_E =/= UID],
    Privilege_s = if Operation == add -> Privilege;
		     true -> undefined end,
    [#ircchanuser{uid=UID, privs=Privilege_s}|Users_R].
		 
%%====================================================================
%% Internal functions
%%====================================================================
openlog() ->
    %%Logpath = code:priv_dir(erlstats),
    Logpath = "priv",
    Logfile = filename:join(Logpath, "erlstats.log"),
    LP = spawn(?MODULE, logger, [init, Logfile]),
    register(logger, LP).

log(S) ->
    logger ! {log, S}.

log(S, F) ->
    R = io_lib:format(S, F),
    logger ! {log, R}.

logger(init, Logfile) ->
    {ok, F} = file:open(Logfile, [write]),
    logger(F).

logger(F) ->
    receive
	{log, Stuff} ->
	    ok = file:write(F, Stuff),
	    ok = file:write(F, << "\n" >>),
	    logger(F);
	Else ->
	    error_logger:info_msg("Unknown log message ~p!", [Else])
    end.
