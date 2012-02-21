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
	 parsecmode/4,
	 parsecmode/2,
	 atomorunknown/1,
	 parsejjusers/1,
	 removeuser/2,
	 removeusermodes/1,
	 addchanusers/2,
	 parseduration/1
	]).

%% Spawn functions
-export([
	 logger/2
	]).

%% For hot code swapping
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

parsecmode(Channel, TS, TSModus, Modes) ->
    Channel_U = case {(TS < Channel#ircchannel.ts), TSModus} of
		    {true, normal} ->
			esmisc:log("Channel TS of ~s was lowered.", [Channel#ircchannel.channame]),
			Channel#ircchannel{
			  modes=[],
			  bans=[],
			  banexps=[],
			  invexps=[],
			  chanlimit=undefined,
			  chankey=undefined,
			  users=removeusermodes(Channel#ircchannel.users),
			  ts=TS
			 };
		    {true, simple} ->
			Channel; %% We ignore this case
		    %% TODO: handle false cases!
		    {false, _Whatever} ->
			Channel %% TODO!
		end,
    parsecmode(Channel_U, Modes, TS).

parsecmode(Channel, Modes) ->
    [Modechars|Modeparams] = Modes, %binary:split(Modes, <<" ">>, [global]),
    parsecmode_i(add, Channel, Modechars, Modeparams, curtime()).

parsecmode(Channel, Modes, TS) ->
    [Modechars|Modeparams] = Modes, %binary:split(Modes, <<" ">>, [global]),
    parsecmode_i(add, Channel, Modechars, Modeparams, TS).
    
parsecmode_i(_Operation, Channel, << >>, _Shouldbeempty, _TS) ->
    Channel;

parsecmode_i(Operation, Channel, << Modechar:8, MRest/binary >>, Pall, TS) ->
    case {lists:member(Modechar, "ntpsmiS"), Operation, Modechar} of
	{_, _, $+} ->
	    parsecmode_i(add, Channel, MRest, Pall, TS);
	{_, _, $-} ->
	    parsecmode_i(remove, Channel, MRest, Pall, TS);
	{true, _, _} ->
	    Newmodes = case Operation of
			   add ->
			       lists:usort([Modechar|Channel#ircchannel.modes]);
			   remove ->
			       Channel#ircchannel.modes -- [Modechar]
		       end,
	    CU = Channel#ircchannel{modes=Newmodes},
	    parsecmode_i(Operation, CU, MRest, Pall, TS);
	{false, remove, $k} ->
	    CU = Channel#ircchannel{chankey=undefined},
	    [<<"*">>|PRest] = Pall,
	    parsecmode_i(Operation, CU, MRest, PRest, TS);
	{false, add, $k} ->
	    [Param|PRest] = Pall,
	    CU = Channel#ircchannel{chankey=Param},	    
	    parsecmode_i(Operation, CU, MRest, PRest, TS);
	{false, remove, $l} ->
	    CU = Channel#ircchannel{chanlimit=0},
	    parsecmode_i(Operation, CU, MRest, Pall, TS);
	{false, add, $l} ->
	    [Param|PRest] = Pall,
	    CU = Channel#ircchannel{chanlimit=list_to_integer(binary_to_list(Param))},
	    parsecmode_i(Operation, CU, MRest, PRest, TS);
	{false, Operation, $v} ->
	    [Param|PRest] = Pall,
	    Users_U = updateuser(Param, Operation, voice, Channel#ircchannel.users),
	    CU = Channel#ircchannel{users=Users_U},
	    parsecmode_i(Operation, CU, MRest, PRest, TS);
	{false, Operation, $h} ->
	    [Param|PRest] = Pall,
	    Users_U = updateuser(Param, Operation, halfop, Channel#ircchannel.users),
	    CU = Channel#ircchannel{users=Users_U},
	    parsecmode_i(Operation, CU, MRest, PRest, TS);
	{false, Operation, $o} ->
	    [Param|PRest] = Pall,
	    Users_U = updateuser(Param, Operation, op, Channel#ircchannel.users),
	    CU = Channel#ircchannel{users=Users_U},
	    parsecmode_i(Operation, CU, MRest, PRest, TS);
	{false, add, $b} ->
	    [Param|PRest] = Pall,
	    List = Channel#ircchannel.bans,
	    List_U = [#ircban{time=curtime(),
			      user=unknown,
			      banmask=Param}|List],
	    CU = Channel#ircchannel{bans=List_U},
	    parsecmode_i(Operation, CU, MRest, PRest, TS);
	{false, remove, $b} ->
	    [Param|PRest] = Pall,
	    List = Channel#ircchannel.bans,
	    List_U = [Elem || #ircban{banmask=BM}=Elem <- List,
			      BM =/= Param],
	    CU = Channel#ircchannel{bans=List_U},
	    parsecmode_i(Operation, CU, MRest, PRest, TS);
	{false, add, $e} ->
	    [Param|PRest] = Pall,
	    List = Channel#ircchannel.banexps,
	    List_U = [Param|List],
	    CU = Channel#ircchannel{banexps=List_U},
	    parsecmode_i(Operation, CU, MRest, PRest, TS);
	{false, remove, $e} ->
	    [Param|PRest] = Pall,
	    List = Channel#ircchannel.banexps,
	    List_U = List -- [Param],
	    CU = Channel#ircchannel{banexps=List_U},
	    parsecmode_i(Operation, CU, MRest, PRest, TS);
	{false, add, $I} ->
	    [Param|PRest] = Pall,
	    List = Channel#ircchannel.invexps,
	    List_U = [Param|List],
	    CU = Channel#ircchannel{invexps=List_U},
	    parsecmode_i(Operation, CU, MRest, PRest, TS);
	{false, remove, $I} ->
	    [Param|PRest] = Pall,
	    List = Channel#ircchannel.invexps,
	    List_U = List -- [Param],
	    CU = Channel#ircchannel{invexps=List_U},
	    parsecmode_i(Operation, CU, MRest, PRest, TS);
	Else ->
	    error_logger:info_msg("Error: unknown channel mode ~p", [Else]),
	    parsecmode_i(Operation, Channel, MRest, Pall, TS)
    end.


parsejjusers(UserUIDs) ->
    lists:foldl(fun(Fullname, A) ->
			[parsejjuser(Fullname, [])|A]
		end, [], binary:split(UserUIDs, <<" ">>, [global])).

parsejjuser(<< UT:8, Rest/binary >>=Fullname, Privs) ->
    case UT of
	$@ ->
	    parsejjuser(Rest, [op|Privs]);
	$% ->
	    parsejjuser(Rest, [halfop|Privs]);
	$+ ->
	    parsejjuser(Rest, [voice|Privs]);
	_Else ->
	    #ircchanuser{uid=Fullname, privs=Privs}
    end.

updateuser(UID, Operation, Privilege, Users) ->
    User = case dict:find(UID, Users) of
	       {ok, Theuser} ->
		   Theuser;
	       error ->
		   #ircchanuser{uid=UID, privs=[]}
	   end,
    Privilege_s = if
		      Operation == add ->
			  lists:usort([Privilege|User#ircchanuser.privs]);
		      true ->
			  User#ircchanuser.privs -- [Privilege]
		  end,
    User_U = User#ircchanuser{privs=Privilege_s},
    dict:store(UID, User_U, Users).

removeuser(UID, Users) ->
    dict:erase(UID, Users).

removeusermodes(Users) ->
    dict:fold(fun(K, V, ND) ->
		      dict:store(K, V#ircchanuser{privs=[]}, ND)
	      end, dict:new(), Users).

addchanusers(#ircchannel{users=Users}=Channel, Newusers) ->
    Users_U = lists:foldl(fun(#ircchanuser{uid=UID}=Newuser, Usrdict) ->
				  dict:store(UID, Newuser, Usrdict)
			  end, Users, Newusers),
    Channel#ircchannel{users=Users_U}.

parseduration(Duration) when is_binary(Duration) ->
    parseduration(binary_to_list(Duration));
parseduration([]) ->
    {error, "Illegal value"};
parseduration(Duration) when is_list(Duration) ->
    Duration_C = lists:sublist(Duration, 1, length(Duration) - 1),
    {Number_S, Factor} = case lists:last(Duration) of
			     $s -> {Duration_C, 1};
			     $S -> {Duration_C, 1};
			     $m -> {Duration_C, 60};
			     $M -> {Duration_C, 60};
			     $h -> {Duration_C, 3600};
			     $H -> {Duration_C, 3600};
			     $d -> {Duration_C, 86400};
			     $D -> {Duration_C, 86400};
			     _Else -> {Duration, 1}
			 end,
    case string:to_integer(Number_S) of
	{error, _Reason}=Error ->
	    Error;
	{Number, []} when Number > 0->
	    {ok, Number * Factor};
	{_Number, []} ->
	    {error, "A positive value is required"};
	{_Number, _Rest} ->
	    {error, "Only integers are allowed."}
    end.
		 
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
    %% {ok, F} = file:open(Logfile, [write]),
    {ok, F} = disk_log:open([{name, erlstats},
			     {file, Logfile},
			     {linkto, self()},
			     %% {repair, true},
			     {type, wrap},
			     {format, external},
			     {size, {10240, 3}},  %% three 10kbyte files
			     {distributed, []},
			     {notify, false},
			     {head, [<<"HC's erlstats. Logfile opened at ">>, integer_to_list(curtime()), <<"\n">>]},
			     {mode, read_write}]),			     
    logger(F).

logger(F) ->
    receive
	{log, Stuff} ->
	    ok = disk_log:blog(F, [integer_to_list(curtime()), <<" ">>, Stuff, 10]),
	    ?MODULE:logger(F);
	Else ->
	    error_logger:info_msg("Unknown log message ~p!", [Else])
    end.

atomorunknown(S) ->
    C = string:to_lower(binary_to_list(iolist_to_binary(S))),
    try list_to_existing_atom(C) of
	Atom when is_atom(Atom) ->
	    Atom
    catch _:_ ->
	    unknown
    end.
