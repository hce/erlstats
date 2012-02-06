%%%-------------------------------------------------------------------
%%% File    : erlstats.erl
%%% Author  : Hans-Christian Esperer <hc@hcesperer.org>
%%% Description : erlstats main server
%%%
%%% Created :  5 Feb 2012 by hc <hc@hc-laptop>
%%%-------------------------------------------------------------------
-module(erlstats).

-behaviour(gen_server).

-include("erlstats.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  socket,
	  usertable,
	  servertable,
	  channeltable,
	  me,
	  connectiondetails,
	  remotepassword,
	  uplinkcapabilities,
	  uplinkuid
	 }).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, Port, Nodename, SID, Password, Remotepassword, Node_Description]=Connectiondetails) ->
    {ok, S} = gen_tcp:connect(Host, Port,
			      [binary,
			       {active, true},
			       {packet, line}]),

    ts6:sts_login(S, SID, Nodename, Password, Node_Description),

    Usertable   = ets:new(usertable,   [set, protected, {keypos, 2}]),
    Chantable   = ets:new(chantable,   [set, protected, {keypos, 2}]),
    Servertable = ets:new(servertable, [set, protected, {keypos, 2}]),
    
    ME = #ircserver{
      sid=SID,
      hostname=Nodename,
      distance=0,
      description=Node_Description,
      uplink=undefined},
    ets:insert(Servertable, ME),
    
    {ok, #state{
       socket=S,
       usertable=Usertable,
       servertable=Servertable,
       channeltable=Chantable,
       me=ME,
       connectiondetails=Connectiondetails,
       remotepassword=Remotepassword
      }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp, _Socket, Data}, State) ->
    Len = size(Data) - 2,  % Substract the length of \r\n
    << Data_wr:Len/binary, _CRLF/binary >> = Data,

    error_logger:info_msg("Parsing ~p", [Data_wr]),
    Newstate = case parseline(Data_wr) of
		   [Instigator, Command|Params] ->
		       Command_lower = string:to_lower(binary_to_list(Command)),
		       Command_atom  = try list_to_existing_atom(Command_lower) of
					   Someatom when is_atom(Someatom) ->
					       Someatom
				       catch _:_ ->
					       unknown
				       end,
						%io:format("Command ~p[~p] Inst ~p Params ~p~n", [Command_atom, Command_lower, Instigator, Params]),
		       irccmd(Command_atom, State, Instigator, Params);
		   Else ->
		       error_logger:error_msg("Unable to handle received line ~p", [Else]),
		       State
    end,
    
    {noreply, Newstate};


handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
parseline(<< Firstbyte:8, Rest/binary >>=Line)  ->
    if
	Firstbyte == $: ->
	    parseline(rest, Rest);
	true ->
	    [[]|parseline(rest, Line)]
    end.

parseline(rest, Line) ->
    [NormalParams|Textparam] = binary:split(Line, << " :" >>),
    NormalParamList = binary:split(NormalParams, << " " >>, [global]),
    NormalParamList ++ Textparam.

irccmd(notice, State, Instigator, [<<"AUTH">>, Authprocess]) ->
    error_logger:info_msg("Authentication Handshake with ~p: ~p", [Instigator, Authprocess]),
    State;

irccmd(notice, State, Instigator, Params) ->
    io:format("Got ~p from ~p", [Params, Instigator]),
    State;

irccmd(ping, State, [], [Pongparam]) ->
    error_logger:info_msg("Sending PONG ~p", [Pongparam]),
    ts6:sts_pong(State#state.socket, Pongparam),
    State;

irccmd(uid, State, SID, [Nick, Hops, TS,
			 Usermode, Ident, Hostname,
			 Ipaddr, UID, Gecos]) ->
    User = #ircuser{
      uid=UID,
      sid=SID,
      nick=Nick,
      hop=Hops,
      ts=TS,
      modes=esmisc:parseumode(Usermode),
      ident=Ident,
      host=Hostname,
      ip=Ipaddr,
      realname=Gecos,
      channels=[],
      authenticated=false,
      away=undefined
     },

    ets:insert(State#state.usertable, User),

    error_logger:info_msg("New user: ~p", [User]),
    
    State;

irccmd(kill, State, Killer, [Killee, Reason]) ->
    error_logger:info_msg("Received KILL for ~p from ~p (Reason ~p)",
			  [(resolveuser(State, Killee))#ircuser.nick,
			   (resolveuser(State, Killer))#ircuser.nick,
			   Reason]),
    ets:delete(State#state.usertable, Killee),
    State;

irccmd(quit, State, Quitter, [Reason]) ->
    error_logger:info_msg("User ~p quit (Reason: ~p)",
			  [(resolveuser(State, Quitter))#ircuser.nick,
			   Reason]),
    ets:delete(State#state.usertable, Quitter),
    State;

irccmd(pass, State, [], [RemotePassword, TS, TS_Version, Uplink_UID]) ->
    if
	State#state.remotepassword =/= RemotePassword ->
	    error_logger:info_msg("Server ~p authenticated with the wrong password!", [Uplink_UID]);
	true ->
	    error_logger:info_msg("Server ~p correctly authenticated with us.", [Uplink_UID])
    end,
    error_logger:info_msg("TS params of server: ~p ~p",
			  [TS, TS_Version]),
    State#state{
      uplinkuid=Uplink_UID
     };

irccmd(capab, State, [], [CapabilityList]) ->
    Capabilities = binary:split(CapabilityList, << " " >>, [global]),
    error_logger:info_msg("Our uplink's capabilities: ~p", [Capabilities]),
    State#state{
      uplinkcapabilities=Capabilities
     };

irccmd(server, State, [], [ServerHostname, Hops, ServerDescription]) ->
    Server = #ircserver{
      sid=State#state.uplinkuid,
      hostname=ServerHostname,
      distance=Hops,
      description=ServerDescription,
      uplink=undefined % Direct connection
     },
    error_logger:info_msg("New IRC server: ~p", [Server]),
    ets:insert(State#state.servertable, Server),
    State;

irccmd(svinfo, State, [], _Parameters) ->
    State;

irccmd(Command, State, Instigator, Params) ->
    error_logger:info_msg("Unknown command ~p with instigator ~p and params ~p", [Command, Instigator, Params]),
    State.


resolveuser(#state{usertable=UT}, UID) ->
    [User] = ets:lookup(UT, UID),
    User.
