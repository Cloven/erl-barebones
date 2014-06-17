-module(mmo_client_fsm).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, s_initial/2, s_second/2, 
         handle_event/3,
         handle_sync_event/4, handle_info/3, 
         terminate/3,
         code_change/4, handle_packet/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

%% ------------------------------------------------------------------
%% MMO Client Internal State
%% ------------------------------------------------------------------

-record(cs,
  {
    ip_addr, % {ip, port}
    sent_count = 0,
    recv_count = 0,
    seq = 0,
    myack = 0,
    unacks = []
  }).

-define(ACKLENGTH, 32).
-define(PACKET, <<Version:8, Seq:16, Seen:16, Ack:?ACKLENGTH, Cmd:8, Arg/binary>>).
-define(ACKHIGHBIT, (1 bsl ?ACKLENGTH)).
%%
%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------


init(IPAddressTuple) ->
  lager:info("initializing, ip_addr is ~p~n",[IPAddressTuple]),
  {ok, s_initial, #cs{ ip_addr = IPAddressTuple } }.

s_initial(_Event, State) ->
  lager:info("in initial state"),
  {next_state, s_second, State}.

s_second(_Event, State) ->
  lager:info("in second state"),
  {next_state, s_initial, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(Event, From, StateName, State) ->
  lager:error("fsm received sync event: ~p from ~p in ~p~n", [Event, From, StateName]),
  {reply, ok, StateName, State}.

handle_info(Info, StateName, State) ->
  lager:info("fsm received info: ~p in state ~p~n", [Info,StateName]),
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec(handle_packet(_FsmP :: pid(), _IPAddr :: any(),  _Ev :: any()) -> any()).
handle_packet(FsmPid, _IPAddrTuple, ?PACKET = P) ->
  lager:info("V: ~p Sq: ~p Sn: ~p Ak: ~p C: ~p Ar: ~p~n",[Version,Seq,Seen,Ack,Cmd,Arg]),
  gen_fsm:send_event(FsmPid, {packet, P});
handle_packet(FsmPid, IPAddrTuple, ErrorPacket) ->
  lager:error("fsm ~p discarding error packet from ~p: ~p~n",[FsmPid, IPAddrTuple, ErrorPacket]).
