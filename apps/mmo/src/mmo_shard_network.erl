-module(mmo_shard_network).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% MMO Shard Internal State
%% ------------------------------------------------------------------

-record(msis, {
    client_table = <<"mmo client ets table x used before initialization!">>,
    shard_name = <<"uninitialized shard name!">>,
    socket = <<"uninitialized shard socket!">>,
    socket_clump_size = 1000,
    counter = 0
  }).


%% ------------------------------------------------------------------
%% MMO Shard Network Function Exports
%% ------------------------------------------------------------------

-export([]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(ShardName) ->
    gen_server:start_link(?MODULE, ShardName, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(ShardName) ->
  {ok, Shards} = application:get_env(mmo, shards),
  [ShardPort] = [P || {S, P} <- Shards, S == ShardName],
  {ok, Socket} = gen_udp:open(ShardPort, [binary, {active, false}, {reuseaddr, true}, {recbuf, 65536}, {sndbuf, 65536}, {buffer, 65536}, {read_packets, 16000}]),
  State = #msis{
    client_table = ets:new(cli_table, [protected, set]),
    socket = Socket,
    shard_name = ShardName
  },
  lager:info("shard network started: ~p on port ~p~n", [ShardName,ShardPort]),
  ok = inet:setopts(Socket,[{active, State#msis.socket_clump_size}]),
  {ok, State}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, _MySocket, TheirIP, TheirPort, TheirMessage}, State) ->
  Counter = State#msis.counter + 1,
  case ets:lookup(State#msis.client_table, {TheirIP, TheirPort}) of
    [] ->
      lager:info("creating a new client: ~p~n",[{TheirIP, TheirPort}]),
      NewClient = new_client({TheirIP, TheirPort}, State),
      mmo_client_fsm:handle_packet(NewClient, {TheirIP, TheirPort}, TheirMessage);
    [{_Key, PreexistingClient}] ->
      lager:info("preexisting client: ~p~n",[PreexistingClient]),
      mmo_client_fsm:handle_packet(PreexistingClient, {TheirIP, TheirPort}, TheirMessage)
  end,
  NewState = State#msis{ counter = Counter },
  %%ok = inet:setopts(State#msis.socket,[{active, State#msis.socket_clump_size}]),
  {noreply, NewState};
handle_info({udp_passive, Socket}, State) ->
  ok = inet:setopts(Socket,[{active, State#msis.socket_clump_size}]),
  {noreply, State};
handle_info(Info, State) ->
  lager:info("misc info: ~p received ~p~n",[State#msis.shard_name, Info]),
  {noreply, State}.

terminate(_Reason, State) ->
  lager:info("shard network on ~p shutting down and releasing clients~n", [State#msis.shard_name]),
  ets:delete(State#msis.client_table),
  ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Exported Function Definitions
%% ------------------------------------------------------------------
%%

-type ipaddr() :: { pos_integer(), pos_integer(), pos_integer(), pos_integer() }.
-type ipport() :: pos_integer().
-spec(new_client({ _I :: ipaddr(), _P :: ipport() }, _State :: any()) -> pid()).
new_client(AddressTuple, State) ->
  {ok, Pid} = gen_fsm:start(mmo_client_fsm, AddressTuple, []),
  ets:insert(State#msis.client_table, {AddressTuple, Pid}),
  Pid.
