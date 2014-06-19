-module(ook).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% ook Function Exports
%% ------------------------------------------------------------------

%-export([get_table/2,takeover_table/2,free_table/2,list_tables/0]).

%% ------------------------------------------------------------------
%% ook Internal State
%% ------------------------------------------------------------------

-type orphan_list() :: [any()].

-record(state, {
    orphans :: orphan_list()
  }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ook}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_State) ->
  process_flag(trap_exit, true),
  InitialState = #state{},
  {ok, InitialState}.

handle_call(_Msg,_From,State) ->
  {reply, ok, State}.

handle_cast({get_table, OwnerName, TableName, TableOpts}, State) when is_atom(OwnerName) ->
  case find_server(OwnerName, 5000) of
    undefined -> {noreply, State};
    Pid -> handle_cast({get_table, Pid, TableName, TableOpts}, State)
  end;
handle_cast({get_table, OwnerPid, TableName, TableOpts}, State) ->
  case find_orphan({OwnerPid, TableName}, State#state.orphans) of
    undefined ->
      link(OwnerPid),
      Table = ets:new(?MODULE, TableOpts),
      ets:setopts(Table, {heir, self(), TableName}),
      ets:give_away(Table, OwnerPid, []),
      {reply, ok, State};
    TableId -> 
      ets:give_away(TableId, OwnerPid, []),
      NewState = #state{ orphans = remove_orphan({OwnerPid, TableName}, State#state.orphans) },
      {reply, ok, NewState}
  end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'ETS-TRANSFER', TableId, OwnerPid, TableName}, State) ->
  NewState = State#state{ orphans = add_orphan(OwnerPid, TableName,  TableId, State#state.orphans)},
  {noreply, NewState};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
%%

add_orphan(OwnerPid, TableName, TableID, T) ->
  T ++ [{OwnerPid, TableName, TableID}].

remove_orphan({OwnerPid, TableName}, Orphans) ->
  lists:filter(fun ({O,TNam,_Tid}) -> {O, TNam} /= {OwnerPid, TableName} end, Orphans).

find_orphan(_Orphan, []) -> undefined;
find_orphan(Orphan, [{Owner, Table, TableID}|T]) ->
  case Orphan == {Owner, Table} of
    true -> TableID;
    _ -> find_orphan(Orphan, T)
  end.


find_server(_ServerName, 0) -> undefined;
find_server(ServerName, Iterations) ->
  case whereis(ServerName) of
    undefined -> 
      timer:sleep(1),
      find_server(ServerName, Iterations - 1);
    Pid -> Pid
  end.


