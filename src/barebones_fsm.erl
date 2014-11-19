-module(barebones_fsm).
-behaviour(gen_fsm).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([s_initial/3, s_initial/2, s_second/3, s_second/2]).
-export([start_link/1]).

-record(state, {
    servername
  }).

%% FSM BEHAVIOUR

start_link(Args) ->
  {ServerName, _} = Args,
  gen_fsm:start_link({local, ServerName}, ?MODULE, Args, []).

init(Args) ->
  { ServerName, _ } = Args,
  lager:info("~p: initializing fsm~n",[ServerName]),
  InitialState = #state{ servername = ServerName },
  {ok, s_initial, InitialState}.

handle_event(Event, StateName, State) ->
  lager:info("~p: uncaught event ~p in ~p~n", [State#state.servername, Event, StateName]),
  {next_state, StateName, State}.

handle_sync_event(Event, From, StateName, State) ->
  lager:info("~p: uncaught sync event ~p from ~p in ~p~n", [State#state.servername, Event, From, StateName]),
  {next_state, StateName, State}.

handle_info(Info, StateName, State) ->
  lager:info("~p: uncaught info ~p in ~p~n", [State#state.servername, Info, StateName]),
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) -> 
  {ok, StateName, State}.

%% CUSTOM STATES

s_initial(next, State) ->
  {next_state, s_second, State};
s_initial(Event, State) ->
  lager:info("~p: unexpected event ~p in s_initial~n", [State#state.servername, Event]),
  {next_state, s_initial, State}.

s_initial(Event, _From, State) ->
  lager:info("~p: unexpected sync event ~p in s_initial~n", [State#state.servername, Event]),
  {next_state, s_initial, State}.

s_second(next, State) ->
  {next_state, s_initial, State};
s_second(_, State) ->
  {next_state, s_second, State}.

s_second(Event, _From, State) ->
  lager:info("~p: unexpected sync event ~p in s_second~n", [State#state.servername, Event]),
  {next_state, s_initial, State}.
