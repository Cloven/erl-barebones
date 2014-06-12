-module(bare_engine).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

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
%% BareEngine Function Exports
%% ------------------------------------------------------------------

-export([queue_worker/0,handle_queue/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    lager:info("bare engine starting"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(State) ->
  %% eventually change to have dynamic number of redis queue listeners
  spawn_link(fun() -> queue_worker() end),
  lager:info("bare engine initialized"),
  {ok, State}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

queue_worker() ->
  {ok, C} = eredis:start_link(),
  handle_queue(C).

handle_queue(C) ->
    {ok, [_, Val]}  = eredis:q(C, ["blpop", "wsqueue", "0"], infinity),
    <<RawAddress:10/binary, Message/binary>> = Val,
    Address = erlang:list_to_binary(string:strip(erlang:binary_to_list(RawAddress))),
    bare_app:tell_subscribers(Address, Message),
    handle_queue(C).
