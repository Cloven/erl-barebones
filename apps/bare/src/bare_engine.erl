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
%% SkelEngine Function Exports
%% ------------------------------------------------------------------

-export([greet/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_State) ->
  {ok, C} = eredis:start_link(),
  {ok, C}.

handle_call(hello, From, C) ->
    io:format("hello, world~n"),
    spawn_link(fun() -> handle_queue(From, C) end),
    {reply, ok, C};
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

greet() ->
  gen_server:call(?MODULE, hello).

handle_queue(_From, C) ->
    {ok, Val}  = eredis:q(C, ["BLPOP", "queue", "0"]),
    io:format("got a thang! ~p~n", [Val]).
