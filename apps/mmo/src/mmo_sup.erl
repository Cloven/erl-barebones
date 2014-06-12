-module(mmo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% regular children helper macro
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% mmo shard supervisor children helper macro
-define(SUPERCHILD(I), {I, {mmo_shard_sup, start_link, [I]}, transient, infinity, supervisor, [mmo_shard_sup]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, Shards} = application:get_env(mmo, shards),
  lager:info("starting shards: ~p~n", [Shards]),
  ShardList = [?SUPERCHILD(ShardName) || {ShardName, _Port} <- Shards],
  {ok, { {one_for_one, 5, 10}, ShardList} }.
