-module(mmo_shard_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% regular children helper macro
-define(CHILD(I, Type, J), {I, {I, start_link, [J]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

%%start_link() ->
%%    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(X) ->
    supervisor:start_link({local, X}, ?MODULE, X).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(ShardName) ->
    Shard = ?CHILD(mmo_shard_network, worker, ShardName),
    {ok, { {one_for_one, 5, 10}, [ Shard ]} }.
