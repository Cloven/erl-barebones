-module(barebones_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% typed children helper macro
-define(CHILD(CName, I, Type, Options), {CName, {I, start_link, [Options]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    BarebonesFsm = ?CHILD(barebones_fsm_child, barebones_fsm, worker, {bob, {some, arguments}}),
    {ok, { {one_for_one, 5, 10}, [ BarebonesFsm ]} }.
