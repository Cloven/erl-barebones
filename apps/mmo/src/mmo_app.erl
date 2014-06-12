-module(mmo_app).
-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start() -> {'error',{atom(),_}} | {'ok',[atom()]}.
start() ->
  application:ensure_all_started(mmo).

-spec start(_, _) -> { ok, pid() }.
start(_Type, _Args) ->
    lager:info("mmo started"),
    mmo_sup:start_link().

-spec stop(_S) -> ok.
stop(_State) ->
    ok.
