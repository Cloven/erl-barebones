-module(bare_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, list_subscribers/0, list_subscribers/1, tell_subscribers/2, tell_subscriber/2]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-type subscription_t() :: <<>>.
-type message_t() :: <<>>.

-spec start(_, _) -> { ok, pid()}.
start(_Type, _Args) ->
    %% 'spirals' are meters that operate over the trailing 60 seconds
    folsom_metrics:new_spiral(fspiral),
    %% set up the websocket subscription table if it doesn't exist yet
    case ets:info(ws_subscriptions) of
      undefined -> ets:new(ws_subscriptions, [bag, named_table, public]);
      _ -> ok
    end,
    Dispatch = cowboy_router:compile([
            {'_', [
                    {"/", cowboy_static, {priv_file, bare, "index.html"}},
                    {"/websocket/:id", ws_handler, []},
                    {"/static/[...]", cowboy_static, {priv_dir, bare, "static"}}
            ]}
    ]),
    %% Name, NbAcceptors, TransOpts, ProtoOpts
    cowboy:start_http(my_http_listener, 100,
        [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    bare_sup:start_link().

-spec stop(_S) -> ok.
stop(_State) ->
    ok.

-spec list_subscribers(subscription_t()) -> ok.
list_subscribers(Channel) ->
  io:format("~p~n",[ets:lookup(ws_subscriptions, Channel)]),
  ok.

-spec list_subscribers() -> ok.
list_subscribers() ->
  io:format("~p~n",[ets:match(ws_subscriptions, '$1')]).

-spec tell_subscriber(pid(), message_t()) -> ok.
tell_subscriber(Pid, Message) ->
  io:format("sending ~p to ~p~n", [Message, Pid]),
  Pid ! Message,
  ok.

-spec tell_subscribers(subscription_t(), message_t()) -> ok.
tell_subscribers(Channel, Message) ->
  Subs = ets:lookup(ws_subscriptions, Channel),
  lists:map(fun (X) -> {_, Pid} = X, tell_subscriber(Pid, Message), folsom_metrics:notify({fspiral, 1})  end, Subs),
  ok.
