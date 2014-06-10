-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
        %% obtain the 'id' parameter from the request, representing the 'room' to join
        { Identifier, _ } = cowboy_req:binding(id, Req),
        MyState = { Identifier, self() },
        ets:insert(ws_subscriptions, MyState),
        {ok, Timeout} = application:get_env(bare, client_timeout, 30000),
	{ok, Req, MyState, Timeout}. 

websocket_handle({text, Msg}, Req, State) ->
	{reply, {text, << "pong: ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info(Info, Req, State) ->
	{reply, {text, Info}, Req, State}.

websocket_terminate(_Reason, _Req, State) ->
        io:format("ws terminating ~p~n",[State]),
        ets:delete_object(ws_subscriptions, State),
	ok.
