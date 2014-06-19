#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname udpgun

%%-mode(compile).

main(Args) ->
  register(udpgun, self()),
  {Time, Value} = timer:tc( fun () -> 
      start_team(32),
      get_results(32)
    end),
  io:format("Time: ~p value ~p~n",[Time,Value]).

get_results(0) -> ok;
get_results(N) ->
  receive
    done -> ok;
    _ -> fail
  end,
  get_results(N-1).

start_team(0) -> ok;
start_team(Number) -> 
  spawn(fun () -> runner(Number) end),
  start_team(Number - 1).

runner(N) ->
  {ok, Socket} = gen_udp:open(5000 + N, [{active, true}, binary]),
  sendpacket(Socket, 100000),
  udpgun ! done.

sendpacket(_Socket, 0) -> ok;
sendpacket(Socket, N) -> 
  %%P = (N rem 3) + 9001,
  P = 9001,
  gen_udp:send(Socket, {127,0,0,1}, P, <<"12345678901234567890123456789012345678901234567890123456789012345678901234">>),
  sendpacket(Socket, N - 1).

