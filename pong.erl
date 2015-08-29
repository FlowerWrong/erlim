-module(pong).

-export([start/0, a/0]).

start() ->
  spawn(pong, a, []).

a() ->
  receive
    {Client, Msg} ->
      io:format("Client is ~p, Msg is ~p", [Client, Msg]),
      Client ! {self(), "pong"}
  end,
  a().
