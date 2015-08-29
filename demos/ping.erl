-module(ping).

-export([s/1]).

s(Server) ->
  Server ! {self(), "ping"},
  receive
    {_Server, Msg} ->
      Msg
  end.
