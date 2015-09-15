-module(util).

-export([timestamp/0, uuid/0]).

%% 16位的timestamp
timestamp() ->
    {Mega, Sec, Micro} = erlang:now(),
    (Mega * 1000000 + Sec) * 1000000 + Micro.

%% uuid v4
uuid() ->
    uuid:generate().