-module(util).

-export([timestamp/0, uuid/0]).

timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    (Mega*1000000+Sec)*1000000+Micro.

uuid() ->
    uuid:generate().