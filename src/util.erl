-module(util).

-export([timestamp/0, datetime2timestamp/1, uuid/0]).

%% 16位的timestamp
timestamp() ->
    {Mega, Sec, Micro} = erlang:now(),
    (Mega * 1000000 + Sec) * 1000000 + Micro.

datetime2timestamp(DateTime) ->
    %% 62167219200 == calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200.
    %% {Seconds div 1000000, Seconds rem 1000000, 0}.

%% uuid v4
uuid() ->
    uuid:generate().
