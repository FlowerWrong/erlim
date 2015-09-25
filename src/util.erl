-module(util).

-export([datetime2timestamp/1, uuid/0, integer2binary/1]).

%% 16位的timestamp
%% timestamp() ->
%%     {Mega, Sec, Micro} = erlang:now(),
%%     (Mega * 1000000 + Sec) * 1000000 + Micro.

datetime2timestamp(DateTime) ->
    %% 62167219200 == calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200.
    %% {Seconds div 1000000, Seconds rem 1000000, 0}.

%% uuid v4
uuid() ->
    uuid:generate().

integer2binary(Number) when is_integer(Number) ->
    list_to_binary(integer_to_list(Number)).