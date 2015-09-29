-module(toml_util).

-export([parse/0]).

parse() ->
    Data = readlines("/etc/erlim/erlim.toml"),
    etoml:parse(Data).

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try get_all_lines(Device)
    after file:close(Device)
    end.

get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> Line ++ get_all_lines(Device)
    end.
