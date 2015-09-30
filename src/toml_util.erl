%%%-------------------------------------------------------------------
%%% @author yang
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%   utils for <a href="https://github.com/toml-lang/toml">toml</a>
%%% @end
%%% Created : 27. 九月 2015 下午12:17
%%%-------------------------------------------------------------------
-module(toml_util).

-export([parse/0]).

%% @doc parse the "/etc/erlim/erlim.toml" file
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
        eof -> [];
        Line -> Line ++ get_all_lines(Device)
    end.
