-module(schedule_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([get_json/2]).

init(_, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, get_json}
    ], Req, State}.

%% @doc two argvs: port and type, type is websocket, onechat or rest_api
get_json(Req, State) ->
    {Port, _} = cowboy_req:qs_val(<<"port">>, Req, <<"8080">>),
    {Type, _} = cowboy_req:qs_val(<<"type">>, Req, <<"onechat">>),
    io:format("Port is ~p~n", [Port]),
    io:format("Type is ~p~n", [Type]),
    io:format("req is ~p~n", [Req]),
    Body = <<"{\"rest\": \"Hello World!\"}">>,
    {Body, Req, State}.
