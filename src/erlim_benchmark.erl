%%%-------------------------------------------------------------------
%%% @author yang
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%   onechat benchmark
%%% @end
%%% Created : 27. 九月 2015 下午12:17
%%%-------------------------------------------------------------------
-module(erlim_benchmark).

-export([start/0, run/0]).

-record(server, {host = "localhost", port = 8080, maxclients = 10000, interval = 10}).

%% @doc start benchmark
start() ->
    for(10000).

%% @doc run benchmark
run() ->
    {ok, Sock} = gen_tcp:connect("localhost", 8080, [binary, {packet, 0}]),
    Msg = <<"{\"cmd\": \"login\", \"name\": \"13560474456\", \"pass\": \"12345678\", \"ack\": \"72cdf1ae-62a3-4ebf-821c-a809d1931293\", \"device\": \"android-xiaomi\"}">>,
    ok = gen_tcp:send(Sock, Msg).

for(N) when N > 0 ->
     spawn(?MODULE, run, []),
     for(N - 1);
for(0)->
     io:format("~n"),
     ok;
for(_N)-> badarg.
