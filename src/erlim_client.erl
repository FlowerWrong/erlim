-module(erlim_client).

-export([login/0, sc/0, gc/0, logout/0]).

login() ->
    {ok, Sock} = gen_tcp:connect("localhost", 3000, [binary, {packet, 0}]),
    Msg = <<"{\"cmd\": \"login\", \"username\": \"yang\", \"password\": \"123456\"}">>,
    ok = gen_tcp:send(Sock, Msg).

sc() ->
    {ok, Sock} = gen_tcp:connect("localhost", 3000, [binary, {packet, 0}]),
    Msg = <<"{\"cmd\": \"single_chat\", \"username\": \"yang\", \"password\": \"123456\", \"to\": \"kang\", \"msg\": \"hello world\"}">>,
    ok = gen_tcp:send(Sock, Msg).

gc() ->
    {ok, Sock} = gen_tcp:connect("localhost", 3000, [binary, {packet, 0}]),
    Msg = <<"{\"cmd\": \"group_chat\", \"username\": \"yang\", \"password\": \"123456\", \"to\": \"xiaomiroom\", \"msg\": \"hello world, xiaomi\"}">>,
    ok = gen_tcp:send(Sock, Msg).

logout() ->
    {ok, Sock} = gen_tcp:connect("localhost", 3000, [binary, {packet, 0}]),
    Msg = <<"{\"cmd\": \"logout\", \"username\": \"yang\", \"password\": \"123456\"}">>,
    ok = gen_tcp:send(Sock, Msg).
