-module(ck).

-export([login/0, sc/1, gc/1, logout/1]).

login() ->
    {ok, Sock} = gen_tcp:connect("localhost", 3000, [binary, {packet, 0}]),
    Msg = <<"{\"cmd\": \"login\", \"username\": \"kang\", \"password\": \"123456\"}">>,
    ok = gen_tcp:send(Sock, Msg),
    Sock.

sc(Sock) ->
    Msg = <<"{\"cmd\": \"single_chat\", \"username\": \"kang\", \"password\": \"123456\", \"to\": \"kang\", \"msg\": \"hello world\"}">>,
    ok = gen_tcp:send(Sock, Msg).

gc(Sock) ->
    Msg = <<"{\"cmd\": \"group_chat\", \"username\": \"kang\", \"password\": \"123456\", \"to\": \"xiaomiroom\", \"msg\": \"hello world, xiaomi\"}">>,
    ok = gen_tcp:send(Sock, Msg).

logout(Sock) ->
    Msg = <<"{\"cmd\": \"logout\", \"username\": \"kang\", \"password\": \"123456\"}">>,
    ok = gen_tcp:send(Sock, Msg),
    ok = gen_tcp:close(Sock).
