-module(ck).

-export([login/0, sc/1, gc/1, logout/1, recv/1, loop_recv/1]).

login() ->
    {ok, Sock} = gen_tcp:connect("localhost", 8080, [binary, {packet, 0}]),
    Msg = <<"{\"cmd\": \"login\", \"name\": \"13560474457\", \"pass\": \"12345678\", \"ack\": \"72cdf1ae-62a3-4ebf-821c-a809d1931293\"}">>,
    ok = gen_tcp:send(Sock, Msg),
    Sock.

sc(Sock) ->
    Msg = iolist_to_binary([<<"{\"cmd\": \"single_chat\", \"to\": 1, \"msg\": \"hello world\", \"ack\": \"72cdf1ae-62a3-4ebf-821c-a809d1931293\"}">>]),
    ok = gen_tcp:send(Sock, Msg).

gc(Sock) ->
    Msg = iolist_to_binary([<<"{\"cmd\": \"group_chat\", \"to\": 1, \"msg\": \"hello world, xiaomi\", \"ack\": \"72cdf1ae-62a3-4ebf-821c-a809d1931293\"}">>]),
    ok = gen_tcp:send(Sock, Msg).

logout(Sock) ->
    Msg = iolist_to_binary([<<"{\"cmd\": \"logout\"}">>]),
    ok = gen_tcp:send(Sock, Msg),
    ok = gen_tcp:close(Sock).

recv(Socket) ->
    receive
        {tcp, Socket, Data} ->
            io:format("Client received msg is: ~p~n", [Data])
    end.

loop_recv(Socket) ->
    recv(Socket),
    loop_recv(Socket).
