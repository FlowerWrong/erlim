-module(ck).

-export([login/0, sc/2, gc/2, logout/2, recv/1, loop_recv/1]).

login() ->
    {ok, Sock} = gen_tcp:connect("localhost", 3000, [binary, {packet, 0}]),
    Msg = <<"{\"cmd\": \"login\", \"name\": \"13560474457\", \"pass\": \"12345678\"}">>,
    ok = gen_tcp:send(Sock, Msg),
    Token = receive
        {tcp, _Socket, Data} ->
            jiffy:decode(Data)
    end,
    io:format("token is ~p~n", [Token]),
    {[{<<"token">>, TokenStr}]} = Token,
    {Sock, TokenStr}.

sc(Sock, Token) ->
    Msg = iolist_to_binary([<<"{\"cmd\": \"single_chat\", \"token\": \"">>, Token, <<"\", \"to\": 2, \"msg\": \"hello world\"}">>]),
    ok = gen_tcp:send(Sock, Msg).

gc(Sock, Token) ->
    Msg = iolist_to_binary([<<"{\"cmd\": \"group_chat\", \"token\": \"">>, Token, <<"\", \"to\": 1, \"msg\": \"hello world, xiaomi\"}">>]),
    ok = gen_tcp:send(Sock, Msg).

logout(Sock, Token) ->
    Msg = iolist_to_binary([<<"{\"cmd\": \"logout\", \"token\": \"">>, Token, <<"\"}">>]),
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
