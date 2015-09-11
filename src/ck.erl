-module(ck).

-export([login/0, sc/2, gc/2, logout/2, loop_recv/1]).

login() ->
    {ok, Sock} = gen_tcp:connect("localhost", 3000, [binary, {packet, 0}]),
    Msg = <<"{\"cmd\": \"login\", \"name\": \"13560474457\", \"pass\": \"12345678\"}">>,
    ok = gen_tcp:send(Sock, Msg),
    Token = receive
        {tcp, Socket, Data} ->
            jiffy:decode(Data)
    end,
    io:format("token is ~p~n", [Token]),
    {[{<<"token">>, TokenStr}]} = Token,
    {Sock, TokenStr}.

sc(Sock, Token) ->
    Msg = iolist_to_binary([<<"{\"cmd\": \"single_chat\", \"token\": \"">>, Token, <<"\", \"to\": \"13560474456\", \"msg\": \"hello world\"}">>]),
    ok = gen_tcp:send(Sock, Msg).

gc(Sock, Token) ->
    Msg = iolist_to_binary([<<"{\"cmd\": \"group_chat\", \"token\": \"">>, Token, <<"\", \"to\": 1, \"msg\": \"hello world, xiaomi\"}">>]),
    ok = gen_tcp:send(Sock, Msg).

logout(Sock, Token) ->
    Msg = iolist_to_binary([<<"{\"cmd\": \"logout\", \"token\": \"">>, Token, <<"\"}">>]),
    ok = gen_tcp:send(Sock, Msg),
    ok = gen_tcp:close(Sock).

loop_recv(Socket) ->
    receive
        {tcp, Socket, String} ->
            io:format("Client received msg is: ~p~n", [String])
    end.
