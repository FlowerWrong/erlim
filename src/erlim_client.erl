-module(erlim_client).

-export([chat/0]).

chat() ->
    {ok, Sock} = gen_tcp:connect("localhost", 3000, [binary, {packet, 0}]),
    Msg = <<"{\"cmd\": \"login\", \"username\": \"yang\", \"password\": \"123456\"}">>,
    ok = gen_tcp:send(Sock, Msg).

    %% ok = gen_tcp:close(Sock).
