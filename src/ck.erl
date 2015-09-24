-module(ck).

-export([login/0, sc/1, gc/1, logout/1, recv/1, loop_recv/1]).


login() ->
    {ok, Sock} = gen_tcp:connect("localhost", 8080, [binary, {packet, 0}, {buffer, 512}]),
    Msg = <<"{\"cmd\": \"login\", \"name\": \"13560474457\", \"pass\": \"12345678\", \"ack\": \"72cdf1ae-62a3-4ebf-821c-a809d1931293\", \"device\": \"android-xiaomi\"}">>,
    send(Sock, Msg),
    Sock.

sc(Sock) ->
    Msg = iolist_to_binary([<<"{\"cmd\": \"single_chat\", \"to\": 2, \"msg\": \"beginbeginbegin Erlangis a general-purpose, concuris a general-purpose, concurrent, garbage-collectedErlangis a general-purpose, concuris a general-purpose, concurrent, garbage-collected programming lis a geneErlangis a general-purpose, concuris a general-purpose, concurrent, garbage-collected programming lis a geneErlangis a general-purpose, concuris a general-purpose, concurrent, garbage-collected programming lis a geneErlangis a general-purpose, concuris a general-purpose, concurrent, garbage-collected programming lis a geneErlangis a general-purpose, concuris a general-purpose, concurrent, garbage-collected programming lis a gene programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected progl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminramming lis a general-purpose, concurrent, garbage-collected programming lrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain ER-lang) is a general-purpose, concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain ER-lang) is a general-purpose, concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain ER-lang) is a general-purpose, concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain ER-lang) is a general-purpose, concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain ER-lang) is a general-purpose, concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain ER-lang) is a general-purpose, concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain ER-lang) is a general-purpose, concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain ER-lang) is a general-purpose, concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain ER-lang) is a general-purpose, concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain ER-lang) is a general-purpose, concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain endendendend\", \"ack\": \"72cdf1ae-62a3-4ebf-821c-a809d1931293\"}">>]),
    send(Sock, Msg).

gc(Sock) ->
    Msg = iolist_to_binary([<<"{\"cmd\": \"group_chat\", \"to\": 1, \"msg\": \"hello world, xiaomi\", \"ack\": \"72cdf1ae-62a3-4ebf-821c-a809d1931293\"}">>]),
    send(Sock, Msg).

logout(Sock) ->
    Msg = iolist_to_binary([<<"{\"cmd\": \"logout\"}">>]),
    send(Sock, Msg),
    ok = gen_tcp:close(Sock).

recv(Socket) ->
    receive
        {tcp, Socket, Data} ->
            DataList = string:tokens(binary_to_list(Data), "\r\n"),
            _IsJSON = jsx:is_json(Data),
            PayloadTmp = lists:nth(3, DataList),
            Payload = list_to_binary(PayloadTmp),
            Json = jiffy:decode(Payload),
            io:format("Json is ~p.~n", [Json]),
            {[{<<"cmd">>, Cmd} | T]} = Json,
            case Cmd of
                <<"ack">> ->
                    [{<<"action">>, Action}, {<<"ack">>, _Ack}] = T,
                    case Action of
                        <<"login">> ->
                            io:format("Login success");
                        <<"single_chat">> ->
                            io:format("Single chat msg send success");
                        <<"group_chat">> ->
                            io:format("Group chat msg send success")
                    end;
                <<"single_chat">> ->
                    [{<<"from">>, From}, {<<"msg">>, Msg}, {<<"ack">>, MsgId}] = T,
                    io:format("Recv msg ~p, from ~p, ack is ~p", [Msg, From, MsgId]),
                    DataToSend = jiffy:encode({[{<<"cmd">>, <<"ack">>}, {<<"action">>, <<"single_chat">>}, {<<"ack">>, MsgId}]}),
                    send(Socket, DataToSend);
                <<"group_chat">> ->
                    [{<<"from">>, From}, {<<"to">>, To}, {<<"msg">>, Msg}, {<<"ack">>, UserRoommsgId}] = T,
                    io:format("Recv msg ~p, from ~p, to room ~p, ack is ~p", [Msg, From, To, UserRoommsgId]),
                    DataToSend = jiffy:encode({[{<<"cmd">>, <<"ack">>}, {<<"action">>, <<"group_chat">>}, {<<"ack">>, UserRoommsgId}]}),
                    send(Socket, DataToSend);
                <<"offline_single_chat_msg">> ->
                    [{<<"msg">>, Msgs}, {<<"ack">>, MsgIds}] = T,
                    io:format("Recv offline msgs ~p, ack is ~p", [Msgs, MsgIds]),
                    DataToSend = jiffy:encode({[{<<"cmd">>, <<"ack">>}, {<<"action">>, <<"offline_single_chat_msg">>}, {<<"ack">>, MsgIds}]}),
                    send(Socket, DataToSend);
                <<"offline_group_chat_msg">> ->
                    [{<<"msg">>, Msgs}, {<<"ack">>, MsgIds}] = T,
                    io:format("Recv offline msgs ~p, ack is ~p", [Msgs, MsgIds]),
                    DataToSend = jiffy:encode({[{<<"cmd">>, <<"ack">>}, {<<"action">>, <<"offline_group_chat_msg">>}, {<<"ack">>, MsgIds}]}),
                    send(Socket, DataToSend);
                <<"error">> ->
                    [{<<"msg">>, ErrorMsg}, {<<"code">>, Code}] = T,
                    io:format("Error msg is ~p, Code is ~p~n", [ErrorMsg, Code]);
                _ ->
                    ok
            end,
            recv(Socket);
        {tcp_close, Socket} ->
            io:format("Socket closed.~n"),
            ok
    end.

loop_recv(Socket) ->
    recv(Socket),
    loop_recv(Socket).

send(Socket, Msg) ->
    PayloadLen = byte_size(Msg),
    Payload = iolist_to_binary([<<"ONECHAT/1.0\r\nPAYLOAD_LEN: ">>, util:integer2binary(PayloadLen), <<"\r\n\r\n">>, Msg]),
    gen_tcp:send(Socket, Payload).