-module(cy).

-export([login/0, sc/1, gc/1, add_friend/2, del_friend/2, logout/1, recv/1, loop_recv/1, create_room/1, del_room/2, leave_room/2, change_room_name/3, pull_to_room/3]).


login() ->
    {ok, Sock} = ssl:connect("localhost", 10000, [binary, {packet, 0}, {buffer, 4096}]),
    Msg = <<"{\"cmd\": \"login\", \"name\": \"13560474456\", \"pass\": \"12345678\", \"ack\": \"72cdf1ae-62a3-4ebf-821c-a809d1931293\", \"device\": \"android-xiaomi\"}">>,
    send(Sock, Msg),
    Sock.

sc(Sock) ->
    Msg = iolist_to_binary([<<"{\"cmd\": \"single_chat\", \"to\": 2, \"msg\": \"beginbeginbegin Erlangis a general-purpose, concuris a general-purpose, concurrent, garbage-collectedErlangis a general-purpose, concuris a general-purpose, concurrent, garbage-collected programming lis a geneErlangis a general-purpose, concuris a general-purpose, concurrent, garbage-collected programming lis a geneErlangis a general-purpose, concuris a general-purpose, concurrent, garbage-collected programming lis a geneErlangis a general-purpose, concuris a general-purpose, concurrent, garbage-collected programming lis a geneErlangis a general-purpose, concuris a general-purpose, concurrent, garbage-collected programming lis a gene programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected progl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminl-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programming lis a general-purpose, concurrent, garbage-collected programminramming lis a general-purpose, concurrent, garbage-collected programming lrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain ER-lang) is a general-purpose, concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain ER-lang) is a general-purpose, concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain ER-lang) is a general-purpose, concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain ER-lang) is a general-purpose, concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain ER-lang) is a general-purpose, concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain ER-lang) is a general-purpose, concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain ER-lang) is a general-purpose, concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain ER-lang) is a general-purpose, concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain ER-lang) is a general-purpose, concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain ER-lang) is a general-purpose, concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is almost a functional language (excluding certain endendendend\", \"ack\": \"72cdf1ae-62a3-4ebf-821c-a809d1931293\"}">>]),
    send(Sock, Msg).

gc(Sock) ->
    Msg = iolist_to_binary([<<"{\"cmd\": \"group_chat\", \"to\": 1, \"msg\": \"hello world, xiaomi\", \"ack\": \"72cdf1ae-62a3-4ebf-821c-a809d1931293\"}">>]),
    send(Sock, Msg).

add_friend(Socket, FriendId) when is_integer(FriendId) ->
    DataToSend = jiffy:encode({[{<<"cmd">>, <<"create_friendship">>}, {<<"to">>, FriendId}, {<<"msg">>, <<"I and yang.">>}, {<<"ack">>, <<"abc">>}]}),
    send(Socket, DataToSend).

del_friend(Socket, FriendId) when is_integer(FriendId) ->
    DataToSend = jiffy:encode({[{<<"cmd">>, <<"del_friendship">>}, {<<"to">>, FriendId}]}),
    send(Socket, DataToSend).

create_room(Socket) ->
    DataToSend = jiffy:encode({[{<<"cmd">>, <<"create_room">>}, {<<"name">>, <<"demoroom">>}, {<<"invitable">>, 1}, {<<"members">>, [2]}, {<<"password">>, <<"123456">>}, {<<"description">>, <<"description">>}, {<<"subject">>, <<"subject">>}, {<<"logo">>, <<"logo">>}]}),
    send(Socket, DataToSend).

del_room(Socket, RoomId) ->
    DataToSend = jiffy:encode({[{<<"cmd">>, <<"del_room">>}, {<<"roomid">>, RoomId}]}),
    send(Socket, DataToSend).

leave_room(Socket, RoomId) ->
    DataToSend = jiffy:encode({[{<<"cmd">>, <<"leave_room">>}, {<<"roomid">>, RoomId}]}),
    send(Socket, DataToSend).

change_room_name(Socket, RoomId, NewName) ->
    DataToSend = jiffy:encode({[{<<"cmd">>, <<"change_room_info">>}, {<<"roomid">>, RoomId}, {<<"newname">>, NewName}]}),
    send(Socket, DataToSend).

pull_to_room(Socket, RoomId, UserId) ->
    DataToSend = jiffy:encode({[{<<"cmd">>, <<"pull_to_room">>}, {<<"roomid">>, RoomId}, {<<"userid">>, UserId}]}),
    send(Socket, DataToSend).


logout(Sock) ->
    Msg = iolist_to_binary([<<"{\"cmd\": \"logout\"}">>]),
    send(Sock, Msg),
    ok = gen_tcp:close(Sock).

recv(Socket) ->
    receive
        {ssl, {sslsocket, {gen_tcp, _Sock, tls_connection, undefined}, _} = Socket, Data} ->
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
                            io:format("Group chat msg send success");
                        <<"create_friendship">> ->
                            io:format("create_friendship msg send success")
                    end;
                <<"single_chat">> ->
                    [{<<"from">>, From}, {<<"msg">>, Msg}, {<<"ack">>, MsgId}] = T,
                    io:format("Recv msg ~p, from ~p, ack is ~p~n", [Msg, From, MsgId]),
                    DataToSend = jiffy:encode({[{<<"cmd">>, <<"ack">>}, {<<"action">>, <<"single_chat">>}, {<<"ack">>, MsgId}]}),
                    send(Socket, DataToSend);
                <<"group_chat">> ->
                    [{<<"from">>, From}, {<<"to">>, To}, {<<"msg">>, Msg}, {<<"ack">>, UserRoommsgId}] = T,
                    io:format("Recv msg ~p, from ~p, to room ~p, ack is ~p~n", [Msg, From, To, UserRoommsgId]),
                    DataToSend = jiffy:encode({[{<<"cmd">>, <<"ack">>}, {<<"action">>, <<"group_chat">>}, {<<"ack">>, UserRoommsgId}]}),
                    send(Socket, DataToSend);
                <<"offline_single_chat_msg">> ->
                    [{<<"msg">>, Msgs}, {<<"ack">>, MsgIds}] = T,
                    io:format("Recv offline msgs ~p, ack is ~p~n", [Msgs, MsgIds]),
                    DataToSend = jiffy:encode({[{<<"cmd">>, <<"ack">>}, {<<"action">>, <<"offline_single_chat_msg">>}, {<<"ack">>, MsgIds}]}),
                    send(Socket, DataToSend);
                <<"offline_group_chat_msg">> ->
                    [{<<"msg">>, Msgs}, {<<"ack">>, MsgIds}] = T,
                    io:format("Recv offline msgs ~p, ack is ~p~n", [Msgs, MsgIds]),
                    DataToSend = jiffy:encode({[{<<"cmd">>, <<"ack">>}, {<<"action">>, <<"offline_group_chat_msg">>}, {<<"ack">>, MsgIds}]}),
                    send(Socket, DataToSend);
                <<"offline_notifications">> ->
                    [{<<"msg">>, Msgs}, {<<"ack">>, MsgIds}] = T,
                    io:format("Recv notifications are ~p, ack is ~p~n", [Msgs, MsgIds]),
                    DataToSend = jiffy:encode({[{<<"cmd">>, <<"ack">>}, {<<"action">>, <<"offline_notifications">>}, {<<"ack">>, MsgIds}]}),
                    send(Socket, DataToSend);
                <<"error">> ->
                    [{<<"msg">>, ErrorMsg}, {<<"code">>, Code}] = T,
                    io:format("Error msg is ~p, Code is ~p~n", [ErrorMsg, Code]);
                _ ->
                    ok
            end,
            recv(Socket);
        {ssl_closed, Socket} ->
            io:format("Socket closed.~n");
        _OtherError ->
            io:format("OtherError is ~p~n", [_OtherError])
    end.

loop_recv(Socket) ->
    recv(Socket),
    loop_recv(Socket).

send(Socket, Msg) ->
    PayloadLen = byte_size(Msg),
    Payload = iolist_to_binary([<<"ONECHAT/1.0\r\nPAYLOAD_LEN: ">>, util:integer2binary(PayloadLen), <<"\r\n\r\n">>, Msg]),
    ssl:send(Socket, Payload).