%%%-------------------------------------------------------------------
%%% @author yang
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%   onechat tcp receiver
%%% @end
%%% Created : 27. 九月 2015 下午12:17
%%%-------------------------------------------------------------------
-module(erlim_tls_receiver).

-behaviour(gen_server).

%% API functions
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(HIBERNATE_TIMEOUT, 90000).

-record(state, {
    transport,
    socket :: port(),
    protocol :: atom(),
    heartbeat_timeout = ?HIBERNATE_TIMEOUT :: integer(),
    client_pid :: pid(),
    ip :: binary(),
    port :: integer(),
    uid :: integer(),
    device :: binary(),
    node :: node(),
    data_complete = 0 :: integer(),  %% 0: 开始接收 1: 接收剩余
    client_data = [] :: list(),
    payload_len :: integer(),
    already_receive_payload_len = 0 :: integer()
}).

-include("table.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(SockArgs) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [SockArgs])}.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(SockArgs = {Transport, _Sock, _SockFun}) ->
    {ok, NewSock} = esockd_connection:accept(SockArgs),
    Transport:async_recv(NewSock, 0, infinity),
    {ok, {IP, Port}} = Transport:peername(NewSock),
    NewState = #state{transport = Transport, socket = NewSock, ip = IP, port = Port},
    gen_server:enter_loop(?MODULE, [], NewState).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({inet_async, Socket, _Ref, {ok, Data}}, #state{transport = Transport, socket = Socket, data_complete = DCFlag, client_data = ClientData, payload_len = PayloadLen, already_receive_payload_len = AlreadyReceivePayloadLen, protocol = P} = State) ->
    io:format("Data is ~p~n", [Data]),
    NewState = case DCFlag of
                   0 ->
                       case string:str(binary_to_list(Data), "ONECHAT/1.0\r\n") of
                           0 ->
                               case ws_util:is_websocket(Data) of
                                   true ->
                                       PayLoadLength = ws_util:get_payload_len(Data),
                                       WsDataList = ws_util:get_packet_data(Data),
                                       WsData = list_to_binary(WsDataList),
                                       Alrpl = byte_size(WsData),
                                       lager:info("Payloadlen is ~p, alrpl is ~p~n", [PayLoadLength, Alrpl]),
                                       case PayLoadLength =:= Alrpl of
                                           true ->
                                               case jsx:is_json(WsData) of
                                                   true -> process_data(Transport, WsData, Socket, State, websocket);
                                                   false ->
                                                       erlim_client:reply_error(Transport, Socket, <<"invide json">>, 10400, websocket),
                                                       State#state{protocol = websocket}
                                               end;
                                           false ->
                                               NewClientData = [WsData | ClientData],
                                               State#state{data_complete = 1, client_data = NewClientData, payload_len = PayLoadLength, protocol = websocket, already_receive_payload_len = Alrpl}
                                       end;
                                   false ->
                                       case erlang:decode_packet(http_bin, Data, []) of
                                           {ok, {http_request, _Method, _RawPath, _Version}, Rest} ->
                                               %% 解析http头部
                                               RestHeadersTmp = cow_http:parse_headers(Rest),
                                               {RestHeaders, <<>>} = RestHeadersTmp,
                                               case lists:member({<<"upgrade">>, <<"websocket">>}, RestHeaders) of
                                                   true ->
                                                       Keys = lists:filter(fun(E) ->
                                                           case catch {<<"sec-websocket-key">>, _Key} = E of
                                                               E -> true;
                                                               _Error -> false
                                                           end
                                                                           end, RestHeaders),
                                                       [{<<"sec-websocket-key">>, Key}] = Keys,
                                                       %% 计算加密key
                                                       AcceptKey = ws_util:key(Key),
                                                       WebSocketDataToBeSend = iolist_to_binary([<<"HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: ">>, AcceptKey, <<"\r\n\r\n">>]),
                                                       Transport:send(Socket, WebSocketDataToBeSend),
                                                       State#state{protocol = websocket};
                                                   false ->
                                                       %% http 直接关闭客户端
                                                       self() ! {tcp_closed, Socket},
                                                       State#state{protocol = http}
                                               end;
                                           _ ->
                                               %% 解析错误 直接关闭客户端
                                               self() ! {tcp_closed, Socket},
                                               State
                                       end
                               end;
                           1 ->
                               %% note: string:tokens 分割失败
                               DataList = re:split(binary_to_list(Data), "\r\n\r\n"),
                               lager:info("DataList is ~p~n", [DataList]),
                               case length(DataList) > 2 of
                                   true ->
                                       erlim_client:reply_error(Transport, Socket, <<"data is invide, may be you have more \r\n">>, 10400, tcp),
                                       State;
                                   false ->
                                       HeaderBinary = lists:nth(1, DataList),
                                       Header = string:tokens(binary_to_list(HeaderBinary), "\r\n"),
                                       PayloadBinary = lists:nth(2, DataList),
                                       PayloadLength0 = string:tokens(lists:nth(2, Header), ": "),
                                       PayloadLength1 = lists:nth(2, PayloadLength0),
                                       PayloadLength = list_to_integer(PayloadLength1),
                                       if
                                           PayloadLength > 10400 ->
                                               erlim_client:reply_error(Transport, Socket, <<"data must less than 8192 bytes">>, 10400, tcp),
                                               State;
                                           true ->
                                               %% 已经接收到的数据大小
                                               Alrpl = byte_size(PayloadBinary),
                                               lager:info("Alrpl is ~p~n", [Alrpl]),
                                               case Alrpl =:= PayloadLength of
                                                   false ->
                                                       NewClientData = [PayloadBinary | ClientData],
                                                       State#state{data_complete = 1, client_data = NewClientData, payload_len = PayloadLength, protocol = tcp, already_receive_payload_len = Alrpl};
                                                   true ->
                                                       S = State#state{data_complete = 0, client_data = [], payload_len = undefined, protocol = tcp, already_receive_payload_len = 0},
                                                       process_data(Transport, PayloadBinary, Socket, S, tcp)
                                               end
                                       end
                               end
                       end;
                   1 ->
                       NewClientData = [Data | ClientData],
                       %% 已经接收到的数据大小
                       Alrpl = byte_size(Data) + AlreadyReceivePayloadLen,
                       lager:info("Payloadlen is ~p, alrpl is ~p~n", [PayloadLen, Alrpl]),
                       case Alrpl =:= PayloadLen of
                           false ->
                               State#state{data_complete = 1, client_data = NewClientData, already_receive_payload_len = Alrpl, protocol = P};
                           true ->
                               S = State#state{data_complete = 0, client_data = [], payload_len = undefined, protocol = P, already_receive_payload_len = 0},
                               PayloadOver = iolist_to_binary(lists:reverse(NewClientData)),
                               lager:info("Payload last is ~p~n", [PayloadOver]),

                               case jsx:is_json(PayloadOver) of
                                   true -> process_data(Transport, PayloadOver, Socket, S, P);
                                   false ->
                                       erlim_client:reply_error(Transport, Socket, <<"invide json">>, 10400, P),
                                       S
                               end
                       end
               end,
    Transport:async_recv(Socket, 0, infinity),
    {noreply, NewState, ?HIBERNATE_TIMEOUT};
handle_info({inet_async, _Sock, _Ref, {error, Reason}}, State) ->
    {stop, {shutdown, {inet_async_error, Reason}}, State};
handle_info(_Info, State) ->
    lager:info("_Info is ~p~n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{client_pid = ClientPid, device = Device}) ->
    %% 这里有三种情况
    %% 1. 用户正常退出, 或网络掉线退出
    lager:info("ClientPid ~p will be terminated.~n", [ClientPid]),
    ok = case ClientPid of
             undefined -> ok;
             _ ->
                 SessionMnesia = mnesia_util:query_session_by_pid_and_device(ClientPid, Device),
                 lager:info("SessionMnesia is ~p.~n", [SessionMnesia]),
                 case SessionMnesia of
                     false -> undefined;
                     #session{uid = Uid} ->
                         ok = erlim_sm:logout(Uid, Device),
                         ok = erlim_client:stop(ClientPid)
                 end
         end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc process socket data
process_data(Transport, Data, Socket, State, Protocol) ->
    JsonMap = jiffy:decode(Data, [return_maps]),
    lager:info("Json is ~p.~n", [JsonMap]),
    Cmd = maps:get(<<"cmd">>, JsonMap),
    if
        Cmd =:= <<"login">> ->
            %% 登陆
            Name = maps:get(<<"name">>, JsonMap),
            Pass = maps:get(<<"pass">>, JsonMap),
            Ack = maps:get(<<"ack">>, JsonMap),
            Device = maps:get(<<"device">>, JsonMap),
            LoginUserMysql = mysql_util:query_user_by_mobile(Name),
            case LoginUserMysql of
                [] ->
                    erlim_client:reply_error(Transport, Socket, <<"404 Not Found user with this name">>, 10404, Protocol),
                    State;
                #user_record{password_digest = PD, id = Uid} ->
                    PassDigest = binary_to_list(PD),
                    {ok, PassDigest} =:= bcrypt:hashpw(Pass, PassDigest),
                    ClientPid = erlim_client_sup:start_child(Transport, Socket, Protocol),
                    erlim_sm:login(Uid, ClientPid, Device),
                    %% Send login ack to client
                    erlim_client:reply_ack(Transport, Socket, <<"login">>, Ack, Protocol),

                    %% 登陆成功后推送离线消息
                    %% 私聊离线消息
                    Msgs = mysql_util:user_msgs(Uid, 1),
                    case Msgs of
                        [] -> ok;
                        _ ->
                            MsgsForJson = lists:map(fun(M) ->
                                {datetime, CreatedAtD} = M#msg_record.created_at,
                                {datetime, UpdatedAtD} = M#msg_record.updated_at,
                                CreatedAtT = util:datetime2timestamp(CreatedAtD),
                                UpdatedAtT = util:datetime2timestamp(UpdatedAtD),
                                {[{id, M#msg_record.id}, {f, M#msg_record.f}, {t, M#msg_record.t}, {msg, M#msg_record.msg}, {unread, M#msg_record.unread}, {created_at, CreatedAtT}, {updated_at, UpdatedAtT}]}
                                                    end, Msgs),
                            MsgsIds = lists:map(fun(M) ->
                                M#msg_record.id
                                                end, Msgs),
                            MsgDataToBeSend = jiffy:encode({[{<<"cmd">>, <<"offline_single_chat_msg">>}, {<<"msg">>, MsgsForJson}, {<<"ack">>, MsgsIds}]}),
                            lager:info("offline single chat msgs are ~p~n", [MsgDataToBeSend]),
                            erlim_client:reply(Transport, Socket, MsgDataToBeSend, Protocol)
                    end,

                    %% 群聊离线消息
                    Roommsgs = mysql_util:user_roommsgs(Uid, 1),
                    case Roommsgs of
                        [] -> ok;
                        _ ->
                            RoommsgsForJson = lists:map(fun(R) ->
                                {datetime, CreatedAtD} = R#roommsg_record.created_at,
                                {datetime, UpdatedAtD} = R#roommsg_record.updated_at,
                                CreatedAtT = util:datetime2timestamp(CreatedAtD),
                                UpdatedAtT = util:datetime2timestamp(UpdatedAtD),
                                {[{id, R#roommsg_record.id}, {f, R#roommsg_record.f}, {t, R#roommsg_record.t}, {msg, R#roommsg_record.msg}, {created_at, CreatedAtT}, {updated_at, UpdatedAtT}]}
                                                        end, Roommsgs),
                            RoommsgsIds = lists:map(fun(R) ->
                                R#roommsg_record.id
                                                    end, Roommsgs),
                            RoomMsgDataToBeSend = jiffy:encode({[{<<"cmd">>, <<"offline_group_chat_msg">>}, {<<"msg">>, RoommsgsForJson}, {<<"ack">>, RoommsgsIds}]}),
                            lager:info("offline group chat msgs are ~p~n", [RoomMsgDataToBeSend]),
                            erlim_client:reply(Transport, Socket, RoomMsgDataToBeSend, Protocol)
                    end,

                    %% 离线通知
                    UnreadNotifications = mysql_util:user_unread_notifications(Uid),
                    case UnreadNotifications of
                        [] -> ok;
                        _ ->
                            UnreadNotificationsForJson = lists:map(fun(R) ->
                                {datetime, CreatedAtD} = R#notification_record.created_at,
                                {datetime, UpdatedAtD} = R#notification_record.updated_at,
                                CreatedAtT = util:datetime2timestamp(CreatedAtD),
                                UpdatedAtT = util:datetime2timestamp(UpdatedAtD),
                                {[{id, R#notification_record.id}, {sender_id, R#notification_record.sender_id}, {receiver_id, R#notification_record.receiver_id}, {notification_type, R#notification_record.notification_type}, {notifiable_type, R#notification_record.notifiable_type}, {notifiable_action, R#notification_record.notifiable_action}, {notifiable_id, R#notification_record.notifiable_id}, {subject, R#notification_record.subject}, {body, R#notification_record.body}, {unread, R#notification_record.unread}, {created_at, CreatedAtT}, {updated_at, UpdatedAtT}]}
                                                                   end, UnreadNotifications),
                            UnreadNotificationsIds = lists:map(fun(R) ->
                                R#notification_record.id
                                                               end, UnreadNotifications),
                            UnreadNotificationsToBeSend = jiffy:encode({[{<<"cmd">>, <<"offline_notifications">>}, {<<"msg">>, UnreadNotificationsForJson}, {<<"ack">>, UnreadNotificationsIds}]}),
                            lager:info("offline notifications are ~p~n", [UnreadNotificationsToBeSend]),
                            erlim_client:reply(Transport, Socket, UnreadNotificationsToBeSend, Protocol)
                    end,

                    State#state{client_pid = ClientPid, uid = Uid, device = Device, protocol = Protocol}
            end;
        true ->
            #state{uid = Uid, device = Device} = State,
            SessionUserMnesia = mnesia_util:query_session_by_uid_and_device(Uid, Device),
            case SessionUserMnesia of
                false ->
                    erlim_client:reply_error(Transport, Socket, <<"404 Not Found this user, please login">>, 10404, Protocol),
                    State;
                _ ->
                    FromUserMysql = mysql_util:query_user_by_id(SessionUserMnesia#session.uid),
                    case FromUserMysql of
                        [] ->
                            %% 可能是因为mysql数据库删除了
                            erlim_client:reply_error(Transport, Socket, <<"404 Not Found this user in mysql, please login again">>, 10404, Protocol),
                            State;
                        _ ->
                            case Cmd of
                                <<"single_chat">> ->
                                    %% 私聊
                                    ToUid = maps:get(<<"to">>, JsonMap),
                                    Msg = maps:get(<<"msg">>, JsonMap),
                                    Ack = maps:get(<<"ack">>, JsonMap),
                                    case is_integer(ToUid) of
                                        false ->
                                            erlim_client:reply_error(Transport, Socket, <<"Single chat send msg to user id must be integer">>, 10400, Protocol),
                                            State;
                                        true ->
                                            %% 是否好友关系
                                            case mysql_util:are_friends(SessionUserMnesia#session.uid, ToUid) of
                                                false ->
                                                    erlim_client:reply_error(Transport, Socket, <<"You are not friends">>, 10403, Protocol),
                                                    State;
                                                true ->
                                                    ToUserMysql = mysql_util:query_user_by_id(ToUid),
                                                    case ToUserMysql of
                                                        [] ->
                                                            erlim_client:reply_error(Transport, Socket, <<"404 Not Found this user in mysql">>, 10404, Protocol),
                                                            State;
                                                        _ ->
                                                            OffMsg = #msg_record{f = FromUserMysql#user_record.id, t = ToUserMysql#user_record.id, msg = Msg, unread = 1},
                                                            {ok_packet, _, _, MsgId, _, _, _} = mysql_util:save_msg(OffMsg),

                                                            %% Send single_chat ack to client
                                                            erlim_client:reply_ack(Transport, Socket, <<"single_chat">>, Ack, Protocol),

                                                            ToUsers = erlim_sm:get_session(ToUid),
                                                            lager:info("ToUsers is ~p~n", [ToUsers]),
                                                            case ToUsers of
                                                                false -> offline;
                                                                _ ->
                                                                    %% online: 发消息给多个终端设备
                                                                    lists:foreach(fun(U) ->
                                                                        DataToSend = jiffy:encode({[{<<"cmd">>, <<"single_chat">>}, {<<"from">>, SessionUserMnesia#session.uid}, {<<"msg">>, Msg}, {<<"ack">>, MsgId}]}),
                                                                        case node(U#session.pid) =:= node() of
                                                                            true ->
                                                                                U#session.pid ! {single_chat, DataToSend};
                                                                            false ->
                                                                                {U#session.register_name, U#session.node} ! {single_chat, DataToSend}
                                                                        end
                                                                                  end, ToUsers)
                                                            end,
                                                            State
                                                    end
                                            end
                                    end;
                                <<"group_chat">> ->
                                    %% 群聊
                                    ToRoomId = maps:get(<<"to">>, JsonMap),
                                    Msg = maps:get(<<"msg">>, JsonMap),
                                    Ack = maps:get(<<"ack">>, JsonMap),
                                    case is_integer(ToRoomId) of
                                        false ->
                                            erlim_client:reply_error(Transport, Socket, <<"Group chat send msg to room id must be integer">>, 10400, Protocol),
                                            State;
                                        true ->
                                            %% 群是否存在
                                            case mysql_util:is_an_exist_room(ToRoomId) of
                                                false ->
                                                    erlim_client:reply_error(Transport, Socket, <<"404 Not Found this room in mysql">>, 10404, Protocol),
                                                    State;
                                                true ->
                                                    %% 用户是否在该群里面
                                                    case mysql_util:in_room(SessionUserMnesia#session.uid, ToRoomId) of
                                                        false ->
                                                            erlim_client:reply_error(Transport, Socket, <<"You are not in this room">>, 10403, Protocol),
                                                            State;
                                                        true ->
                                                            RoomMsg = #roommsg_record{f = FromUserMysql#user_record.id, t = ToRoomId, msg = Msg},
                                                            {ok_packet, _, _, RoommsgId, _, _, _} = mysql_util:save_room_msg(RoomMsg),

                                                            Members = mysql_util:room_members(ToRoomId),
                                                            lager:info("Members are ~p.~n", [Members]),

                                                            %% Send group_chat ack to client
                                                            erlim_client:reply_ack(Transport, Socket, <<"group_chat">>, Ack, Protocol),

                                                            lists:foreach(fun(M) ->
                                                                case mysql_util:query_user_by_id(M#room_users_record.user_id) of
                                                                    [] -> false;
                                                                    #user_record{id = Id} ->
                                                                        %% Save members unread roommsg
                                                                        {ok_packet, _, _, UserRoommsgId, _, _, _} = mysql_util:save_user_room_msg(RoommsgId, Id),
                                                                        ToUsers = erlim_sm:get_session(Id),
                                                                        case ToUsers of
                                                                            false -> offline;
                                                                            _ ->
                                                                                %% online: 发消息给多个终端设备
                                                                                lists:foreach(fun(U) ->
                                                                                    DataToSend = jiffy:encode({[{<<"cmd">>, <<"group_chat">>}, {<<"from">>, SessionUserMnesia#session.uid}, {<<"to">>, ToRoomId}, {<<"msg">>, Msg}, {<<"ack">>, UserRoommsgId}]}),
                                                                                    {U#session.register_name, U#session.node} ! {group_chat, DataToSend}
                                                                                              end, ToUsers)
                                                                        end
                                                                end
                                                                          end, Members),
                                                            State
                                                    end
                                            end
                                    end;
                                <<"create_friendship">> ->
                                    %% 添加好友, 会发一个通知给对方, 对方同意, 才确认好友关系, 否则只是单方面的好友, 会存在于通讯录
                                    ToUid = maps:get(<<"to">>, JsonMap),
                                    Msg = maps:get(<<"msg">>, JsonMap),
                                    Ack = maps:get(<<"ack">>, JsonMap),
                                    case is_integer(ToUid) of
                                        false ->
                                            erlim_client:reply_error(Transport, Socket, <<"To user id must be integer">>, 10400, Protocol),
                                            State;
                                        true ->
                                            %% 好友请求消息回执
                                            erlim_client:reply_ack(Transport, Socket, <<"create_friendship">>, Ack, Protocol),
                                            Sender = SessionUserMnesia#session.uid,

                                            %% 添加记录前是否已经是好友了
                                            case mysql_util:are_friends(Sender, ToUid) of
                                                true ->
                                                    erlim_client:reply_error(Transport, Socket, <<"You are already friends">>, 10403, Protocol),
                                                    State;
                                                false ->
                                                    %% 添加记录后是否已经是好友了
                                                    case mysql_util:add_firend(Sender, ToUid, <<"">>) of
                                                        false ->
                                                            lager:info("You have already send a friendship request, please wait a moment."),
                                                            erlim_client:reply_error(Transport, Socket, <<"You have already send a friendship request.">>, 10403, Protocol);
                                                        _ ->
                                                            case mysql_util:are_friends(Sender, ToUid) of
                                                                true ->
                                                                    %% 发通知给双方, 他们已经成为了好友
                                                                    Subject = <<"You are friends">>,
                                                                    %% 发通知给对方
                                                                    NR = #notification_record{sender_id = Sender, receiver_id = ToUid, notification_type = 3, notifiable_type = <<"User">>, notifiable_action = <<"create_friendship">>, notifiable_id = ToUid, subject = Subject, body = Subject, unread = 1},
                                                                    {ok_packet, _, _, NotificationIdOfTo, _, _, _} = mysql_util:save_notification(NR),

                                                                    %% 对方是否在线
                                                                    send_notification(Sender, ToUid, 2, Subject, NotificationIdOfTo),

                                                                    %% 发通知给请求者
                                                                    NR1 = #notification_record{sender_id = ToUid, receiver_id = Sender, notification_type = 3, notifiable_type = <<"User">>, notifiable_action = <<"create_friendship">>, notifiable_id = Sender, subject = Subject, body = Subject, unread = 1},
                                                                    {ok_packet, _, _, NotificationIdOfMine, _, _, _} = mysql_util:save_notification(NR1),
                                                                    send_notification_to_self(Transport, 2, Subject, NotificationIdOfMine, Protocol, Socket),
                                                                    State;
                                                                false ->
                                                                    %% 添加记录后不是好友
                                                                    %% 对方是否在线
                                                                    NR = #notification_record{sender_id = Sender, receiver_id = ToUid, notification_type = 2, notifiable_type = <<"User">>, notifiable_action = <<"create_friendship">>, notifiable_id = ToUid, subject = Msg, body = Msg, unread = 1},
                                                                    lager:info("NR2 is ~p~n", [NR]),
                                                                    {ok_packet, _, _, NotificationIdOfRequest, _, _, _} = mysql_util:save_notification(NR),
                                                                    send_notification(Sender, ToUid, 2, Msg, NotificationIdOfRequest)
                                                            end
                                                    end
                                            end
                                    end;
                                <<"access_friendship">> ->
                                    %% 同意添加好友, 会发一个通知给对方
                                    %% @TODO
                                    State;
                                <<"deny_friendship">> ->
                                    %% 拒绝添加好友, 会发一个通知给对方
                                    %% @TODO
                                    State;
                                <<"del_friendship">> ->
                                    %% 移除好友, 无需对方同意, 直接移除, 但会发送通知
                                    ToUid = maps:get(<<"to">>, JsonMap),
                                    case is_integer(ToUid) of
                                        false ->
                                            lager:info("To user id must be integer"),
                                            erlim_client:reply_error(Transport, Socket, <<"To user id must be integer">>, 10400, Protocol),
                                            State;
                                        true ->
                                            Sender = SessionUserMnesia#session.uid,
                                            mysql_util:del_friend(Sender, ToUid),

                                            %% 需要消息回执
                                            MsgSender = <<"you del friends success">>,
                                            NRDelFriendshipSender = #notification_record{sender_id = 0, receiver_id = Sender, notification_type = 5, notifiable_type = <<"User">>, notifiable_action = <<"del_friendship">>, notifiable_id = Sender, subject = MsgSender, body = MsgSender, unread = 1},
                                            {ok_packet, _, _, NotificationIdOfMine, _, _, _} = mysql_util:save_notification(NRDelFriendshipSender),
                                            send_notification_to_self(Transport, 5, MsgSender, NotificationIdOfMine, Protocol, Socket),

                                            %% 需要消息回执
                                            Msg = <<"del our friendship">>,
                                            NRDelFriendship = #notification_record{sender_id = Sender, receiver_id = ToUid, notification_type = 5, notifiable_type = <<"User">>, notifiable_action = <<"del_friendship">>, notifiable_id = ToUid, subject = Msg, body = Msg, unread = 1},
                                            {ok_packet, _, _, NotificationIdOfTo, _, _, _} = mysql_util:save_notification(NRDelFriendship),
                                            send_notification(Sender, ToUid, 5, Msg, NotificationIdOfTo),
                                            State
                                    end;
                                <<"create_room">> ->
                                    %% 建群, 群主可设置是否需要密码等, 拉人, 无需对方同意, 有通知给对方
                                    RoomName = maps:get(<<"name">>, JsonMap),
                                    Invitable = maps:get(<<"invitable">>, JsonMap, 1),
                                    Members = maps:get(<<"members">>, JsonMap, []),
                                    Password = maps:get(<<"password">>, JsonMap, <<"">>),
                                    Description = maps:get(<<"description">>, JsonMap, <<"">>),
                                    Subject = maps:get(<<"subject">>, JsonMap, <<"">>),
                                    Logo = maps:get(<<"logo">>, JsonMap, <<"">>),

                                    Creator = SessionUserMnesia#session.uid,
                                    Room = #room_record{creator = Creator, name = RoomName, invitable = Invitable, password = Password, description = Description, subject = Subject, logo = Logo, max_member_count = 500},
                                    {ok_packet, _, _, RoomId, _, _, _} = mysql_util:create_room(Room),

                                    %% 加入群创建者
                                    mysql_util:add_member(RoomId, Creator),
                                    Msg = <<"create room success">>,
                                    NRRoom = #notification_record{sender_id = 0, receiver_id = Creator, notification_type = 11, notifiable_type = <<"User">>, notifiable_action = <<"create_room">>, notifiable_id = Creator, subject = Msg, body = Msg, unread = 1},
                                    {ok_packet, _, _, NotificationIdOfMine, _, _, _} = mysql_util:save_notification(NRRoom),
                                    send_notification_to_self(Transport, 11, Msg, NotificationIdOfMine, Protocol, Socket),

                                    %% 发消息给被拉入者
                                    lists:foreach(fun(Mid) ->
                                        mysql_util:add_member(RoomId, Mid),
                                        MsgPull = <<"pull you to this room">>,
                                        NRRoomPull = #notification_record{sender_id = Creator, receiver_id = Mid, notification_type = 11, notifiable_type = <<"User">>, notifiable_action = <<"create_room">>, notifiable_id = Mid, subject = MsgPull, body = MsgPull, unread = 1},
                                        {ok_packet, _, _, NotificationIdOfTo, _, _, _} = mysql_util:save_notification(NRRoomPull),
                                        send_notification(Creator, Mid, 11, MsgPull, NotificationIdOfTo)
                                                  end, Members),
                                    State;
                                <<"del_room">> ->
                                    %% 删除群(群主), 无需对方同意, 有通知给对方
                                    RoomId = maps:get(<<"roomid">>, JsonMap),
                                    lager:info("Room id is~p~n", [RoomId]),
                                    %% 群是否存在
                                    case mysql_util:is_an_exist_room(RoomId) of
                                        false ->
                                            erlim_client:reply_error(Transport, Socket, <<"room not exist">>, 10400, Protocol);
                                        true ->
                                            %% 是否群主
                                            Creator = SessionUserMnesia#session.uid,
                                            case mysql_util:is_room_leader(RoomId, Creator) of
                                                false ->
                                                    erlim_client:reply_error(Transport, Socket, <<"you are not the room leader">>, 10403, Protocol);
                                                true ->
                                                    %% 发送消息通知
                                                    lists:foreach(fun(M) ->
                                                        Mid = M#room_users_record.user_id,
                                                        case Mid =:= Creator of
                                                            true ->
                                                                Msg = <<"del room success">>,
                                                                NRRoom = #notification_record{sender_id = 0, receiver_id = Creator, notification_type = 12, notifiable_type = <<"User">>, notifiable_action = <<"create_room">>, notifiable_id = Creator, subject = Msg, body = Msg, unread = 1},
                                                                {ok_packet, _, _, NotificationIdOfMine, _, _, _} = mysql_util:save_notification(NRRoom),
                                                                send_notification_to_self(Transport, 12, Msg, NotificationIdOfMine, Protocol, Socket);
                                                            false ->
                                                                Msg = <<"deleted this room, you have to leave it">>,
                                                                NRRoom = #notification_record{sender_id = Creator, receiver_id = Mid, notification_type = 12, notifiable_type = <<"User">>, notifiable_action = <<"create_room">>, notifiable_id = Mid, subject = Msg, body = Msg, unread = 1},
                                                                {ok_packet, _, _, NotificationIdOfTo, _, _, _} = mysql_util:save_notification(NRRoom),
                                                                send_notification(Creator, Mid, 12, Msg, NotificationIdOfTo)
                                                        end
                                                                  end, mysql_util:room_members(RoomId)),
                                                    %% 最后才删除群
                                                    mysql_util:del_room(RoomId)
                                            end
                                    end,
                                    State;
                                <<"join_room">> ->
                                    %% 加群
                                    %% 1. 无密码, 自己申请进群
                                    %% 2. 有密码, 自己申请进群, 需要输入密码
                                    %% add room member count limit
                                    %% 是否群成员
                                    RoomId = maps:get(<<"roomid">>, JsonMap),
                                    lager:info("Room id is~p~n", [RoomId]),
                                    %% 群是否存在
                                    case mysql_util:is_an_exist_room(RoomId) of
                                        false ->
                                            erlim_client:reply_error(Transport, Socket, <<"room not exist">>, 10400, Protocol);
                                        true ->
                                            %% @TODO
                                            ok
                                    end,
                                    State;
                                <<"pull_to_room">> ->
                                    %% 其他人拉某人进群, 无需密码
                                    %% add room member count limit
                                    %% 是否群成员
                                    RoomId = maps:get(<<"roomid">>, JsonMap),
                                    UserId = maps:get(<<"userid">>, JsonMap),
                                    lager:info("Room id is ~p userid is ~p~n", [RoomId, UserId]),
                                    Puller = SessionUserMnesia#session.uid,
                                    %% 群是否存在
                                    case mysql_util:is_an_exist_room(RoomId) of
                                        false ->
                                            erlim_client:reply_error(Transport, Socket, <<"room not exist">>, 10400, Protocol);
                                        true ->
                                            %% 邀请人是否群成员
                                            case mysql_util:in_room(Puller, RoomId) of
                                                false ->
                                                    erlim_client:reply_error(Transport, Socket, <<"You are not the room member">>, 10403, Protocol);
                                                true ->
                                                    RoomMembersCount = mysql_util:room_members_count(RoomId),
                                                    RoomRecordMysql = mysql_util:room(RoomId),
                                                    MaxMembersCount = RoomRecordMysql#room_record.max_member_count,
                                                    %% 群成员数限制
                                                    case MaxMembersCount > RoomMembersCount of
                                                        true ->
                                                            %% 是否群成员
                                                            case mysql_util:in_room(UserId, RoomId) of
                                                                false ->
                                                                    mysql_util:add_member(RoomId, UserId),

                                                                    %% 发送消息通知
                                                                    lists:foreach(fun(M) ->
                                                                        Mid = M#room_users_record.user_id,
                                                                        if
                                                                            Mid =:= Puller ->
                                                                                Msg = <<"pull someone to room success">>,
                                                                                NRRoom = #notification_record{sender_id = 0, receiver_id = Puller, notification_type = 15, notifiable_type = <<"User">>, notifiable_action = <<"pull_to_room">>, notifiable_id = Puller, subject = Msg, body = Msg, unread = 1},
                                                                                {ok_packet, _, _, NotificationIdOfMine, _, _, _} = mysql_util:save_notification(NRRoom),
                                                                                send_notification_to_self(Transport, 15, Msg, NotificationIdOfMine, Protocol, Socket);
                                                                            true ->
                                                                                Msg = case Mid =:= UserId of
                                                                                          true ->
                                                                                              <<"invited you to this room">>;
                                                                                          false ->
                                                                                              <<"some one invited some one to this room">>
                                                                                      end,
                                                                                NRRoom = #notification_record{sender_id = Puller, receiver_id = Mid, notification_type = 15, notifiable_type = <<"User">>, notifiable_action = <<"pull_to_room">>, notifiable_id = Mid, subject = Msg, body = Msg, unread = 1},
                                                                                {ok_packet, _, _, NotificationIdOfTo, _, _, _} = mysql_util:save_notification(NRRoom),
                                                                                send_notification(Puller, Mid, 15, Msg, NotificationIdOfTo)
                                                                        end
                                                                                  end, mysql_util:room_members(RoomId));
                                                                true ->
                                                                    erlim_client:reply_error(Transport, Socket, <<"User is already room member">>, 10403, Protocol)
                                                            end;
                                                        false ->
                                                            erlim_client:reply_error(Transport, Socket, <<"Room members limit">>, 10403, Protocol)
                                                    end
                                            end
                                    end,
                                    State;
                                <<"leave_room">> ->
                                    %% 退群, 群主退群则下一个加入的人自动成为群主, 通知所有群成员
                                    RoomId = maps:get(<<"roomid">>, JsonMap),
                                    lager:info("Room id is~p~n", [RoomId]),
                                    %% 群是否存在
                                    case mysql_util:is_an_exist_room(RoomId) of
                                        false ->
                                            erlim_client:reply_error(Transport, Socket, <<"room not exist">>, 10400, Protocol);
                                        true ->
                                            Cid = SessionUserMnesia#session.uid,
                                            %% 发送消息通知
                                            lists:foreach(fun(M) ->
                                                Mid = M#room_users_record.user_id,
                                                case Mid =:= Cid of
                                                    true ->
                                                        Msg = <<"leave room success">>,
                                                        NRRoom = #notification_record{sender_id = 0, receiver_id = Cid, notification_type = 13, notifiable_type = <<"User">>, notifiable_action = <<"leave_room">>, notifiable_id = Cid, subject = Msg, body = Msg, unread = 1},
                                                        {ok_packet, _, _, NotificationIdOfMine, _, _, _} = mysql_util:save_notification(NRRoom),
                                                        send_notification_to_self(Transport, 13, Msg, NotificationIdOfMine, Protocol, Socket);
                                                    false ->
                                                        Msg = <<"deleted this room, you have to leave it">>,
                                                        NRRoom = #notification_record{sender_id = Cid, receiver_id = Mid, notification_type = 13, notifiable_type = <<"User">>, notifiable_action = <<"leave_room">>, notifiable_id = Mid, subject = Msg, body = Msg, unread = 1},
                                                        {ok_packet, _, _, NotificationIdOfTo, _, _, _} = mysql_util:save_notification(NRRoom),
                                                        send_notification(Cid, Mid, 13, Msg, NotificationIdOfTo)
                                                end
                                                          end, mysql_util:room_members(RoomId)),
                                            %% 是否群主
                                            case mysql_util:is_room_leader(RoomId, Cid) of
                                                false ->
                                                    %% 退群
                                                    mysql_util:del_room_member(RoomId, Cid),

                                                    %% 退群以后是否为空群
                                                    case mysql_util:room_members(RoomId) of
                                                        [] -> mysql_util:del_room(RoomId);
                                                        _ -> ok
                                                    end;
                                                true ->
                                                    %% 退群
                                                    mysql_util:del_room_member(RoomId, Cid),
                                                    %% 修改群主
                                                    mysql_util:change_room_leader(RoomId)
                                            end
                                    end,
                                    State;
                                <<"change_room_info">> ->
                                    %% 修改群信息(暂时不限制), 通知所有群成员
                                    RoomId = maps:get(<<"roomid">>, JsonMap),
                                    NewName = maps:get(<<"newname">>, JsonMap),
                                    lager:info("Room id is~p~n", [RoomId]),
                                    lager:info("NewName is~p~n", [NewName]),
                                    %% 群是否存在
                                    case mysql_util:is_an_exist_room(RoomId) of
                                        false ->
                                            erlim_client:reply_error(Transport, Socket, <<"room not exist">>, 10400, Protocol);
                                        true ->
                                            Cid = SessionUserMnesia#session.uid,
                                            %% 发送消息通知
                                            lists:foreach(fun(M) ->
                                                Mid = M#room_users_record.user_id,
                                                case Mid =:= Cid of
                                                    true ->
                                                        Msg = <<"change room name success">>,
                                                        NRRoom = #notification_record{sender_id = 0, receiver_id = Cid, notification_type = 14, notifiable_type = <<"User">>, notifiable_action = <<"change_room_info">>, notifiable_id = Cid, subject = Msg, body = Msg, unread = 1},
                                                        {ok_packet, _, _, NotificationIdOfMine, _, _, _} = mysql_util:save_notification(NRRoom),
                                                        send_notification_to_self(Transport, 14, Msg, NotificationIdOfMine, Protocol, Socket);
                                                    false ->
                                                        Msg = <<"changed the room name">>,
                                                        NRRoom = #notification_record{sender_id = Cid, receiver_id = Mid, notification_type = 14, notifiable_type = <<"User">>, notifiable_action = <<"change_room_info">>, notifiable_id = Mid, subject = Msg, body = Msg, unread = 1},
                                                        {ok_packet, _, _, NotificationIdOfTo, _, _, _} = mysql_util:save_notification(NRRoom),
                                                        send_notification(Cid, Mid, 14, Msg, NotificationIdOfTo)
                                                end
                                                          end, mysql_util:room_members(RoomId)),
                                            mysql_util:change_room_name(RoomId, NewName)
                                    end,
                                    State;
                                <<"ack">> ->
                                    %% 消息回执
                                    Action = maps:get(<<"action">>, JsonMap),
                                    Ack = maps:get(<<"ack">>, JsonMap),
                                    lager:info("Action is ~p, Ack is ~p~n", [Action, Ack]),
                                    case Action of
                                        <<"single_chat">> ->
                                            lager:info("Single chat msg ack is ~p~n", [Ack]),
                                            mysql_util:mark_read(Ack, single_chat);
                                        <<"group_chat">> ->
                                            lager:info("Group chat msg ack is ~p~n", [Ack]),
                                            mysql_util:mark_read(Ack, group_chat);
                                        <<"offline_single_chat_msg">> ->
                                            lager:info("Single chat offline msg ack is ~p~n", [Ack]),
                                            lists:foreach(fun(MsgId) ->
                                                mysql_util:mark_read(MsgId, single_chat)
                                                          end, Ack);
                                        <<"offline_group_chat_msg">> ->
                                            lager:info("Group chat offline msg ack is ~p~n", [Ack]),
                                            lists:foreach(fun(RoomMsgId) ->
                                                mysql_util:mark_read(RoomMsgId, Uid, group_chat)
                                                          end, Ack);
                                        <<"offline_notifications">> ->
                                            lager:info("offline_notifications acks are ~p~n", [Ack]),
                                            lists:foreach(fun(Nid) ->
                                                mysql_util:mark_read(Nid, notification)
                                                          end, Ack);
                                        _ ->
                                            erlim_client:reply_error(Transport, Socket, <<"404 Not Found this ack action">>, 10404, Protocol)
                                    end,
                                    State;
                                <<"logout">> ->
                                    %% 手动退出登陆
                                    self() ! {tcp_closed, Socket},
                                    State;
                                _ ->
                                    %% @TODO webrtc signaling server
                                    case Protocol of
                                        websocket ->
                                            case Cmd of
                                                <<"webrtc_create">> ->
                                                    ToUid = maps:get(<<"to">>, JsonMap),
                                                    RoomName = maps:get(<<"name">>, JsonMap),
                                                    %% 对方是否在线
                                                    case mnesia_util:query_session_by_uid(ToUid) of
                                                        false ->
                                                            erlim_client:reply_error(Transport, Socket, <<"User is not online.">>, 10403, Protocol),
                                                            State;
                                                        ToUsers ->
                                                            Uuid = util:uuid(),
                                                            webrtc_room:create(Uuid, RoomName),
                                                            webrtc_room:join(Uuid, SessionUserMnesia#session.pid, "anymous"),
                                                            %% 发送视频通讯请求给用户,但是此处发给多个终端还是一个, join就表示了ack
                                                            DataToSend = jiffy:encode({[{<<"cmd">>, <<"webrtc_create">>}, {<<"from">>, SessionUserMnesia#session.uid}, {<<"room_uuid">>, Uuid}]}),
                                                            lists:foreach(fun(U) ->
                                                                {U#session.register_name, U#session.node} ! {webrtc_create, DataToSend}
                                                                          end, ToUsers),
                                                            %% 发送房间信息给请求者
                                                            erlim_client:reply(Transport, Socket, DataToSend, Protocol),
                                                            State
                                                    end;
                                                <<"webrtc_join">> ->
                                                    ToRoomUuid = maps:get(<<"to">>, JsonMap),
                                                    case webrtc_room:get_members(ToRoomUuid) of
                                                        false ->
                                                            erlim_client:reply_error(Transport, Socket, <<"Invide room.">>, 10404, Protocol);
                                                        Members ->
                                                            case length(Members) of
                                                                1 ->
                                                                    {atomic, ok} = webrtc_room:join(ToRoomUuid, SessionUserMnesia#session.pid, "anymous"),
                                                                    DataToSend = jiffy:encode({[{<<"cmd">>, <<"webrtc_join">>}, {<<"from">>, SessionUserMnesia#session.uid}, {<<"to">>, ToRoomUuid}]}),
                                                                    lists:foreach(fun(U) ->
                                                                        case mnesia_util:query_session_by_pid(U#webrtc_members.pid) of
                                                                            false -> ok;
                                                                            ToU ->
                                                                                {ToU#session.register_name, ToU#session.node} ! {webrtc_join, DataToSend}
                                                                        end
                                                                                  end, Members),
                                                                    %% 发送加入信息给请求者
                                                                    erlim_client:reply(Transport, Socket, DataToSend, Protocol),
                                                                    State;
                                                                _ ->
                                                                    erlim_client:reply_error(Transport, Socket, <<"Invide room.">>, 10404, Protocol),
                                                                    State
                                                            end
                                                    end;
                                                <<"webrtc_leave">> ->
                                                    ToRoomUuid = maps:get(<<"to">>, JsonMap),
                                                    case webrtc_room:get_members(ToRoomUuid) of
                                                        false ->
                                                            erlim_client:reply_error(Transport, Socket, <<"Invide room.">>, 10404, Protocol);
                                                        Members ->
                                                            case length(Members) of
                                                                0 -> webrtc_room:delete(ToRoomUuid);
                                                                1 ->
                                                                    {atomic, ok} = webrtc_room:leave(ToRoomUuid, SessionUserMnesia#session.pid),
                                                                    webrtc_room:delete(ToRoomUuid),
                                                                    DataToSend = jiffy:encode({[{<<"cmd">>, <<"webrtc_leave">>}, {<<"from">>, SessionUserMnesia#session.uid}, {<<"to">>, ToRoomUuid}]}),
                                                                    %% 发送离开信息给请求者
                                                                    erlim_client:reply(Transport, Socket, DataToSend, Protocol),
                                                                    State;
                                                                _ ->
                                                                    {atomic, ok} = webrtc_room:leave(ToRoomUuid, SessionUserMnesia#session.pid),
                                                                    DataToSend = jiffy:encode({[{<<"cmd">>, <<"webrtc_leave">>}, {<<"from">>, SessionUserMnesia#session.uid}, {<<"to">>, ToRoomUuid}]}),

                                                                    lists:foreach(fun(U) ->
                                                                        case mnesia_util:query_session_by_pid(U#webrtc_members.pid) of
                                                                            false -> ok;
                                                                            ToU ->
                                                                                {ToU#session.register_name, ToU#session.node} ! {webrtc_leave, DataToSend}
                                                                        end
                                                                                  end, webrtc_room:get_members(ToRoomUuid)),
                                                                    %% 发送离开信息给请求者
                                                                    erlim_client:reply(Transport, Socket, DataToSend, Protocol),
                                                                    State
                                                            end
                                                    end;
                                                <<"webrtc_send_offer">> ->
                                                    ToUid = maps:get(<<"to">>, JsonMap),
                                                    Sdp = maps:get(<<"sdp">>, JsonMap),
                                                    io:format("Sdp is ~p~n", [Sdp]),
                                                    %% 对方是否在线
                                                    case mnesia_util:query_session_by_uid(ToUid) of
                                                        false ->
                                                            erlim_client:reply_error(Transport, Socket, <<"User is not online.">>, 10403, Protocol),
                                                            State;
                                                        ToUsers ->
                                                            %% 发送offer给用户,但是此处发给多个终端还是一个
                                                            lists:foreach(fun(U) ->
                                                                DataToSend = jiffy:encode({[{<<"cmd">>, <<"webrtc_send_offer">>}, {<<"from">>, SessionUserMnesia#session.uid}, {<<"sdp">>, Sdp}]}),
                                                                {U#session.register_name, U#session.node} ! {webrtc_send_offer, DataToSend}
                                                                          end, ToUsers),
                                                            State
                                                    end;
                                                <<"webrtc_send_answer">> ->
                                                    ToUid = maps:get(<<"to">>, JsonMap),
                                                    Sdp = maps:get(<<"sdp">>, JsonMap),
                                                    io:format("Sdp is ~p~n", [Sdp]),
                                                    %% 对方是否在线
                                                    case mnesia_util:query_session_by_uid(ToUid) of
                                                        false ->
                                                            erlim_client:reply_error(Transport, Socket, <<"User is not online.">>, 10403, Protocol),
                                                            State;
                                                        ToUsers ->
                                                            %% 发送answer给用户,但是此处发给多个终端还是一个
                                                            lists:foreach(fun(U) ->
                                                                DataToSend = jiffy:encode({[{<<"cmd">>, <<"webrtc_send_answer">>}, {<<"from">>, SessionUserMnesia#session.uid}, {<<"sdp">>, Sdp}]}),
                                                                {U#session.register_name, U#session.node} ! {webrtc_send_answer, DataToSend}
                                                                          end, ToUsers),
                                                            State
                                                    end;
                                                <<"webrtc_send_ice_candidate">> ->
                                                    ToUid = maps:get(<<"to">>, JsonMap),
                                                    Label = maps:get(<<"label">>, JsonMap),
                                                    Candidate = maps:get(<<"candidate">>, JsonMap),
                                                    io:format("Candidate is ~p, label is ~p~n", [Candidate, Label]),
                                                    %% 对方是否在线
                                                    case mnesia_util:query_session_by_uid(ToUid) of
                                                        false ->
                                                            erlim_client:reply_error(Transport, Socket, <<"User is not online.">>, 10403, Protocol),
                                                            State;
                                                        ToUsers ->
                                                            %% 发送ice_candidate给用户,但是此处发给多个终端还是一个
                                                            lists:foreach(fun(U) ->
                                                                DataToSend = jiffy:encode({[{<<"cmd">>, <<"webrtc_send_ice_candidate">>}, {<<"from">>, SessionUserMnesia#session.uid}, {<<"label">>, Label}, {<<"candidate">>, Candidate}]}),
                                                                {U#session.register_name, U#session.node} ! {webrtc_send_ice_candidate, DataToSend}
                                                                          end, ToUsers),
                                                            State
                                                    end;
                                                _ ->
                                                    %% 用户登陆后发送了未知命令
                                                    erlim_client:reply_error(Transport, Socket, <<"Unknown cmd">>, 10400, Protocol),
                                                    State
                                            end;
                                        _ ->
                                            %% 用户登陆后发送了未知命令
                                            erlim_client:reply_error(Transport, Socket, <<"Unknown cmd">>, 10400, Protocol),
                                            State
                                    end
                            end
                    end
            end
    end.


%% @doc send notification to others
send_notification(Sender, ToUid, TypeInteger, Msg, Ack) ->
    ToUsers = erlim_sm:get_session(ToUid),
    case ToUsers of
        false -> offline;
        _ ->
            %% online: 发消息给多个终端设备
            lists:foreach(fun(U) ->
                DataToSend = jiffy:encode({[{<<"cmd">>, <<"notification">>}, {<<"notification_type">>, TypeInteger}, {<<"from">>, Sender}, {<<"msg">>, Msg}, {<<"ack">>, Ack}]}),
                case node(U#session.pid) =:= node() of
                    true ->
                        U#session.pid ! {notification, DataToSend};
                    false ->
                        {U#session.register_name, U#session.node} ! {notification, DataToSend}
                end
                          end, ToUsers)
    end.

%% @doc send notification to self
send_notification_to_self(Transport, TypeInteger, Msg, Ack, Protocol, Socket) ->
    DataToSender = jiffy:encode({[{<<"cmd">>, <<"notification">>}, {<<"notification_type">>, TypeInteger}, {<<"from">>, 0}, {<<"msg">>, Msg}, {<<"ack">>, Ack}]}),
    erlim_client:reply(Transport, Socket, DataToSender, Protocol).
