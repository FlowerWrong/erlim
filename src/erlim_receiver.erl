-module(erlim_receiver).

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
    socket,
    protocol,
    heartbeat_timeout = ?HIBERNATE_TIMEOUT,
    client_pid,
    ip,
    uid,
    device,
    node,
    data_complete = 0,  %% 0: 开始接收 1: 接收剩余
    client_data = [],
    payload_len,
    already_receive_payload_len = 0
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
start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).


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
init([Socket]) ->
    {ok, {IP, _Port}} = inet:peername(Socket),
    NewState = #state{socket = Socket, ip = IP, node = node()},
    setopts(NewState#state.socket),
    {ok, NewState}.

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
handle_info({tcp, Socket, Data}, #state{socket = Socket, data_complete = DCFlag, client_data = ClientData, payload_len = PayloadLen, already_receive_payload_len = AlreadyReceivePayloadLen, protocol = P} = State) ->
    setopts(Socket),
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
                                                   true -> process_data(WsData, Socket, State, websocket);
                                                   false ->
                                                       erlim_client:reply_error(Socket, <<"invide json">>, 10400, websocket),
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
                                                       gen_tcp:send(Socket, WebSocketDataToBeSend),
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
                               DataList = string:tokens(binary_to_list(Data), "\r\n"),
                               PayloadLength0 = string:tokens(lists:nth(2, DataList), ": "),
                               PayloadLength1 = lists:nth(2, PayloadLength0),
                               PayloadLength = list_to_integer(PayloadLength1),
                               if
                                   PayloadLength > 1048576 ->
                                       erlim_client:reply_error(Socket, <<"data must less than 8192 bytes">>, 10400, tcp),
                                       State;
                                   true ->
                                       PayloadList = lists:nth(3, DataList),
                                       PayloadBinary = list_to_binary(PayloadList),
                                       %% 已经接收到的数据大小
                                       Alrpl = byte_size(PayloadBinary),
                                       case Alrpl =:= PayloadLength of
                                           false ->
                                               NewClientData = [PayloadBinary | ClientData],
                                               State#state{data_complete = 1, client_data = NewClientData, payload_len = PayloadLength, protocol = tcp, already_receive_payload_len = Alrpl};
                                           true ->
                                               S = State#state{data_complete = 0, client_data = [], payload_len = undefined, protocol = tcp, already_receive_payload_len = 0},
                                               process_data(PayloadBinary, Socket, S, tcp)
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
                                   true -> process_data(PayloadOver, Socket, S, P);
                                   false ->
                                       erlim_client:reply_error(Socket, <<"invide json">>, 10400, P),
                                       S
                               end
                       end
               end,
    {noreply, NewState, NewState#state.heartbeat_timeout};
% tcp connection change to passive
handle_info({tcp_passive, Socket}, #state{socket = Socket} = State) ->
    lager:info("tcp_passive is ~p~n", [State]),
    {noreply, State};
% connection closed
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(timeout, State) ->
    proc_lib:hibernate(gen_server, enter_loop, [?MODULE, [], State]),
    {noreply, State, State#state.heartbeat_timeout};
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

setopts(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    inet:peername(Socket).

process_data(Data, Socket, State, Protocol) ->
    Json = jiffy:decode(Data),
    lager:info("Json is ~p.~n", [Json]),
    {[{<<"cmd">>, Cmd} | T]} = Json,
    if
        Cmd =:= <<"login">> ->
            [{<<"name">>, Name}, {<<"pass">>, Pass}, {<<"ack">>, Ack}, {<<"device">>, Device}] = T,
            LoginUserMysql = mysql_util:query_user_by_mobile(Name),
            case LoginUserMysql of
                [] ->
                    erlim_client:reply_error(Socket, <<"404 Not Found user with this name">>, 10404, Protocol),
                    State;
                #user_record{password_digest = PD, id = Uid} ->
                    PassDigest = binary_to_list(PD),
                    {ok, PassDigest} =:= bcrypt:hashpw(Pass, PassDigest),
                    ClientPid = erlim_client_sup:start_child(Socket, Protocol),
                    erlim_sm:login(Uid, ClientPid, Device),
                    %% Send login ack to client
                    erlim_client:reply_ack(Socket, <<"login">>, Ack, Protocol),

                    %% 登陆成功后推送离线消息
                    %% single chat offline msg
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
                            erlim_client:reply(Socket, MsgDataToBeSend, Protocol)
                    end,

                    %% group chat offline msg
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
                            erlim_client:reply(Socket, RoomMsgDataToBeSend, Protocol)
                    end,

                    State#state{client_pid = ClientPid, uid = Uid, device = Device, protocol = Protocol}
            end;
        true ->
            #state{uid = Uid, device = Device} = State,
            SessionUserMnesia = mnesia_util:query_session_by_uid_and_device(Uid, Device),
            case SessionUserMnesia of
                false ->
                    erlim_client:reply_error(Socket, <<"404 Not Found this user, please login">>, 10404, Protocol),
                    State;
                _ ->
                    FromUserMysql = mysql_util:query_user_by_id(SessionUserMnesia#session.uid),
                    case FromUserMysql of
                        [] ->
                            %% 可能是因为mysql数据库删除了
                            erlim_client:reply_error(Socket, <<"404 Not Found this user in mysql, please login again">>, 10404, Protocol),
                            State;
                        _ ->
                            case Cmd of
                                <<"single_chat">> ->
                                    [{<<"to">>, ToUid}, {<<"msg">>, Msg}, {<<"ack">>, Ack}] = T,
                                    case is_integer(ToUid) of
                                        false ->
                                            erlim_client:reply_error(Socket, <<"Single chat send msg to user id must be integer">>, 10400, Protocol),
                                            State;
                                        true ->
                                            %% 是否好友关系
                                            case mysql_util:are_friends(SessionUserMnesia#session.uid, ToUid) of
                                                false ->
                                                    erlim_client:reply_error(Socket, <<"You are not friends">>, 10403, Protocol),
                                                    State;
                                                true ->
                                                    ToUserMysql = mysql_util:query_user_by_id(ToUid),
                                                    case ToUserMysql of
                                                        [] ->
                                                            erlim_client:reply_error(Socket, <<"404 Not Found this user in mysql">>, 10404, Protocol),
                                                            State;
                                                        _ ->
                                                            OffMsg = #msg_record{f = FromUserMysql#user_record.id, t = ToUserMysql#user_record.id, msg = Msg, unread = 1},
                                                            {ok_packet, _, _, MsgId, _, _, _} = mysql_util:save_msg(OffMsg),

                                                            %% Send single_chat ack to client
                                                            erlim_client:reply_ack(Socket, <<"single_chat">>, Ack, Protocol),

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
                                    [{<<"to">>, ToRoomId}, {<<"msg">>, Msg}, {<<"ack">>, Ack}] = T,
                                    case is_integer(ToRoomId) of
                                        false ->
                                            erlim_client:reply_error(Socket, <<"Group chat send msg to room id must be integer">>, 10400, Protocol),
                                            State;
                                        true ->
                                            %% 群是否存在
                                            case mysql_util:is_an_exist_room(ToRoomId) of
                                                false ->
                                                    erlim_client:reply_error(Socket, <<"404 Not Found this room in mysql">>, 10404, Protocol),
                                                    State;
                                                true ->
                                                    %% 用户是否在该群里面
                                                    case mysql_util:in_room(SessionUserMnesia#session.uid, ToRoomId) of
                                                        false ->
                                                            erlim_client:reply_error(Socket, <<"You are not in this room">>, 10403, Protocol),
                                                            State;
                                                        true ->
                                                            RoomMsg = #roommsg_record{f = FromUserMysql#user_record.id, t = ToRoomId, msg = Msg},
                                                            {ok_packet, _, _, RoommsgId, _, _, _} = mysql_util:save_room_msg(RoomMsg),

                                                            Members = mysql_util:room_members(ToRoomId),
                                                            lager:info("Members are ~p.~n", [Members]),

                                                            %% Send group_chat ack to client
                                                            erlim_client:reply_ack(Socket, <<"group_chat">>, Ack, Protocol),

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
                                <<"ack">> ->
                                    [{<<"action">>, Action}, {<<"ack">>, Ack}] = T,
                                    lager:info("Action is ~p, Timestamp is ~p~n", [Action, Ack]),
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
                                        _ ->
                                            erlim_client:reply_error(Socket, <<"404 Not Found this ack action">>, 10404, Protocol)
                                    end,
                                    State;
                                <<"logout">> ->
                                    self() ! {tcp_closed, Socket},
                                    State;
                                _ ->
                                    %% FIXME webrtc signaling server
                                    case Protocol of
                                        websocket ->
                                            case Cmd of
                                                <<"webrtc_create">> ->
                                                    [{<<"to">>, ToUid}, {<<"name">>, RoomName}] = T,
                                                    %% 对方是否在线
                                                    case mnesia_util:query_session_by_uid(ToUid) of
                                                        false ->
                                                            erlim_client:reply_error(Socket, <<"User is not online.">>, 10403, Protocol),
                                                            State;
                                                        ToUsers ->
                                                            Uuid = util:uuid(),
                                                            webrtc_room:create(Uuid, RoomName),
                                                            webrtc_room:join(Uuid, SessionUserMnesia#session.pid, ""),
                                                            %% 发送视频通讯请求给用户,但是此处发给多个终端还是一个, join就表示了ack
                                                            DataToSend = jiffy:encode({[{<<"cmd">>, <<"webrtc_create">>}, {<<"from">>, SessionUserMnesia#session.uid}, {<<"room_uuid">>, Uuid}]}),
                                                            lists:foreach(fun(U) ->
                                                                {U#session.register_name, U#session.node} ! {webrtc_create, DataToSend}
                                                            end, ToUsers),
                                                            %% 发送房间信息给请求者
                                                            erlim_client:reply(Socket, DataToSend, Protocol),
                                                            State
                                                    end;
                                                <<"webrtc_join">> ->
                                                    [{<<"to">>, ToRoomUuid}] = T,
                                                    case webrtc_room:get_members(ToRoomUuid) of
                                                        false ->
                                                            erlim_client:reply_error(Socket, <<"Invide room.">>, 10404, Protocol);
                                                        Members ->
                                                            case length(Members) of
                                                                1 ->
                                                                    {atomic, ok} = webrtc_room:join(ToRoomUuid, SessionUserMnesia#session.pid, ""),
                                                                    DataToSend = jiffy:encode({[{<<"cmd">>, <<"webrtc_join">>}, {<<"from">>, SessionUserMnesia#session.uid}, {<<"to">>, ToRoomUuid}]}),
                                                                    lists:foreach(fun(U) ->
                                                                        case mnesia_util:query_session_by_pid(U#webrtc_members.pid) of
                                                                            false -> ok;
                                                                            ToU ->
                                                                                {ToU#session.register_name, ToU#session.node} ! {webrtc_join, DataToSend}
                                                                        end
                                                                    end, Members),
                                                                    %% 发送加入信息给请求者
                                                                    erlim_client:reply(Socket, DataToSend, Protocol),
                                                                    State;
                                                                _ ->
                                                                    erlim_client:reply_error(Socket, <<"Invide room.">>, 10404, Protocol),
                                                                    State
                                                            end
                                                    end;
                                                <<"webrtc_leave">> ->
                                                    %% FIXME
                                                    [{<<"to">>, ToRoomid}, {<<"ack">>, Ack}] = T,
                                                    Room = integer_to_binary(ToRoomid),
                                                    case pg2:get_members(Room) of
                                                        {error, _} ->
                                                            erlim_client:reply_error(Socket, <<"Invide room.">>, 10404, Protocol);
                                                        Members ->
                                                            case length(Members) of
                                                                0 ->
                                                                    pg2:delete(Room);
                                                                1 ->
                                                                    ok = pg2:leave(Room, self()),
                                                                    pg2:delete(Room),
                                                                    erlim_client:reply_ack(Socket, <<"webrtc_leave">>, Ack, Protocol),
                                                                    State;
                                                                _ ->
                                                                    ok = pg2:leave(Room, self()),
                                                                    DataToSend = jiffy:encode({[{<<"cmd">>, <<"webrtc_leave">>}, {<<"from">>, SessionUserMnesia#session.uid}, {<<"to">>, ToRoomid}, {<<"ack">>, Ack}]}),
                                                                    ws_util:relay_message(DataToSend, Room),
                                                                    erlim_client:reply_ack(Socket, <<"webrtc_leave">>, Ack, Protocol),
                                                                    State
                                                            end
                                                    end;
                                                <<"webrtc_send_offer">> ->
                                                    [{<<"to">>, ToUid}, {<<"sdp">>, Sdp}, {<<"ack">>, _Ack}] = T,
                                                    io:format("Sdp is ~p~n", [Sdp]),
                                                    %% 对方是否在线
                                                    case mnesia_util:query_session_by_uid(ToUid) of
                                                        false ->
                                                            erlim_client:reply_error(Socket, <<"User is not online.">>, 10403, Protocol),
                                                            State;
                                                        ToUsers ->
                                                            %% 发送offer给用户,但是此处发给多个终端还是一个
                                                            lists:foreach(fun(U) ->
                                                                DataToSend = jiffy:encode({[{<<"cmd">>, <<"webrtc_send_offer">>}, {<<"from">>, SessionUserMnesia#session.uid}, {<<"sdp">>, Sdp}, {<<"ack">>, util:uuid()}]}),
                                                                {U#session.register_name, U#session.node} ! {webrtc_send_offer, DataToSend}
                                                            end, ToUsers),
                                                            State
                                                    end;
                                                <<"webrtc_send_answer">> ->
                                                    [{<<"to">>, ToUid}, {<<"sdp">>, Sdp}, {<<"ack">>, _Ack}] = T,
                                                    io:format("Sdp is ~p~n", [Sdp]),
                                                    %% 对方是否在线
                                                    case mnesia_util:query_session_by_uid(ToUid) of
                                                        false ->
                                                            erlim_client:reply_error(Socket, <<"User is not online.">>, 10403, Protocol),
                                                            State;
                                                        ToUsers ->
                                                            %% 发送answer给用户,但是此处发给多个终端还是一个
                                                            lists:foreach(fun(U) ->
                                                                DataToSend = jiffy:encode({[{<<"cmd">>, <<"webrtc_send_answer">>}, {<<"from">>, SessionUserMnesia#session.uid}, {<<"sdp">>, Sdp}, {<<"ack">>, util:uuid()}]}),
                                                                {U#session.register_name, U#session.node} ! {webrtc_send_answer, DataToSend}
                                                            end, ToUsers),
                                                            State
                                                    end;
                                                <<"webrtc_send_ice_candidate">> ->
                                                    [{<<"to">>, ToUid}, {<<"label">>, Label}, {<<"candidate">>, Candidate}, {<<"ack">>, _Ack}] = T,
                                                    io:format("Candidate is ~p, label is ~p~n", [Candidate, Label]),
                                                    %% 对方是否在线
                                                    case mnesia_util:query_session_by_uid(ToUid) of
                                                        false ->
                                                            erlim_client:reply_error(Socket, <<"User is not online.">>, 10403, Protocol),
                                                            State;
                                                        ToUsers ->
                                                            %% 发送ice_candidate给用户,但是此处发给多个终端还是一个
                                                            lists:foreach(fun(U) ->
                                                                DataToSend = jiffy:encode({[{<<"cmd">>, <<"webrtc_send_ice_candidate">>}, {<<"from">>, SessionUserMnesia#session.uid}, {<<"label">>, Label}, {<<"candidate">>, Candidate}, {<<"ack">>, util:uuid()}]}),
                                                                {U#session.register_name, U#session.node} ! {webrtc_send_ice_candidate, DataToSend}
                                                            end, ToUsers),
                                                            State
                                                    end;
                                                _ ->
                                                    %% 用户登陆后发送了未知命令
                                                    erlim_client:reply_error(Socket, <<"Unknown cmd">>, 10400, Protocol),
                                                    State
                                            end;
                                        _ ->
                                            %% 用户登陆后发送了未知命令
                                            erlim_client:reply_error(Socket, <<"Unknown cmd">>, 10400, Protocol),
                                            State
                                    end
                            end
                    end
            end
    end.
