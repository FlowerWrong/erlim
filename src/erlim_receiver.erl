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
    heartbeat_timeout = ?HIBERNATE_TIMEOUT,
    client_pid,
    ip,
    uid
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
    NewState = #state{socket = Socket, ip = IP},
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
handle_info({tcp, Socket, Data}, #state{socket = Socket} = State) ->
    setopts(Socket),
    IsJSON = jsx:is_json(Data),
    NewState =
        case IsJSON of
            false ->
                erlim_client:reply_error(Socket, <<"Invide JSON">>, 10400),
                State;
            true ->
                Json = jiffy:decode(Data),
                lager:info("Json is ~p.~n", [Json]),
                {[{<<"cmd">>, Cmd} | T]} = Json,
                if
                    Cmd =:= <<"login">> ->
                        [{<<"name">>, Name}, {<<"pass">>, Pass}, {<<"ack">>, Ack}] = T,
                        io:format("Ack is ~p~n", [Ack]),
                        LoginUserMysql = mysql_util:query_user_by_mobile(Name),
                        case LoginUserMysql of
                            [] ->
                                erlim_client:reply_error(Socket, <<"404 Not Found user with this name">>, 10404),
                                State;
                            #user_record{password_digest = PD, id = Uid} ->
                                PassDigest = binary_to_list(PD),
                                {ok, PassDigest} =:= bcrypt:hashpw(Pass, PassDigest),
                                ClientPid = erlim_client_sup:start_child(Socket),
                                erlim_sm:login(Uid, ClientPid),
                                %% Send login ack to client
                                erlim_client:reply_ack(Socket, <<"login">>, Ack),

                                State#state{client_pid = ClientPid, uid = Uid}
                        end;
                    true ->
                        #state{uid = Uid} = State,
                        SessionUserMnesia = mnesia_util:query_session_by_uid(Uid),
                        case SessionUserMnesia of
                            false ->
                                erlim_client:reply_error(Socket, <<"404 Not Found with this token, please login">>, 10404),
                                State;
                            _ ->
                                FromUserMysql = mysql_util:query_user_by_id(SessionUserMnesia#user.uid),
                                case FromUserMysql of
                                    [] ->
                                        %% 可能是因为mysql数据库删除了
                                        erlim_client:reply_error(Socket, <<"404 Not Found this user in mysql, please login again">>, 10404),
                                        State;
                                    _ ->
                                        case Cmd of
                                            <<"single_chat">> ->
                                                [{<<"to">>, ToUid}, {<<"msg">>, Msg}, {<<"ack">>, Ack}] = T,
                                                %% 是否好友关系
                                                case mysql_util:are_friends(SessionUserMnesia#user.uid, ToUid) of
                                                    false ->
                                                        erlim_client:reply_error(Socket, <<"You are not friends">>, 10403),
                                                        State;
                                                    true ->
                                                        ToUserMysql = mysql_util:query_user_by_id(ToUid),
                                                        case ToUserMysql of
                                                            [] ->
                                                                erlim_client:reply_error(Socket, <<"404 Not Found this user in mysql">>, 10404),
                                                                State;
                                                            _ ->
                                                                ToPid = erlim_sm:get_session(ToUid),
                                                                OffMsg = #msg_record{f = FromUserMysql#user_record.id, t = ToUserMysql#user_record.id, msg = Msg, unread = 1},
                                                                {ok_packet, _, _, MsgId, _, _, _} = mysql_util:save_msg(OffMsg),
                                                                %% Send single_chat ack to client
                                                                erlim_client:reply_ack(Socket, <<"single_chat">>, Ack),
                                                                case ToPid of
                                                                    false ->  %% ofline
                                                                        ok;
                                                                    _ ->  %% online
                                                                        DataToSend = jiffy:encode({[{<<"cmd">>, <<"single_chat">>}, {<<"from">>, SessionUserMnesia#user.uid}, {<<"msg">>, Msg}, {<<"ack">>, MsgId}]}),
                                                                        ToPid ! {single_chat, DataToSend}
                                                                end,
                                                                State
                                                        end
                                                end;
                                            <<"group_chat">> ->
                                                [{<<"to">>, ToRoomId}, {<<"msg">>, Msg}, {<<"ack">>, Ack}] = T,
                                                %% 群是否存在
                                                case mysql_util:is_an_exist_room(ToRoomId) of
                                                    false ->
                                                        erlim_client:reply_error(Socket, <<"404 Not Found this room in mysql">>, 10404),
                                                        State;
                                                    true ->
                                                        %% 用户是否在该群里面
                                                        case mysql_util:in_room(SessionUserMnesia#user.uid, ToRoomId) of
                                                            false ->
                                                                erlim_client:reply_error(Socket, <<"You are not in this room">>, 10403),
                                                                State;
                                                            true ->
                                                                RoomMsg = #roommsg_record{f = FromUserMysql#user_record.id, t = ToRoomId, msg = Msg},
                                                                {ok_packet, _, _, RoommsgId, _, _, _} = mysql_util:save_room_msg(RoomMsg),

                                                                Members = mysql_util:room_members(ToRoomId),
                                                                io:format("Members are ~p.~n", [Members]),

                                                                %% Send group_chat ack to client
                                                                erlim_client:reply_ack(Socket, <<"group_chat">>, Ack),

                                                                lists:foreach(fun(M) ->
                                                                    case mysql_util:query_user_by_id(M#room_users_record.user_id) of
                                                                        [] -> false;
                                                                        #user_record{id = Id} ->
                                                                            %% Save members unread roommsg
                                                                            {ok_packet, _, _, UserRoommsgId, _, _, _} = mysql_util:save_user_room_msg(RoommsgId, Id),
                                                                            case mnesia_util:query_session_by_uid(Id) of
                                                                                false -> false;
                                                                                #user{pid = ToPid} ->
                                                                                    DataToSend = jiffy:encode({[{<<"cmd">>, <<"group_chat">>}, {<<"from">>, SessionUserMnesia#user.uid}, {<<"to">>, ToRoomId}, {<<"msg">>, Msg}, {<<"ack">>, UserRoommsgId}]}),
                                                                                    ToPid ! {group_chat, DataToSend}
                                                                            end
                                                                    end
                                                                              end, Members),
                                                                State
                                                        end
                                                end;
                                            <<"ack">> ->
                                                [{<<"action">>, Action}, {<<"ack">>, Ack}] = T,
                                                io:format("Action is ~p, Timestamp is ~p~n", [Action, Ack]),
                                                case Action of
                                                    <<"single_chat">> ->
                                                        io:format("Single chat msg ack is ~p~n", [Ack]),
                                                        mysql_util:mark_read(Ack, single_chat);
                                                    <<"group_chat">> ->
                                                        io:format("Group chat msg ack is ~p~n", [Ack]),
                                                        mysql_util:mark_read(Ack, group_chat);
                                                    _ ->
                                                        erlim_client:reply_error(Socket, <<"404 Not Found this ack action">>, 10404)
                                                end,
                                                State;
                                            <<"logout">> ->
                                                self() ! {tcp_closed, Socket},
                                                State;
                                            _ ->
                                                %% 用户登陆后发送了未知命令
                                                self() ! {unknown_cmd, Socket},
                                                State
                                        end
                                end
                        end
                end
        end,

    io:format("NewState is ~p~n", [NewState]),
    {noreply, NewState, NewState#state.heartbeat_timeout};
% tcp connection change to passive
handle_info({tcp_passive, Socket}, #state{socket = Socket} = State) ->
    io:format("tcp_passive is ~p~n", [State]),
    {noreply, State};
handle_info({unknown_cmd, Socket}, State) ->
    DataToSend = jiffy:encode({[{<<"cmd">>, <<"unknown_cmd">>}]}),
    erlim_client:reply(Socket, DataToSend),
    {noreply, State};
% connection closed
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(timeout, State) ->
    proc_lib:hibernate(gen_server, enter_loop, [?MODULE, [], State]),
    {noreply, State, State#state.heartbeat_timeout};
handle_info(_Info, State) ->
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
terminate(_Reason, #state{client_pid = ClientPid}) ->
    %% 这里有三种情况
    %% 1. 用户正常退出, 或网络掉线退出
    lager:error("Receiver ~p terminated.~n", [self()]),
    io:format("ClientPid ~p will be terminated.~n", [ClientPid]),
    case ClientPid of
        undefined -> undefined;
        _ ->
            SessionMnesia = mnesia_util:query_session_by_pid(ClientPid),
            io:format("SessionMnesia is ~p.~n", [SessionMnesia]),
            case SessionMnesia of
                false -> undefined;
                #user{uid = Uid} ->
                    ok = erlim_sm:logout(Uid),
                    mysql_util:save_logout(Uid),
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
