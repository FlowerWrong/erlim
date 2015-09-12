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

-record(state, {
    socket,
    heartbeat_timeout = 600000,
    client_pid,
    ip,
    token,
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
    Json = jiffy:decode(Data),
    io:format("IsJSON is ~p.~n", [IsJSON]),
    io:format("Json is ~p.~n", [Json]),
    {[{<<"cmd">>, Cmd} | T]} = Json,
    io:format("Cmd is ~p.~n", [Cmd]),

    NewState = case Cmd of
        <<"login">> ->
            [{<<"name">>, Name}, {<<"pass">>, Pass}] = T,
            LoginUserMysql = mysql_util:query_user_by_mobile(Name),
            io:format("user is ~p~n", [LoginUserMysql]),
            PassDigest = binary_to_list(LoginUserMysql#user_record.password_digest),
            {ok, PassDigest} =:= bcrypt:hashpw(Pass, PassDigest),

            ClientPid = erlim_client_sup:start_child(Socket),

            Uid = LoginUserMysql#user_record.id,
            {ok, Token} = erlim_sm:login(Uid, ClientPid),

            DataToSend = jiffy:encode({[{<<"token">>, Token}]}),
            io:format("DataToSend is ~p~n", [DataToSend]),
            gen_tcp:send(Socket, DataToSend),
            State#state{client_pid = ClientPid, token = Token, uid = Uid};
        <<"single_chat">> ->
            %% FIXME 添加用户权限判断
            [{<<"token">>, Token}, {<<"to">>, ToUid}, {<<"msg">>, Msg}] = T,
            ToPid = erlim_sm:get_session(ToUid),
            ToUserMysql = mysql_util:query_user_by_id(ToUid),

            SessionUserMnesia = mnesia_util:query_session_by_token(Token),
            FromUserMysql = mysql_util:query_user_by_id(SessionUserMnesia#user.uid),

            case ToPid of
                false ->  %% ofline
                    io:format("Send msg to ~p, ~p, msg is ~p.~n", [ToPid, ToUserMysql, Msg]),
                    OffMsg = #msg_record{f = FromUserMysql#user_record.id, t = ToUserMysql#user_record.id, msg = Msg, unread = 1},
                    {ok_packet, _, _, _, _, _, _} = mysql_util:save_msg(OffMsg);
                _ ->  %% online
                    io:format("Send msg to ~p, ~p, msg is ~p.~n", [ToPid, ToUserMysql, Msg]),
                    OnlineMsg = #msg_record{f = FromUserMysql#user_record.id, t = ToUserMysql#user_record.id, msg = Msg, unread = 0},
                    {ok_packet, _, _, _, _, _, _} = mysql_util:save_msg(OnlineMsg),

                    DataToSend = jiffy:encode({[{<<"cmd">>, <<"single_chat">>}, {<<"from">>, SessionUserMnesia#user.uid}, {<<"to">>, ToUid}, {<<"msg">>, Msg}]}),
                    ToPid ! {single_chat, DataToSend}
            end,
            State;
        <<"group_chat">> ->
            %% FIXME 添加用户权限判断
            [{<<"token">>, Token}, {<<"to">>, ToRoomId}, {<<"msg">>, Msg}] = T,
            io:format("ToRoomId is ~p.~n", [ToRoomId]),
            io:format("Msg is ~p.~n", [Msg]),

            SessionUserMnesia = mnesia_util:query_session_by_token(Token),
            FromUserMysql = mysql_util:query_user_by_id(SessionUserMnesia#user.uid),
            RoomMsg = #roommsg_record{f = FromUserMysql#user_record.id, t = ToRoomId, msg = Msg},
            mysql_util:save_room_msg(RoomMsg),

            Members = mysql_util:room_members(ToRoomId),
            io:format("Members is ~p.~n", [Members]),

            DataToSend = jiffy:encode({[{<<"cmd">>, <<"group_chat">>}, {<<"from">>, SessionUserMnesia#user.uid}, {<<"to">>, ToRoomId}, {<<"msg">>, Msg}]}),

            lists:foreach(fun(M) ->
                case mysql_util:query_user_by_id(M#room_users_record.user_id) of
                    [] -> false;
                    #user_record{id = Id} ->
                        case mnesia_util:query_session_by_uid(Id) of
                            false -> false;
                            #user{pid = ToPid} ->
                                ToPid ! {group_chat, DataToSend}
                        end
                end
            end, Members),
            State;
        <<"logout">> ->
            %% FIXME 添加用户权限判断
            [{<<"token">>, _Token}] = T,
            self() ! {tcp_closed, Socket},
            State
    end,

    io:format("NewState is ~p~n", [NewState]),
    {noreply, NewState, NewState#state.heartbeat_timeout};
% tcp connection change to passive
handle_info({tcp_passive, Socket}, #state{socket = Socket} = State) ->
    io:format("tcp_passive is ~p~n", [State]),
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
    io:format("Receiver ~p terminated.~n", [self()]),
    SessionMnesia = mnesia_util:query_session_by_pid(ClientPid),
    io:format("SessionMnesia is ~p.~n", [SessionMnesia]),
    ok = erlim_sm:logout(SessionMnesia#user.token),
    mysql_util:save_logout(SessionMnesia#user.uid),
    ok = erlim_client:stop(ClientPid).

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
