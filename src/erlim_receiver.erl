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
    client_pid
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
    State = #state{socket = Socket},
    setopts(State#state.socket),
    {ok, State}.

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
    NewState = State,
    io:format("NewState is ~p~n", [NewState]),
    {noreply, NewState, NewState#state.heartbeat_timeout};
% tcp connection change to passive
handle_info({tcp_passive, Socket}, #state{socket = Socket} = State) ->
    io:format("tcp_passive is ~p~n", [State]),
    setopts(Socket),
    {noreply, State};
% connection closed
handle_info({tcp_closed, _Socket}, State) ->
    io:format("tcp_closed is ~p~n", [State]),
    {stop, tcp_closed, State};
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
terminate(_Reason, _State) ->
    ok.

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
    inet:setopts(Socket, [{active, 300}, {packet, 0}, binary]).


%% 接收数据
get_request(Socket) ->
    io:format("get request pid is ~p.~n", [self()]),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Data is ~p.~n", [Data]),
            %% IsJSON = jsx:is_json(Data),
            Json = jiffy:decode(Data),
            io:format("Json is ~p.~n", [Json]),
            {[{<<"cmd">>, Cmd}, {<<"username">>, Username}, {<<"password">>, Password} | T]} = Json,
            io:format("Cmd is ~p.~n", [Cmd]),

            CurrentUser = query_user(Username, Password),
            {user, Cname, Cpass, Pid} = CurrentUser,

            case Cmd of
                <<"login">> ->
                    io:format("CurrentUser is ~p.~n", [CurrentUser]),
                    io:format("Pid is ~p.~n", [Pid]),

                    UserToUpdate = #user{username = Cname, password = Cpass, pid = self()},
                    F1 = fun() ->
                        mnesia:write(UserToUpdate)
                         end,
                    mnesia:transaction(F1),

                    Fun2 = fun() ->
                        Query = qlc:q([X || X <- mnesia:table(user)]),
                        qlc:e(Query)
                          end,
                    io:format("UpdatedUser is ~p.~n", [mnesia:transaction(Fun2)]);
                <<"single_chat">> ->
                    %% {<<"to">>,<<"kang">>},{<<"msg">>,<<"hello world">>}
                    [{<<"to">>, ToUsername}, {<<"msg">>, Msg}] = T,
                    io:format("ToUsername is ~p.~n", [ToUsername]),
                    ToUser = query_user(ToUsername),
                    io:format("ToUser is ~p.~n", [ToUser]),
                    {user, _ToUsername, _ToPass, ToPid} = ToUser,
                    io:format("ToPid is ~p.~n", [ToPid]),
                    case ToPid of
                        0 ->
                            %% ofline
                            ok;
                        _ ->
                            %% online
                            io:format("Send msg to ~p~n", [self()]),
                            self() ! {single_chat, Msg},
                            ok
                    end,
                    io:format("Msg is ~p.~n", [Msg]),
                    ok;
                <<"group_chat">> ->
                    [{<<"to">>, To}, {<<"msg">>, Msg}] = T,
                    io:format("Pid is ~p.~n", [Pid]),
                    io:format("To is ~p.~n", [To]),
                    io:format("Msg is ~p.~n", [Msg]),
                    ok;
                <<"logout">> ->
                    io:format("Pid is ~p.~n", [Pid]),
                    io:format("T is ~p.~n", [T]),
                    ok
            end,

            %% 解析数据,绑定pid,获取pid
            %% 判断操作(login/logout/single_chat/group_chat)
            %% if chat: 发送数据给目标pid
            %% gen_tcp:send(Socket, Data),
            %% ok = gen_tcp:close(Socket);
            get_request(Socket);
        {error, closed} ->
            io:format("client sock close~n"),
            Pid = self(),
            io:format("Pid  close ~p.~n", [Pid]),
            CloseSession = query_pid(Pid),
            {user, CloseName, ClosePass, _Pid} = CloseSession,

            UserToUpdate = #user{username = CloseName, password = ClosePass, pid = 0},
            F1 = fun() ->
                mnesia:write(UserToUpdate)
                 end,
            mnesia:transaction(F1),

            Fun2 = fun() ->
                Query = qlc:q([X || X <- mnesia:table(user)]),
                qlc:e(Query)
                  end,
            io:format("Close session Users is ~p.~n", [mnesia:transaction(Fun2)])
  end.



%% helper

query_pid(Pid) ->
    Fun = fun() ->
        Query = qlc:q([X || X <- mnesia:table(user), X#user.pid =:= Pid]),
        qlc:e(Query)
        end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> false;
        {atomic, [User]} -> User
    end.

query_user(Username) ->
    Fun = fun() ->
        Query = qlc:q([X || X <- mnesia:table(user), X#user.username =:= binary_to_list(Username)]),
        qlc:e(Query)
        end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> false;
        {atomic, [User]} -> User
    end.

query_user(Username, Password) ->
    Fun = fun() ->
        Query = qlc:q([X || X <- mnesia:table(user), X#user.username =:= binary_to_list(Username), X#user.password =:= binary_to_list(Password)]),
        qlc:e(Query)
        end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> false;
        {atomic, [User]} -> User
    end.
