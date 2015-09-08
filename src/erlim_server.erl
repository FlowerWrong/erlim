-module(erlim_server).

-behaviour(gen_server).

%% API functions
-export([start_link/1, get_request/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 10000).
-define(TCP_SEND_TIMEOUT, 15000).

%% http://www.cnblogs.com/ribavnu/archive/2013/08/06/3240435.html
%% http://www.cnblogs.com/ribavnu/p/3409823.html
%% http://learnyousomeerlang.com/buckets-of-sockets
%% TODO use active once
-define(TCP_OPTIONS, [binary,
    {packet, 0},
    {active, false},
    {reuseaddr, true},
    {nodelay, true},
    {send_timeout, ?TCP_SEND_TIMEOUT},
    {send_timeout_close, true},
    {keepalive, true}]
).

-record(state, {
    port,                   % listen port
    lsocket=null            % listen socket
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
start_link(Port) ->
    State = #state{port=Port},
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

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
init(State = #state{port=Port}) ->
    io:format("Init pid is ~p.~n", [self()]),

    erlim_mnesia:init_mnesia(),

    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, LSocket} ->
            io:format("~nI am waitting for connect.~n"),
            {ok, wait_connect(State#state{lsocket=LSocket})};
        {error, Reason} ->
            {stop, {create_listen_socket, Reason}}
    end.


%% accept spawn a new process
wait_connect(State = #state{lsocket=LSocket}) ->
    io:format("Main accept pid is ~p.~n", [self()]),
    {ok, Sock} = gen_tcp:accept(LSocket),

    %% Pid = spawn(?MODULE, get_request, [Sock]),
    {ok, Pid} = erlim_receiver:start_child(Sock),
    gen_tcp:controlling_process(Sock, Pid),

    io:format("Spawn pid is ~p.~n", [Pid]),
    wait_connect(State).


%% 接收数据
get_request(Sock) ->
    io:format("get request pid is ~p.~n", [self()]),
    case gen_tcp:recv(Sock, 0) of
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
            %% gen_tcp:send(Sock, Data),
            %% ok = gen_tcp:close(Sock);
            get_request(Sock);
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
    Reply = {ok},
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
% handle_info(Info, State) ->
%     io:format("single chat Info is ~p, pid is ~p.~n", [Info, self()]),
%     {noreply, State}.

handle_info({single_chat, Msg}, State) ->
    io:format("single chat msg is ~p, pid is ~p.~n", [Msg, self()]),
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
