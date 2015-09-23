-module(erlim_server).

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

%% We do not block on send anymore.
-define(TCP_SEND_TIMEOUT, 15000).

%% http://www.cnblogs.com/ribavnu/archive/2013/08/06/3240435.html
%% http://www.cnblogs.com/ribavnu/p/3409823.html
%% http://learnyousomeerlang.com/buckets-of-sockets
-define(TCP_OPTIONS, [binary,
    {ip, {0, 0, 0, 0}},
    {packet, 0},
    %% {backlog, 8192},
    {buffer, 1048576},
    %% {recbuf, 8192},
    {active, false},
    {reuseaddr, true},
    {nodelay, true},
    {send_timeout, ?TCP_SEND_TIMEOUT},
    {send_timeout_close, true},
    {keepalive, true}]
).

-record(state, {
    port,                   % listen port
    listen_socket,          % listen socket
    acceptor_ref
}).

-include("table.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% http://www.blogjava.net/yongboy/archive/2012/10/24/390185.html

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
init(#state{port=Port}) ->
    process_flag(trap_exit, true),

    erlim_mnesia:init_mnesia(),

    application:start(crypto),
    application:start(bcrypt),
    application:start(emysql),
    lager:start(),

    {ok,[{
        <<"database">>,
        [
            {<<"encoding">>, Encoding},
            {<<"db">>, Dbname},
            {<<"pwd">>, Pwd},
            {<<"name">>, UserName},
            {<<"size">>, Size},
            {<<"host">>, Host}
        ]
    }]} = toml_util:parse(),
    io:format("Host is ~p~n", [Host]),
    emysql:add_pool(erlim_pool, [
        {size, Size},
        {user, binary_to_list(UserName)},
        {password, binary_to_list(Pwd)},
        {host, binary_to_list(Host)},
        {database, binary_to_list(Dbname)},
        {encoding, binary_to_atom(Encoding, utf8)}
    ]),

    observer:start(),

    {ok, ListenSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    {ok, AcceptorRef} = prim_inet:async_accept(ListenSocket, -1),
    NewState = #state{listen_socket = ListenSocket, acceptor_ref = AcceptorRef},
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

%% http://erlangcentral.org/wiki/index.php/Building_a_Non-blocking_TCP_server_using_OTP_principles
handle_info({inet_async, ListenSocket, AcceptorRef, {ok, ClientSocket}}, #state{listen_socket = ListenSocket, acceptor_ref = AcceptorRef} = State) ->
    case set_sockopt(State#state.listen_socket, ClientSocket) of
        ok ->
            %% New client connected - spawn a new process using the simple_one_for_one
            Pid = erlim_receiver_sup:start_child(ClientSocket),
            gen_tcp:controlling_process(ClientSocket, Pid),

            %% Signal the network driver that we are ready to accept another connection
            {ok, NewAcceptorRef} = prim_inet:async_accept(ListenSocket, -1),
            NewState = State#state{acceptor_ref = NewAcceptorRef},
            {noreply, NewState};
        Error ->
            io:format("set sockopt error is ~p~n", [Error]),
            {stop, Error, State}
    end;
handle_info({inet_async, ListenSocket, AcceptorRef, Error}, #state{listen_socket = ListenSocket, acceptor_ref = AcceptorRef} = State) ->
    io:format("Error in socket acceptor: ~p.~n", [Error]),
    {stop, Error, State};
handle_info(Info, State) ->
    io:format("Info: ~p~n", [Info]),
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
terminate(_Reason, State) ->
    gen_tcp:close(State#state.listen_socket).

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

%% ===================================================================
%% Internal functions
%% ===================================================================

%% Taken from prim_inet.  We are merely copying some socket options from the
%% listening socket to the new TCP socket.
set_sockopt(ListenSocket, ClientSocket) ->
    true = inet_db:register_socket(ClientSocket, inet_tcp),
    case prim_inet:getopts(ListenSocket, [active, nodelay, keepalive, delay_send, priority, tos]) of
    {ok, Opts} ->
        case prim_inet:setopts(ClientSocket, Opts) of
            ok -> ok;
            Error ->
                io:format("set_sockopt/2: prim_inet:setopts/2 error is ~p~n", [Error]),
                gen_tcp:close(ClientSocket), Error
        end;
    Error ->
        io:format("set_sockopt/2: prim_inet:getopts/2 error is ~p~n", [Error]),
        gen_tcp:close(ClientSocket), Error
    end.
