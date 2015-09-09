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
    listen_socket=null,     % listen socket
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
init(State = #state{port=Port}) ->
    process_flag(trap_exit, true),
    io:format("Init pid is ~p.~n", [self()]),

    erlim_mnesia:init_mnesia(),

    observer:start(),

    {ok, ListenSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    {ok, AcceptorRef} = prim_inet:async_accept(ListenSocket, -1),
    NewState = #state{listen_socket = ListenSocket, acceptor_ref = AcceptorRef},
    io:format("State is ~p~n", [NewState]),
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


handle_info({inet_async, ListenSocket, AcceptorRef, {ok, ClientSocket}}, #state{listen_socket = ListenSocket, acceptor_ref = AcceptorRef} = State) ->
    case set_sockopt(State#state.listen_socket, ClientSocket) of
        ok ->
            Pid = erlim_receiver_sup:start_child(ClientSocket),
            io:format("erlim_server start_child receiver is ~p.~n", [Pid]),
            gen_tcp:controlling_process(ClientSocket, Pid),
            case prim_inet:async_accept(ListenSocket, -1) of
                {ok, NewAcceptorRef} -> ok;
                {error, NewAcceptorRef} ->
                    exit({async_accept, inet:format_error(NewAcceptorRef)})
            end,
            NewState = State#state{acceptor_ref = NewAcceptorRef},
            {noreply, NewState};
        Error ->
            {stop, Error, State}
    end;
handle_info(_Info, State) ->
    log:i("_Info:~p~n", [_Info]),
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
                ok    -> ok;
                Error -> gen_tcp:close(ClientSocket), Error
            end;
        Error ->
            gen_tcp:close(ClientSocket), Error
    end.
