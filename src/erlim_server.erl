-module(erlim_server).

-behaviour(gen_server).

%% API functions
-export([start_link/2]).
-export([accept_loop/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 10000).
-define(TCP_OPTIONS, [binary, {packet, raw}, {active, false}, {reuseaddr, true}]).

-record(state, {
  port,                   % listen port
  ip=any,                 % ip
  lsocket=null,           % listen socket
  conn=0,                 % curent connect
  maxconn                 % max connect
}).

-record(client, {
  username,               % user name
  password,               % password
  pid                     % client pid
}).

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
start_link(Port, Max) ->
  State = #state{port=Port, maxconn=Max},
  io:format("max connection is ~p~n", [Max]),
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
  case gen_tcp:listen(Port, ?TCP_OPTIONS) of
    {ok, LSocket} ->
      {ok, accept(State#state{lsocket=LSocket})};
    {error, Reason} ->
      {stop, {create_listen_socket, Reason}}
  end.


%% accept spawn a new process
accept(State =#state{lsocket=LSocket, conn=Conn, maxconn=Max}) ->
  proc_lib:spawn(erlim_server, accept_loop, [self(), LSocket, Conn, Max]),
  State.

%% accept the new connection
accept_loop(Server, LSocket, Conn, Max) ->
  {ok, Sock} = gen_tcp:accept(LSocket),
  if
    Conn + 1 > Max ->
      io:format("reach the max connection~n"),
      gen_tcp:close(Sock);
    true ->
      gen_server:cast(Server, {accept_new, self()}),
      loop(Sock, Server)
end.

loop(Sock, Server) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, Data} ->
      io:format("server recv data close ~p~n", [Data]),
      %% 解析数据,绑定pid,获取pid
      %% 判断操作(register/login/logout/single_chat/group_chat)
      %% if chat: 发送数据给目标pid
      gen_tcp:send(Sock, Data),
      %% gen_tcp:close(Sock);
      loop(Sock, Server);
    {error, closed} ->
      io:format("client sock close~n"),
      gen_server:cast(Server, {connect_close, self()})
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
%% handle_cast(_Msg, State) ->
%%     {noreply, State}.

%% the server receive the notify that a connect has construct
handle_cast({accept_new, _Pid}, State=#state{conn=Cur}) ->
  io:format("current connect:~p~n", [Cur+1]),
  {noreply, accept(State#state{conn=Cur+1})};

%% someone connect has been close, so change the max connect
handle_cast({connect_close, _Pid}, State=#state{conn=Cur}) ->
  io:format("current connect:~p~n", [Cur-1]),
  {noreply, State#state{conn=Cur-1}}.

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
