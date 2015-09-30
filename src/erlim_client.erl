%%%-------------------------------------------------------------------
%%% @author yang
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%   onechat client
%%% @end
%%% Created : 27. 九月 2015 下午12:17
%%%-------------------------------------------------------------------
-module(erlim_client).

-behaviour(gen_server).

%% API functions
-export([
    start_link/2,
    stop/1,
    reply/3,
    reply_error/4,
    reply_ack/4
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    socket,
    protocol
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
start_link(Socket, Protocol) ->
    gen_server:start_link(?MODULE, [Socket, Protocol], []).

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
init([Socket, Protocol]) ->
    State = #state{socket = Socket, protocol = Protocol},
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
handle_cast(stop, State) ->
    {stop, normal, State};
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
handle_info({single_chat, Msg}, #state{socket = Socket, protocol = Protocol} = State) ->
    lager:info("erlim_client single_chat msg is ~p~n", [Msg]),
    reply(Socket, Msg, Protocol),
    {noreply, State};
handle_info({group_chat, Msg}, #state{socket = Socket, protocol = Protocol} = State) ->
    lager:info("erlim_client group_chat msg is ~p~n", [Msg]),
    reply(Socket, Msg, Protocol),
    {noreply, State};
%% @TODO notification
handle_info({notification, Msg}, #state{socket = Socket, protocol = Protocol} = State) ->
    lager:info("erlim_client notification msg is ~p~n", [Msg]),
    reply(Socket, Msg, Protocol),
    {noreply, State};
handle_info({webrtc_create, Msg}, #state{socket = Socket, protocol = Protocol} = State) ->
    lager:info("erlim_client webrtc_create msg is ~p~n", [Msg]),
    reply(Socket, Msg, Protocol),
    {noreply, State};
handle_info({webrtc_join, Msg}, #state{socket = Socket, protocol = Protocol} = State) ->
    lager:info("erlim_client webrtc_join msg is ~p~n", [Msg]),
    reply(Socket, Msg, Protocol),
    {noreply, State};
handle_info({webrtc_leave, Msg}, #state{socket = Socket, protocol = Protocol} = State) ->
    lager:info("erlim_client webrtc_leave msg is ~p~n", [Msg]),
    reply(Socket, Msg, Protocol),
    {noreply, State};
handle_info({webrtc_send_offer, Msg}, #state{socket = Socket, protocol = Protocol} = State) ->
    lager:info("erlim_client webrtc_send_offer msg is ~p~n", [Msg]),
    reply(Socket, Msg, Protocol),
    {noreply, State};
handle_info({webrtc_send_answer, Msg}, #state{socket = Socket, protocol = Protocol} = State) ->
    lager:info("erlim_client webrtc_send_answer msg is ~p~n", [Msg]),
    reply(Socket, Msg, Protocol),
    {noreply, State};
handle_info({webrtc_send_ice_candidate, Msg}, #state{socket = Socket, protocol = Protocol} = State) ->
    lager:info("erlim_client webrtc_send_ice_candidate msg is ~p~n", [Msg]),
    reply(Socket, Msg, Protocol),
    {noreply, State};
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
    lager:info("Client ~p terminated.~n", [self()]),
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

%% @doc stop a client
stop(ClientPid) ->
    gen_server:cast(ClientPid, stop),
    ok.

%% @doc reply to client
reply(Socket, Msg, tcp) ->
    PayloadLen = byte_size(Msg),
    Payload = iolist_to_binary([<<"ONECHAT/1.0\r\nPAYLOAD_LEN: ">>, util:integer2binary(PayloadLen), <<"\r\n\r\n">>, Msg]),
    gen_tcp:send(Socket, Payload);
reply(Socket, Msg, websocket) ->
    ws_util:send_ws_data(Socket, Msg).

%% @doc reply error to client
reply_error(Socket, Error, Code, tcp) ->
    DataToSend = jiffy:encode({[{<<"cmd">>, <<"error">>}, {<<"msg">>, Error}, {<<"code">>, Code}]}),
    reply(Socket, DataToSend, tcp);
reply_error(Socket, Error, Code, websocket) ->
    DataToSend = jiffy:encode({[{<<"cmd">>, <<"error">>}, {<<"msg">>, Error}, {<<"code">>, Code}]}),
    reply(Socket, DataToSend, websocket).

%% @doc reply ack to client
reply_ack(Socket, Action, Ack, tcp) ->
    DataToSend = jiffy:encode({[{<<"cmd">>, <<"ack">>}, {<<"action">>, Action}, {<<"ack">>, Ack}]}),
    reply(Socket, DataToSend, tcp);
reply_ack(Socket, Action, Ack, websocket) ->
    DataToSend = jiffy:encode({[{<<"cmd">>, <<"ack">>}, {<<"action">>, Action}, {<<"ack">>, Ack}]}),
    reply(Socket, DataToSend, websocket).