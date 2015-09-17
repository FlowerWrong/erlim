-module(erlim_sm).

-behaviour(gen_server).

%% API functions
-export([
    start_link/0,
    login/3,
    get_session/1,
    get_session/2,
    logout/2
]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

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
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
init([]) ->
    {ok, #state{}}.

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

login(Uid, ClientPid, Device) when is_integer(Uid), is_pid(ClientPid) ->
    UserToBeLoginMnesia = #user{uid = Uid, pid = ClientPid, device = Device, node = node()},
    F = fun() ->
        mnesia:write(UserToBeLoginMnesia)
         end,
    mnesia:transaction(F).

get_session(Uid) when is_integer(Uid) ->
    case mnesia_util:query_session_by_uid(Uid) of
        false -> false;
        Users -> Users
    end.
get_session(Uid, Device) when is_integer(Uid) ->
    case mnesia_util:query_session_by_uid_and_device(Uid, Device) of
        false -> false;
        {user, _Name, ClientPid} -> ClientPid
    end.

logout(Uid, Device) when is_integer(Uid) ->
    lager:info("Users is ~p~n", [mnesia_util:all()]),
    CurrentUserMnesia = mnesia_util:query_session_by_uid_and_device(Uid, Device),
    lager:info("CurrentUser is ~p~n", [CurrentUserMnesia]),
    F = fun() -> mnesia:delete_object(CurrentUserMnesia) end,
    mnesia:transaction(F),
    lager:info("Users is ~p~n", [mnesia_util:all()]),
    ok.
