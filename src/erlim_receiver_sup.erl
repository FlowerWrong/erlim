%% @hidden
-module(erlim_receiver_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD, {erlim_receiver, {erlim_receiver, start_link, []}, temporary, infinity, worker, [erlim_receiver]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Socket) ->
    {ok, Pid} = supervisor:start_child(?MODULE, [Socket]),
    Pid.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, {{simple_one_for_one, 0, 1}, [?CHILD]}}.
