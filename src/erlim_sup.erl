%% @hidden
-module(erlim_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(SERVERCHILD(I, Type), {I, {I, start_link, [8080]}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

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
    ErlimTlsServer = ?SERVERCHILD(erlim_tls_server, worker),
    ErlimTlsReceiverSup = ?CHILD(erlim_tls_receiver_sup, worker),
    ErlimClientSup = ?CHILD(erlim_client_sup, worker),
    {ok, { {one_for_one, 5, 10}, [ErlimTlsServer, ErlimTlsReceiverSup, ErlimClientSup] } }.
