-module(erlim_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(SERVERCHILD(I, Type), {I, {I, start_link, [3000]}, permanent, 5000, Type, [I]}).
-define(SESSIONCHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ErlimServer = ?SERVERCHILD(erlim_server, worker),
    ErlimSessionSup = ?SESSIONCHILD(erlim_session_sup, worker),
    io:format("Start erlim_server and erlim_session_sup.~n"),
    {ok, { {one_for_one, 5, 10}, [ErlimServer, ErlimSessionSup] } }.
