-module(scheduler_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Mnesia = ?CHILD(scheduler_mnesia, worker),
	{ok, {{one_for_one, 1, 5}, [Mnesia]}}.
