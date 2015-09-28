-module(scheduler_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [{"/", schedule_handler, []}]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8083}],
		[{env, [{dispatch, Dispatch}]}]
	),
	scheduler_sup:start_link().

stop(_State) ->
	ok.
