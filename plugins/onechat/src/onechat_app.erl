-module(onechat_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [{"/", user_handler, []}]}
	]),
	cowboy:start_http(http, 100, [{port, 8084}],
		[{env, [{dispatch, Dispatch}]}]
	),
	onechat_sup:start_link().

stop(_State) ->
	ok.
