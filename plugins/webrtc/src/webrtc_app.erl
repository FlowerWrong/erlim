-module(webrtc_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [{"/", cowboy_static, {priv_file, webrtc, "static/index.html"}}]}
	]),
	cowboy:start_http(http, 100, [{port, 8081}],
		[{env, [{dispatch, Dispatch}]}]
	),
	webrtc_sup:start_link().

stop(_State) ->
	ok.
