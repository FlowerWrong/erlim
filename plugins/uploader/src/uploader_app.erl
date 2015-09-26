-module(uploader_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, uploader, "index.html"}},
            {"/upload", uploader_handler, []},
            {"/files/[...]", cowboy_static, {priv_dir, uploader, "files"}}
        ]}
    ]),
    cowboy:start_http(http, 100, [{port, 8082}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    uploader_sup:start_link().

stop(_State) ->
    ok.
