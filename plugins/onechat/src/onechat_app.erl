-module(onechat_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    {ok,[{
        <<"database">>,
        [
            {<<"encoding">>, Encoding},
            {<<"db">>, Dbname},
            {<<"pwd">>, Pwd},
            {<<"name">>, UserName},
            {<<"size">>, Size},
            {<<"host">>, Host}
        ]
    }]} = toml_util:parse(),

    emysql:add_pool(onechat_pool, [
        {size, Size},
        {user, binary_to_list(UserName)},
        {password, binary_to_list(Pwd)},
        {host, binary_to_list(Host)},
        {database, binary_to_list(Dbname)},
        {encoding, binary_to_atom(Encoding, utf8)}
    ]),

	Dispatch = cowboy_router:compile([
		{'_', [
            {"/", index_handler, []},
            {"/api/v1/contacts", friendship_handler, []},
            {"/api/v1/blocks", blockship_handler, []}
        ]}
	]),
	cowboy:start_http(http, 100, [{port, 8084}],
		[{env, [{dispatch, Dispatch}]}]
	),
	onechat_sup:start_link().

stop(_State) ->
	ok.
