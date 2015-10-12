%% @hidden
-module(erlim_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


%% We do not block on send anymore.
-define(TCP_SEND_TIMEOUT, 15000).

-define(TCP_OPTIONS, [binary,
    {ip, {0, 0, 0, 0}},
    {packet, 0},
    %% {backlog, 8192},
    {buffer, 1024},
    %% {recbuf, 8192},
    {active, false},
    {reuseaddr, true},
    {nodelay, true},
    {send_timeout, ?TCP_SEND_TIMEOUT},
    {send_timeout_close, true},
    {keepalive, true}]
).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    erlim_mnesia:init_mnesia(),

    [ok = application:start(App) ||
        App <- [syntax_tools, asn1, crypto, public_key, bcrypt, emysql]],
    ssl:start(),
    esockd:start(),
    lager:start(),

    {ok, [{
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
    emysql:add_pool(erlim_pool, [
        {size, Size},
        {user, binary_to_list(UserName)},
        {password, binary_to_list(Pwd)},
        {host, binary_to_list(Host)},
        {database, binary_to_list(Dbname)},
        {encoding, binary_to_atom(Encoding, utf8)}
    ]),

    %% http://www.ttlsa.com/nginx/nginx-configuration-ssl/
    %% http://erlycoder.com/87/ssl-how-to-self-signed-ssl-certifiate-creation-with-open-ssl
    SslOpts = [
        {cacertfile, "/home/yy/dev/erlang/erlim/crt/demoCA/cacert.pem"},
        {certfile, "/home/yy/dev/erlang/erlim/crt/nginx.crt"},
        {keyfile, "/home/yy/dev/erlang/erlim/crt/nginx.key"}
    ],
    Opts = [{acceptors, 4},
        {max_clients, 1000},
        {ssl, SslOpts},
        {sockopts, ?TCP_OPTIONS}],

    MFArgs = {erlim_tls_receiver, start_link, []},
    esockd:open(onechat, 10000, Opts, MFArgs),

    erlim_sup:start_link().

stop(_State) ->
    ok.