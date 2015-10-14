%% @hidden
-module(erlim_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

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
-define(HTTP_OPTIONS, [
    {loop, {erlim_http, dispatch}},
    {name, erlim_http}
]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    [ok = application:start(App) ||
        App <- [syntax_tools, os_mon, asn1, crypto, public_key, bcrypt, emysql]],

    [ok = App:start() ||
        App <- [ssl, esockd, lager]],

    erlim_mnesia:init_mnesia(),

    %% erlang app config file in 3 ways
    %% http://blog.yufeng.info/archives/2852
    {ok, [
        {<<"database">>,
            [
                {<<"encoding">>, Encoding},
                {<<"db">>, Dbname},
                {<<"pwd">>, Pwd},
                {<<"name">>, UserName},
                {<<"size">>, Size},
                {<<"host">>, Host}
            ]
        },
        {<<"socket">>,
            [
                {<<"use_ssl">>, EnableSSL},
                {<<"time_out">>, _TcpSentTimeOut},
                {<<"max_clients">>, MaxClients},
                {<<"acceptors">>, Acceptors},
                {<<"port">>, Port}
            ]
        },
        {<<"ssl">>,
            [
                {<<"keyfile">>, KeyFile},
                {<<"certfile">>, CertFile},
                {<<"cacertfile">>, CaCertFile}
            ]
        },
        {<<"http">>,
            [
                {<<"port">>, HttpPort}
            ]
        }
    ]} = toml_util:parse(),
    emysql:add_pool(erlim_pool, [
        {size, Size},
        {user, binary_to_list(UserName)},
        {password, binary_to_list(Pwd)},
        {host, binary_to_list(Host)},
        {database, binary_to_list(Dbname)},
        {encoding, binary_to_atom(Encoding, utf8)}
    ]),

    Opts = case EnableSSL of
               0 ->
                   [{acceptors, Acceptors},
                       {max_clients, MaxClients},
                       {sockopts, ?TCP_OPTIONS}];
               1 ->
                   %% http://www.ttlsa.com/nginx/nginx-configuration-ssl/
                   %% http://erlycoder.com/87/ssl-how-to-self-signed-ssl-certifiate-creation-with-open-ssl
                   SslOpts = [
                       {cacertfile, binary_to_list(CaCertFile)},
                       {certfile, binary_to_list(CertFile)},
                       {keyfile, binary_to_list(KeyFile)}
                   ],
                   [{acceptors, Acceptors},
                       {max_clients, MaxClients},
                       {ssl, SslOpts},
                       {sockopts, ?TCP_OPTIONS}];
               _ -> exit(config_file_error)
           end,
    open_listener({onechat, Port, Opts}),

    open_listener({http, HttpPort, ?HTTP_OPTIONS}),

    erlim_sup:start_link().

stop(_State) ->
    ok.


%% @doc open onechat port
open_listener({onechat, Port, Options}) ->
    MFArgs = {erlim_receiver, start_link, []},
    {ok, _} = esockd:open(onechat, Port, Options, MFArgs);
%% @doc open http port
open_listener({http, Port, Options}) ->
    NewOpts = lists:merge(Options, [{port, Port}]),
    lager:info("Http opts are ~p~n", [NewOpts]),
    mochiweb_http:start(NewOpts).