%%%-------------------------------------------------------------------
%%% @author yy
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 九月 2015 下午3:38
%%%-------------------------------------------------------------------
-module(scheduler_mnesia).
-author("yy").

-include("scheduler.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(TOTAL_IP, <<"0.0.0.0">>).

%% @doc API for setup
-export([init/0]).

%% @doc mnesia CURD API
-export([reqs/0, ip_records/0, ip_numbers/0, ip_record/1, total_requests/0, total_request_record/0, add_total_request/0, add_ip_request/1]).


%% @doc req records
reqs() ->
    Fun = fun() ->
        Query = qlc:q([X || X <- mnesia:table(req)]),
        qlc:e(Query)
          end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> [];
        {atomic, Requests} -> Requests
    end.

ip_records() ->
    Fun = fun() ->
        Query = qlc:q([X || X <- mnesia:table(req), X#req.ip =/= ?TOTAL_IP]),
        qlc:e(Query)
          end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> [];
        {atomic, Requests} -> Requests
    end.

ip_numbers() ->
    length(ip_records()).

%% @doc ip record
ip_record(Ip) when is_binary(Ip) ->
    Fun = fun() ->
        Query = qlc:q([X || X <- mnesia:table(req), X#req.ip =:= Ip]),
        qlc:e(Query)
          end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> [];
        {atomic, [Request]} -> Request
    end.


%% @doc total req record
total_request_record() ->
    ip_record(?TOTAL_IP).

%% @doc total req number
total_requests() ->
    case total_request_record() of
        [] -> 0;
        Request -> Request#req.number
    end.

%% @doc total req number +1
add_total_request() ->
    case total_request_record() of
        [] -> false;
        Request ->
            TotalRequestMnesia = #req{ip = ?TOTAL_IP, port = 0, number = Request#req.number + 1, connected = 0, max = 0},
            F = fun() ->
                mnesia:write(TotalRequestMnesia)
                end,
            mnesia:transaction(F)
    end.

add_ip_request(Ip) when is_binary(Ip) ->
    case ip_record(Ip) of
        [] -> false;
        Request ->
            IpRequestMnesia = #req{ip = Ip, port = Request#req.port, number = Request#req.number + 1, connected = Request#req.connected, max = Request#req.max},
            F = fun() ->
                mnesia:write(IpRequestMnesia)
                end,
            mnesia:transaction(F)
    end.


%% @doc API for setup
init() ->
    case mnesia:system_info(extra_db_nodes) of
        [] ->
            mnesia:stop(),
            mnesia:delete_schema([node()]),
            mnesia:create_schema([node()]),
            mnesia:start(),
            update_tables(),
            mnesia:change_table_copy_type(schema, node(), disc_copies),
            create_table(),
            total_data(),
            ip_data();
        _ ->
            ok
    end,
    mnesia:info().

create_table() ->
    update_tables(),
    mnesia:create_table(req, [
        {attributes, record_info(fields, req)},
        {ram_copies, [node()]}
    ]),
    mnesia:add_table_copy(req, node(), ram_copies).

update_tables() ->
    case catch mnesia:table_info(req, attributes) of
        [ip, port, number, connected, max] -> mnesia:delete_table(req);
        {'EXIT', _} -> ok
    end.

total_data() ->
    TotalRequestMnesia = #req{ip = <<"0.0.0.0">>, port = 0, number = 0, connected = 0, max = 0},
    F = fun() ->
        mnesia:write(TotalRequestMnesia)
        end,
    mnesia:transaction(F).

ip_data() ->
    TotalRequestMnesia = #req{ip = <<"192.168.10.140">>, port = 8080, number = 0, connected = 0, max = 200000},
    F = fun() ->
        mnesia:write(TotalRequestMnesia)
        end,
    mnesia:transaction(F).