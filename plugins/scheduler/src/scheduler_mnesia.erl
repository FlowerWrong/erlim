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

%% API
-export([init/0]).

init() ->
    case mnesia:system_info(extra_db_nodes) of
        [] ->
            mnesia:stop(),
            mnesia:delete_schema([node()]),
            mnesia:create_schema([node()]),
            mnesia:start(),
            update_tables(),
            mnesia:change_table_copy_type(schema, node(), disc_copies),
            create_table();
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
