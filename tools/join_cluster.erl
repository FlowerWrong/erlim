-module(join_cluster).

-export([start/0]).

set_table_copy(Table, _Node, {badrpc, Reason}) ->
    io:format("Error: cannot get storage type for table ~p on node erlim@192.168.33.10:~n ~p~n",[Table, Reason]);
set_table_copy(Table, Node, Type) ->
    io:format("setting table ~p to mode ~p~n",[Table, Type]),
    case mnesia:add_table_copy(Table, Node, Type) of
    {aborted, _} ->
        mnesia:change_table_copy_type(Table, Node, Type);
    _ ->
        ok
    end.

set_tables({badrpc, Reason}) ->
    io:format("ERROR: cannot get tables list on erlim@192.168.33.10 : ~p~n",[Reason]);
set_tables([]) ->
    ok;
set_tables([schema | Tables]) ->
    set_tables(Tables);
set_tables([s2s | Tables]) ->
    set_tables(Tables);
set_tables([session | Tables]) ->
    set_tables(Tables);
set_tables([Table | Tables]) ->
    set_table_copy(Table, node(),
                   rpc:call('erlim@192.168.33.10', mnesia, table_info, [Table, storage_type])),
    set_tables(Tables).

start() ->
    io:format("~n",[]),
    R = case net_adm:ping('erlim@192.168.33.10') of
        pong ->
            set_table_copy(schema, node(), disc_copies),
            set_tables(rpc:call('erlim@192.168.33.10', mnesia, system_info, [tables])),
            0;
        pang ->
            io:format("node ~p is not reachable, please check epmd port, and FIREWALL_WINDOW ports~n", ['erlim@192.168.33.10']),
            1
    end,
    halt(R).
