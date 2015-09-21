-module(join_cluster).

-export([start/1]).

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

set_tables({badrpc, Reason}, _Remote) ->
    io:format("ERROR: cannot get tables list on erlim@192.168.33.10 : ~p~n",[Reason]);
set_tables([], _Remote) ->
    ok;
set_tables([schema | Tables], Remote) ->
    set_tables(Tables, Remote);
set_tables([Table | Tables], Remote) ->
    %% rpc:call => ram_copies
    set_table_copy(Table, node(), rpc:call(Remote, mnesia, table_info, [Table, storage_type])),
    set_tables(Tables, Remote).

start(Remote) ->
    io:format("~n",[]),
    case net_adm:ping(Remote) of
        pong ->
            mnesia:stop(),
            mnesia:delete_schema([node()]),
            mnesia:start(),
            mnesia:change_config(extra_db_nodes, [Remote]),
            set_table_copy(schema, node(), disc_copies),
            %% rpc:call => [schema, session]
            set_tables(rpc:call(Remote, mnesia, system_info, [tables]), Remote);
        pang ->
            io:format("node ~p is not reachable, please check epmd port, and FIREWALL_WINDOW ports~n", [Remote])
    end.
