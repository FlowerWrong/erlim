-module(leave_cluster).
-export([start/0]).

del_table_copy(Table, Node) ->
    case mnesia:del_table_copy(Table, Node) of
    {aborted, Reason} -> io:format("Error: can not remove ~p table: ~p~n", [Table, Reason]);
    _ -> io:format("table ~p removed from cluster~n", [Table])
    end.

del_tables([],_) ->
    ok;
del_tables([schema | Tables], Node) ->
    del_tables(Tables, Node);
del_tables([Table | Tables], Node) ->
    del_table_copy(Table, Node),
    del_tables(Tables, Node).

start() ->
    io:format("~n",[]),
    Removed = node(),
    case mnesia:system_info(running_db_nodes)--[Removed] of
        [] -> io:format("Error: no other node running in the cluster~n");
        Nodes ->
            del_tables(mnesia:system_info(local_tables), Removed),
            mnesia:stop(),
            io:format("Nodes are ~p~n", [Nodes]),
            case rpc:call(hd(Nodes), mnesia, del_table_copy, [schema, Removed]) of
                {badrpc, Reason} -> io:format("Error: can not unregister node ~p from cluster: ~p~n", [Removed, Reason]);
                {aborted, Reason} -> io:format("Error: can not unregister node ~p from cluster: ~p~n", [Removed, Reason]);
                {atomic, ok} ->
                    mnesia:delete_schema([Removed]),
                    io:format("node ~p removed from cluster~n", [Removed])
                end
    end.
