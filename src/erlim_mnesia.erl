%%%-------------------------------------------------------------------
%%% @author yy
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 九月 2015 下午3:38
%%%-------------------------------------------------------------------
-module(erlim_mnesia).
-author("yy").

-include("table.hrl").

%% API
-export([init_mnesia/0]).

init_mnesia() ->
    case mnesia:system_info(extra_db_nodes) of
        [] ->
            case mnesia:create_schema([node()]) of
                {error, Reason} ->
                    lager:info("Error reason is ~p~n", [Reason]);
                ok ->
                    application:start(mnesia),
                    update_tables(),
                    create_table()
            end;
        _ ->
            ok
    end,
    mnesia:info().

create_table() ->
    update_tables(),
    mnesia:create_table(session, [
        {attributes, record_info(fields, session)},
        {ram_copies, [node()]}
    ]),
    mnesia:add_table_copy(session, node(), ram_copies).

update_tables() ->
    case catch mnesia:table_info(session, attributes) of
      [uid, pid, device, node] -> mnesia:delete_table(session);
      {'EXIT', _} -> ok
    end.
