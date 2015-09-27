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
            mnesia:stop(),
            mnesia:delete_schema([node()]),
            mnesia:create_schema([node()]),
            mnesia:start(),
            update_tables(),
            mnesia:change_table_copy_type(schema, node(), disc_copies),
            create_table(),
            WebrtcRoomMnesia = #webrtc_room{uuid = "36d685c6-dac4-484a-9865-8dcc75800e50", name = "demo"},
            F = fun() ->
                mnesia:write(WebrtcRoomMnesia)
            end,
            mnesia:transaction(F);
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
    mnesia:add_table_copy(session, node(), ram_copies),

    mnesia:create_table(webrtc_room, [
        {attributes, record_info(fields, webrtc_room)},
        {ram_copies, [node()]}
    ]),
    mnesia:add_table_copy(webrtc_room, node(), ram_copies),

    mnesia:create_table(webrtc_members, [
        {attributes, record_info(fields, webrtc_members)},
        {ram_copies, [node()]}
    ]),
    mnesia:add_table_copy(webrtc_members, node(), ram_copies).

update_tables() ->
    case catch mnesia:table_info(session, attributes) of
        [uid, pid, device, node, register_name] -> mnesia:delete_table(session);
        {'EXIT', _} -> ok
    end,
    case catch mnesia:table_info(webrtc_room, attributes) of
        [uuid, name] -> mnesia:delete_table(webrtc_room);
        {'EXIT', _} -> ok
    end,
    case catch mnesia:table_info(webrtc_members, attributes) of
        [room_uuid, pid, nick_name] -> mnesia:delete_table(webrtc_members);
        {'EXIT', _} -> ok
    end.
