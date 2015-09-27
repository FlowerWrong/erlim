%%%-------------------------------------------------------------------
%%% @author yang
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 九月 2015 下午12:17
%%%-------------------------------------------------------------------
-module(webrtc_room).
-author("yang").

-include("table.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
%% http://www.erlang.org/doc/man/pg2.html
-export([create/2, delete/1, join/3, get_members/1, leave/2, rooms/0]).

create(Uuid, Name) when is_binary(Uuid) ->
    WebrtcRoomMnesia = #webrtc_room{uuid = Uuid, name = Name},
    F = fun() ->
        mnesia:write(WebrtcRoomMnesia)
    end,
    mnesia:transaction(F).

delete(RoomUuid) when is_binary(RoomUuid) ->
    Fun = fun() ->
        Query = qlc:q([X || X <- mnesia:table(webrtc_room), X#webrtc_room.uuid =:= RoomUuid]),
        qlc:e(Query)
    end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> false;
        {atomic, [WebRTCRoom]} ->
            F = fun() -> mnesia:delete_object(WebRTCRoom) end,
            mnesia:transaction(F)
    end.

join(RoomUuid, Pid, NickName) when is_binary(RoomUuid), is_pid(Pid) ->
    WebrtcMemberMnesia = #webrtc_members{room_uuid = RoomUuid, pid = Pid, nick_name = NickName},
    F = fun() ->
        mnesia:write(WebrtcMemberMnesia)
    end,
    mnesia:transaction(F).

get_members(RoomUuid) when is_binary(RoomUuid) ->
    Fun = fun() ->
        Query = qlc:q([X || X <- mnesia:table(webrtc_members), X#webrtc_members.room_uuid =:= RoomUuid]),
        qlc:e(Query)
    end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> false;
        {atomic, Members} -> Members
    end.

leave(RoomUuid, Pid) when is_binary(RoomUuid), is_pid(Pid) ->
    Fun = fun() ->
        Query = qlc:q([X || X <- mnesia:table(webrtc_members), X#webrtc_members.room_uuid =:= RoomUuid, X#webrtc_members.pid =:= Pid]),
        qlc:e(Query)
    end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> false;
        {atomic, [WebRTCRoomMember]} ->
            F = fun() -> mnesia:delete_object(WebRTCRoomMember) end,
            mnesia:transaction(F)
    end.

rooms() ->
    Fun = fun() ->
        Query = qlc:q([X || X <- mnesia:table(webrtc_room)]),
        qlc:e(Query)
    end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> false;
        {atomic, WebRTCRooms} -> WebRTCRooms
    end.