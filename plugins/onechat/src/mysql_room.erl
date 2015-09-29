%%%-------------------------------------------------------------------
%%% @author yy
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 九月 2015 下午3:42
%%%-------------------------------------------------------------------
-module(mysql_room).
-author("yy").

-include("onechat.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([
    room_members/1,
    is_an_exist_room/1,
    in_room/2
]).


%% 群成员
room_members(RoomId) when is_integer(RoomId) ->
    emysql:prepare(room_members_stmt, <<"SELECT * FROM room_users WHERE room_id = ?">>),
    Result = emysql:execute(onechat_pool, room_members_stmt, [RoomId]),
    emysql_util:as_record(Result, room_users_record, record_info(fields, room_users_record)).

%% 该群是否存在
is_an_exist_room(Roomid) when is_integer(Roomid) ->
    emysql:prepare(is_an_exist_room_stmt, <<"SELECT * FROM rooms WHERE id = ?">>),
    Result = emysql:execute(onechat_pool, is_an_exist_room_stmt, [Roomid]),
    case emysql_util:as_record(Result, room_record, record_info(fields, room_record)) of
        [] ->
            false;
        _ ->
            true
    end.

%% 是否在该群
in_room(Uid, Roomid) when is_integer(Uid), is_integer(Roomid) ->
    emysql:prepare(in_room_stmt, <<"SELECT * FROM room_users WHERE user_id = ? AND room_id = ?">>),
    Result = emysql:execute(onechat_pool, in_room_stmt, [Uid, Roomid]),
    case emysql_util:as_record(Result, room_users_record, record_info(fields, room_users_record)) of
        [] -> false;
        _ -> true
    end.