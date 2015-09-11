-module(mysql_util).

-export([query_user_by_mobile/1, save_msg/1, save_room_msg/1]).

-include("table.hrl").
-include_lib("stdlib/include/qlc.hrl").

query_user_by_mobile(Username) ->
    emysql:prepare(query_user_stmt, <<"SELECT * FROM users WHERE mobile = ?">>),
    Result = emysql:execute(erlim_pool, query_user_stmt, [Username]),
    Recs = emysql_util:as_record(Result, user_record, record_info(fields, user_record)),
    [User | _T] = Recs,
    User.

save_msg(Msg) ->
    io:format("Msg is ~p~n", [Msg]),
    emysql:prepare(save_msg_stmt, <<"INSERT INTO msgs SET f = ?, t = ?, msg = ?, unread = ?">>),
    emysql:execute(erlim_pool, save_msg_stmt, [Msg#msg_record.f, Msg#msg_record.t, Msg#msg_record.msg, Msg#msg_record.unread]).

save_room_msg(RoomMsg) ->
    emysql:prepare(save_room_msg_stmt, <<"INSERT INTO roommsgs SET f = ?, t = ?, msg = ?">>),
    emysql:execute(erlim_pool, save_room_msg_stmt, [RoomMsg#roommsg_record.f, RoomMsg#roommsg_record.t, RoomMsg#roommsg_record.msg]).
