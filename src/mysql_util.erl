-module(mysql_util).

-export([query_user_by_mobile/1]).

-include("table.hrl").
-include_lib("stdlib/include/qlc.hrl").

query_user_by_mobile(Username) ->
    emysql:prepare(query_user_stmt, <<"SELECT * FROM users WHERE mobile = ?">>),
    Result = emysql:execute(erlim_pool, query_user_stmt, [Username]),
    Recs = emysql_util:as_record(Result, user_record, record_info(fields, user_record)),
    [User | _T] = Recs,
    User.

save_msg(Msg) ->
    emysql:prepare(save_msg_stmt, <<"INSERT INTO msgs SET from = ?, to = ?, msg = ?, unread = ?">>),
    emysql:execute(erlim_pool, save_msg_stmt, [Msg#msg_record.from, Msg#msg_record.to, Msg#msg_record.msg, Msg#msg_record.unread]).

save_room_msg(RoomMsg) ->
    emysql:prepare(save_room_msg_stmt, <<"INSERT INTO roommsgs SET from = ?, to = ?, msg = ?">>),
    emysql:execute(erlim_pool, save_room_msg_stmt, [RoomMsg#roommsg_record.from, RoomMsg#roommsg_record.to, RoomMsg#roommsg_record.msg]).
