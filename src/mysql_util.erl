-module(mysql_util).

-export([
    query_user_by_mobile/1,
    query_user_by_id/1,
    save_logout/1,
    save_msg/1,
    save_room_msg/1,
    user_msgs/2,
    room_msgs/1,
    mark_read/2,
    query_msg_by_id/1,
    room_members/1,
    is_an_exist_room/1,
    are_friends/2,
    in_room/2
]).

-include("table.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% 通过手机号查询用户
query_user_by_mobile(Mobile) ->
    emysql:prepare(query_user_by_mobile_stmt, <<"SELECT * FROM users WHERE mobile = ?">>),
    Result = emysql:execute(erlim_pool, query_user_by_mobile_stmt, [Mobile]),
    Recs = emysql_util:as_record(Result, user_record, record_info(fields, user_record)),
    [User | _T] = Recs,
    User.

%% 通过id查询用户
query_user_by_id(Uid) when is_integer(Uid) ->
    emysql:prepare(query_user_by_id_stmt, <<"SELECT * FROM users WHERE id = ?">>),
    Result = emysql:execute(erlim_pool, query_user_by_id_stmt, [Uid]),
    Recs = emysql_util:as_record(Result, user_record, record_info(fields, user_record)),
    [User | _T] = Recs,
    User.

%% 退出时保存时间
save_logout(Uid) when is_integer(Uid) ->
    emysql:prepare(sm_stmt, <<"SELECT * FROM sms WHERE user_id = ?">>),
    Result = emysql:execute(erlim_pool, sm_stmt, [Uid]),
    Now = calendar:local_time(),
    case emysql_util:as_record(Result, sm_record, record_info(fields, sm_record)) of
        [] ->
            emysql:prepare(insert_sm_stmt, <<"INSERT INTO sms SET user_id = ?, last_logout_at = ?, created_at = ?, updated_at = ?">>),
            emysql:execute(erlim_pool, insert_sm_stmt, [Uid, Now, Now, Now]);
        [_Sm] ->
            emysql:prepare(up_sm_stmt, <<"UPDATE sms SET last_logout_at = ?, updated_at = ? WHERE user_id = ?">>),
            emysql:execute(erlim_pool, up_sm_stmt, [Now, Now, Uid])
    end.

%% 保存私聊消息
save_msg(Msg) when is_record(Msg, msg_record) ->
    emysql:prepare(save_msg_stmt, <<"INSERT INTO msgs SET f = ?, t = ?, msg = ?, unread = ?, created_at = ?, updated_at = ?">>),
    Now = calendar:local_time(),
    emysql:execute(erlim_pool, save_msg_stmt, [Msg#msg_record.f, Msg#msg_record.t, Msg#msg_record.msg, Msg#msg_record.unread, Now, Now]).

%% 保存群聊消息
save_room_msg(RoomMsg) when is_record(RoomMsg, roommsg_record) ->
    emysql:prepare(save_room_msg_stmt, <<"INSERT INTO roommsgs SET f = ?, t = ?, msg = ?, created_at = ?, updated_at = ?">>),
    Now = calendar:local_time(),
    emysql:execute(erlim_pool, save_room_msg_stmt, [RoomMsg#roommsg_record.f, RoomMsg#roommsg_record.t, RoomMsg#roommsg_record.msg, Now, Now]).

%% 查询用户已读/未读消息
user_msgs(Uid, Unread) when is_integer(Uid), is_integer(Unread) ->
    emysql:prepare(msg_stmt, <<"SELECT * FROM msgs WHERE t = ? AND unread = ?">>),
    Result = emysql:execute(erlim_pool, msg_stmt, [Uid, Unread]),
    emysql_util:as_record(Result, msg_record, record_info(fields, msg_record)).

%% 查询所有群消息
room_msgs(Id) when is_integer(Id) ->
    emysql:prepare(roommsg_stmt, <<"SELECT * FROM roommsgs WHERE t = ?">>),
    Result = emysql:execute(erlim_pool, roommsg_stmt, [Id]),
    emysql_util:as_record(Result, roommsg_record, record_info(fields, roommsg_record)).

mark_read(MsgId, single_chat) when is_integer(MsgId) ->
    emysql:prepare(mark_read_single_chat_stmt, <<"UPDATE msgs SET unread = ? WHERE id = ?">>),
    emysql:execute(erlim_pool, mark_read_single_chat_stmt, [0, MsgId]);
%% TODO
mark_read(MsgId, group_chat) when is_integer(MsgId) ->
    ok.

query_msg_by_id(MsgId) when is_integer(MsgId) ->
    emysql:prepare(query_msg_by_id_stmt, <<"SELECT * FROM msgs WHERE id = ?">>),
    Result = emysql:execute(erlim_pool, query_msg_by_id_stmt, [MsgId]),
    Recs = emysql_util:as_record(Result, user_record, record_info(fields, user_record)),
    [Msg | _T] = Recs,
    Msg.


%% 群成员
room_members(RoomId) when is_integer(RoomId) ->
    emysql:prepare(room_members_stmt, <<"SELECT * FROM room_users WHERE room_id = ?">>),
    Result = emysql:execute(erlim_pool, room_members_stmt, [RoomId]),
    emysql_util:as_record(Result, room_users_record, record_info(fields, room_users_record)).

is_an_exist_room(Roomid) when is_integer(Roomid) ->
    emysql:prepare(is_an_exist_room_stmt, <<"SELECT * FROM rooms WHERE id = ?">>),
    Result = emysql:execute(erlim_pool, is_an_exist_room_stmt, [Roomid]),
    case emysql_util:as_record(Result, room_record, record_info(fields, room_record)) of
        [] ->
            false;
        _ ->
            true
    end.

%% 是否朋友关系
are_friends(Uid, Fid) when is_integer(Uid), is_integer(Fid) ->
    emysql:prepare(are_friends_stmt, <<"SELECT * FROM friendships WHERE user_id = ? AND friend_id = ? AND confirmed = 1">>),
    Result1 = emysql:execute(erlim_pool, are_friends_stmt, [Uid, Fid]),
    case emysql_util:as_record(Result1, friendship_record, record_info(fields, friendship_record)) of
        [] ->
            false;
        _ ->
            Result2 = emysql:execute(erlim_pool, are_friends_stmt, [Fid, Uid]),
            case emysql_util:as_record(Result2, friendship_record, record_info(fields, friendship_record)) of
                [] ->
                    false;
                _ ->
                    true
            end
    end.

in_room(Uid, Roomid) when is_integer(Uid), is_integer(Roomid) ->
    emysql:prepare(in_room_stmt, <<"SELECT * FROM room_users WHERE user_id = ? AND room_id = ?">>),
    Result = emysql:execute(erlim_pool, in_room_stmt, [Uid, Roomid]),
    case emysql_util:as_record(Result, room_users_record, record_info(fields, room_users_record)) of
        [] -> false;
        _ -> true
    end.