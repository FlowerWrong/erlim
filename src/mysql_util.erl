-module(mysql_util).

-export([
    add_firend/3,
    query_user_by_mobile/1,
    query_user_by_id/1,
    save_msg/1,
    save_room_msg/1,
    save_user_room_msg/2,
    user_msgs/2,
    user_roommsgs/2,
    room_msgs/1,
    mark_read/2,
    mark_read/3,
    query_msg_by_id/1,
    room_members/1,
    is_an_exist_room/1,
    are_friends/2,
    in_room/2,
    save_notification/1
]).

-include("table.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%% @doc user
%% 添加好友
add_firend(Uid, FriendId, NickName) when is_integer(Uid), is_integer(FriendId) ->
    emysql:prepare(add_firend_stmt, <<"INSERT INTO friendships SET user_id = ?, friend_id = ?, nickname = ?, confirmed = ?, created_at = ?, updated_at = ?">>),
    Now = calendar:local_time(),
    emysql:execute(erlim_pool, add_firend_stmt, [Uid, FriendId, NickName, 1, Now, Now]).


%% @doc 是否朋友关系
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

%% @doc 通过手机号查询用户
query_user_by_mobile(Mobile) ->
    emysql:prepare(query_user_by_mobile_stmt, <<"SELECT * FROM users WHERE mobile = ?">>),
    Result = emysql:execute(erlim_pool, query_user_by_mobile_stmt, [Mobile]),
    Recs = emysql_util:as_record(Result, user_record, record_info(fields, user_record)),
    [User | _T] = Recs,
    User.

%% @doc 通过id查询用户
query_user_by_id(Uid) when is_integer(Uid) ->
    emysql:prepare(query_user_by_id_stmt, <<"SELECT * FROM users WHERE id = ?">>),
    Result = emysql:execute(erlim_pool, query_user_by_id_stmt, [Uid]),
    Recs = emysql_util:as_record(Result, user_record, record_info(fields, user_record)),
    [User | _T] = Recs,
    User.

%%% @doc msg
%% @doc 保存私聊消息
save_msg(Msg) when is_record(Msg, msg_record) ->
    emysql:prepare(save_msg_stmt, <<"INSERT INTO msgs SET f = ?, t = ?, msg = ?, unread = ?, created_at = ?, updated_at = ?">>),
    Now = calendar:local_time(),
    emysql:execute(erlim_pool, save_msg_stmt, [Msg#msg_record.f, Msg#msg_record.t, Msg#msg_record.msg, Msg#msg_record.unread, Now, Now]).

%% @doc 保存群聊消息
save_room_msg(RoomMsg) when is_record(RoomMsg, roommsg_record) ->
    emysql:prepare(save_room_msg_stmt, <<"INSERT INTO roommsgs SET f = ?, t = ?, msg = ?, created_at = ?, updated_at = ?">>),
    Now = calendar:local_time(),
    emysql:execute(erlim_pool, save_room_msg_stmt, [RoomMsg#roommsg_record.f, RoomMsg#roommsg_record.t, RoomMsg#roommsg_record.msg, Now, Now]).

%% @doc 保存用户群聊消息
save_user_room_msg(RoommsgId, UserId) when is_integer(RoommsgId), is_integer(UserId) ->
    emysql:prepare(save_user_room_msg_stmt, <<"INSERT INTO user_roommsgs SET user_id = ?, roommsg_id = ?, unread = ?, created_at = ?, updated_at = ?">>),
    Now = calendar:local_time(),
    emysql:execute(erlim_pool, save_user_room_msg_stmt, [UserId, RoommsgId, 1, Now, Now]).

%% @doc 查询用户已读/未读消息
user_msgs(Uid, Unread) when is_integer(Uid), is_integer(Unread) ->
    emysql:prepare(msg_stmt, <<"SELECT * FROM msgs WHERE t = ? AND unread = ?">>),
    Result = emysql:execute(erlim_pool, msg_stmt, [Uid, Unread]),
    emysql_util:as_record(Result, msg_record, record_info(fields, msg_record)).

%% @doc 查询用户已读/未读群消息
user_roommsgs(Uid, Unread) when is_integer(Uid), is_integer(Unread) ->
    emysql:prepare(user_roommsgs_stmt, <<"SELECT roommsg_id FROM user_roommsgs WHERE user_id = ? AND unread = ?">>),
    UserRoommsgsResult = emysql:execute(erlim_pool, user_roommsgs_stmt, [Uid, Unread]),
    UserRoommsgsRecords = emysql_util:as_record(UserRoommsgsResult, user_roommsgs_record, record_info(fields, user_roommsgs_record)),
    Roommsgs = lists:map(fun(UserRoommsg) ->
        RoommsgId = UserRoommsg#user_roommsgs_record.roommsg_id,
        RoommsgsResult = emysql:execute(erlim_pool, <<"SELECT * FROM roommsgs WHERE id = ?">>, [RoommsgId]),
        RoommsgsRecords = emysql_util:as_record(RoommsgsResult, roommsg_record, record_info(fields, roommsg_record)),
        [Roommsg | _T] = RoommsgsRecords,
        Roommsg
                         end, UserRoommsgsRecords),
    Roommsgs.

%% @doc 查询所有群消息
room_msgs(Id) when is_integer(Id) ->
    emysql:prepare(roommsg_stmt, <<"SELECT * FROM roommsgs WHERE t = ?">>),
    Result = emysql:execute(erlim_pool, roommsg_stmt, [Id]),
    emysql_util:as_record(Result, roommsg_record, record_info(fields, roommsg_record)).

%% @doc 标记私聊/群聊消息为已读
mark_read(MsgId, single_chat) when is_integer(MsgId) ->
    emysql:prepare(mark_read_single_chat_stmt, <<"UPDATE msgs SET unread = ? WHERE id = ?">>),
    emysql:execute(erlim_pool, mark_read_single_chat_stmt, [0, MsgId]);
mark_read(UserRoommsgId, group_chat) when is_integer(UserRoommsgId) ->
    emysql:prepare(mark_read_group_chat_stmt, <<"UPDATE user_roommsgs SET unread = ? WHERE id = ?">>),
    emysql:execute(erlim_pool, mark_read_group_chat_stmt, [0, UserRoommsgId]).

%% @doc 标记私聊/群聊消息为已读
mark_read(RoommsgId, Uid, group_chat) when is_integer(RoommsgId), is_integer(Uid) ->
    emysql:prepare(mark_read_group_chat_stmt, <<"UPDATE user_roommsgs SET unread = ? WHERE user_id = ? AND roommsg_id = ?">>),
    emysql:execute(erlim_pool, mark_read_group_chat_stmt, [0, Uid, RoommsgId]).

%% @doc 查询私聊消息
query_msg_by_id(MsgId) when is_integer(MsgId) ->
    emysql:prepare(query_msg_by_id_stmt, <<"SELECT * FROM msgs WHERE id = ?">>),
    Result = emysql:execute(erlim_pool, query_msg_by_id_stmt, [MsgId]),
    Recs = emysql_util:as_record(Result, user_record, record_info(fields, user_record)),
    [Msg | _T] = Recs,
    Msg.

%%% @doc room
%% @doc 群成员
room_members(RoomId) when is_integer(RoomId) ->
    emysql:prepare(room_members_stmt, <<"SELECT * FROM room_users WHERE room_id = ?">>),
    Result = emysql:execute(erlim_pool, room_members_stmt, [RoomId]),
    emysql_util:as_record(Result, room_users_record, record_info(fields, room_users_record)).

%% @doc 该群是否存在
is_an_exist_room(Roomid) when is_integer(Roomid) ->
    emysql:prepare(is_an_exist_room_stmt, <<"SELECT * FROM rooms WHERE id = ?">>),
    Result = emysql:execute(erlim_pool, is_an_exist_room_stmt, [Roomid]),
    case emysql_util:as_record(Result, room_record, record_info(fields, room_record)) of
        [] ->
            false;
        _ ->
            true
    end.

%% @doc 是否在该群
in_room(Uid, Roomid) when is_integer(Uid), is_integer(Roomid) ->
    emysql:prepare(in_room_stmt, <<"SELECT * FROM room_users WHERE user_id = ? AND room_id = ?">>),
    Result = emysql:execute(erlim_pool, in_room_stmt, [Uid, Roomid]),
    case emysql_util:as_record(Result, room_users_record, record_info(fields, room_users_record)) of
        [] -> false;
        _ -> true
    end.


%%% @doc notification
%% @TODO SQL error
save_notification(NR) when is_record(NR, notification_record) ->
    emysql:prepare(save_notification_stmt, <<"INSERT INTO notifications SET sender_id = ?, receiver_id = ?, notification_type = ?, notifiable_type = ?, notifiable_action = ?, notifiable_id = ?, subject = ?, body = ?, read = ?, created_at = ?, updated_at = ?">>),
    Now = calendar:local_time(),
    emysql:execute(erlim_pool, save_notification_stmt, [NR#notification_record.sender_id, NR#notification_record.receiver_id, NR#notification_record.notification_type, NR#notification_record.notifiable_type, NR#notification_record.notifiable_action, NR#notification_record.notifiable_id, NR#notification_record.subject, NR#notification_record.body, NR#notification_record.read, Now, Now]).