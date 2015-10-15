%%%-------------------------------------------------------------------
%%% @author yang
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%   mysql query api
%%% @end
%%% Created : 27. 九月 2015 下午12:17
%%%-------------------------------------------------------------------
-module(mysql_util).

-export([
    add_firend/3,
    friendships/1,
    blockships/1,
    add_blockship/2,
    del_blockship/2,
    del_friend/2,
    query_user_by_mobile/1,
    query_user_by_id/1,
    change_room_nickname/3,
    change_frined_nickname/3,
    change_room_none_bother/3,
    change_room_bg/3,
    save_msg/1,
    save_room_msg/1,
    save_user_room_msg/2,
    user_msgs/2,
    user_roommsgs/2,
    room_msgs/1,
    query_room_msgs/3,
    mark_read/2,
    mark_read/3,
    query_msg_by_id/1,
    add_member/5,
    add_member/2,
    room_members/1,
    room_members_count/1,
    room/1,
    is_an_exist_room/1,
    is_room_leader/2,
    change_room_leader/1,
    change_room_name/2,
    are_friends/2,
    are_blocks/2,
    in_room/2,
    create_room/1,
    del_room_member/2,
    del_room_members/2,
    del_room/1,
    save_notification/1,
    user_unread_notifications/1
]).

-include("table.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%% @doc user
%% @doc 添加好友
add_firend(Uid, FriendId, NickName) when is_integer(Uid), is_integer(FriendId) ->
    emysql:prepare(have_add_firend_stmt, <<"SELECT * FROM friendships WHERE user_id = ? AND friend_id = ?">>),
    Result = emysql:execute(erlim_pool, have_add_firend_stmt, [Uid, FriendId]),
    case emysql_util:as_record(Result, friendship_record, record_info(fields, friendship_record)) of
        [] ->
            emysql:prepare(add_firend_stmt, <<"INSERT INTO friendships SET user_id = ?, friend_id = ?, nickname = ?, confirmed = ?, created_at = ?, updated_at = ?">>),
            Now = calendar:local_time(),
            emysql:execute(erlim_pool, add_firend_stmt, [Uid, FriendId, NickName, 1, Now, Now]);
        _ -> false
    end.

%% @doc 联系人列表
friendships(Uid) when is_integer(Uid) ->
    emysql:prepare(friendships_stmt, <<"SELECT * FROM friendships WHERE user_id = ? AND confirmed = 1">>),
    Result = emysql:execute(erlim_pool, friendships_stmt, [Uid]),
    case emysql_util:as_record(Result, friendship_record, record_info(fields, friendship_record)) of
        [] -> [];
        Friendships -> Friendships
    end.

%% @doc 黑名单列表
blockships(Uid) when is_integer(Uid) ->
    emysql:prepare(blockships_stmt, <<"SELECT * FROM blockships WHERE user_id = ?">>),
    Result = emysql:execute(erlim_pool, blockships_stmt, [Uid]),
    case emysql_util:as_record(Result, blockship_record, record_info(fields, blockship_record)) of
        [] -> [];
        Blockships -> Blockships
    end.

add_blockship(Uid, FriendId) when is_integer(Uid), is_integer(FriendId) ->
    case are_friends(Uid, FriendId) of
        false -> false;
        true ->
            emysql:prepare(add_blockship_stmt, <<"INSERT INTO blockships SET user_id = ?, block_id = ?, created_at = ?, updated_at = ?">>),
            Now = calendar:local_time(),
            emysql:execute(erlim_pool, add_blockship_stmt, [Uid, FriendId, Now, Now])
    end.

%% @doc 删除黑名单
del_blockship(Uid, Bid) when is_integer(Uid), is_integer(Bid) ->
    emysql:prepare(del_blockship_stmt, <<"DELETE FROM blockships WHERE user_id = ? AND block_id = ?">>),
    emysql:execute(erlim_pool, del_blockship_stmt, [Uid, Bid]).

%% @doc 删除好友
del_friend(Uid, Fid) when is_integer(Uid), is_integer(Fid) ->
    emysql:prepare(del_friend_stmt, <<"DELETE FROM friendships WHERE user_id = ? AND friend_id = ?">>),
    emysql:execute(erlim_pool, del_friend_stmt, [Fid, Uid]).

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

%% @doc 是否已经拉黑
are_blocks(Uid, Bid) when is_integer(Uid), is_integer(Bid) ->
    emysql:prepare(are_blocks_stmt, <<"SELECT * FROM blockships WHERE user_id = ? AND block_id = ?">>),
    Result = emysql:execute(erlim_pool, are_blocks_stmt, [Uid, Bid]),
    case emysql_util:as_record(Result, blockship_record, record_info(fields, blockship_record)) of
        [] -> false;
        _ -> true
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
    case Recs of
        [] -> [];
        [User | _T] -> User
    end.

%% @doc 修改群昵称
change_room_nickname(Uid, RoomId, Nickname) when is_integer(Uid), is_integer(RoomId) ->
    emysql:prepare(change_room_nickname_stmt, <<"UPDATE room_users SET nick_name = ? WHERE user_id = ? AND room_id = ?">>),
    emysql:execute(erlim_pool, change_room_nickname_stmt, [Nickname, Uid, RoomId]).

%% @doc 修改群昵称
change_frined_nickname(Uid, FriendId, Nickname) when is_integer(Uid), is_integer(FriendId) ->
    emysql:prepare(change_room_nickname_stmt, <<"UPDATE friendships SET nickname = ? WHERE user_id = ? AND friend_id = ?">>),
    emysql:execute(erlim_pool, change_room_nickname_stmt, [Nickname, Uid, FriendId]).

%% @doc 修改群消息免打扰
change_room_none_bother(Uid, RoomId, NoneBother) when is_integer(Uid), is_integer(RoomId), is_integer(NoneBother) ->
    emysql:prepare(change_room_none_bother_stmt, <<"UPDATE room_users SET none_bother = ? WHERE user_id = ? AND room_id = ?">>),
    emysql:execute(erlim_pool, change_room_none_bother_stmt, [NoneBother, Uid, RoomId]).

%% @doc 修改群聊背景
change_room_bg(Uid, RoomId, Bg) when is_integer(Uid), is_integer(RoomId) ->
    emysql:prepare(change_room_bg_stmt, <<"UPDATE room_users SET bg = ? WHERE user_id = ? AND room_id = ?">>),
    emysql:execute(erlim_pool, change_room_bg_stmt, [Bg, Uid, RoomId]).

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
room_msgs(RoomId) when is_integer(RoomId) ->
    emysql:prepare(roommsg_stmt, <<"SELECT * FROM roommsgs WHERE t = ?">>),
    Result = emysql:execute(erlim_pool, roommsg_stmt, [RoomId]),
    emysql_util:as_record(Result, roommsg_record, record_info(fields, roommsg_record)).

%% @doc 查询群消息
query_room_msgs(RoomId, LastId, Limit) when is_integer(RoomId), is_integer(LastId), is_integer(Limit) ->
    emysql:prepare(query_room_msgs_stmt, <<"SELECT * FROM roommsgs WHERE t = ? AND id < ? LIMIT ?">>),
    Result = emysql:execute(erlim_pool, query_room_msgs_stmt, [RoomId, LastId, Limit]),
    emysql_util:as_record(Result, roommsg_record, record_info(fields, roommsg_record)).

%% @doc 标记私聊/群聊/通知消息为已读
mark_read(MsgId, single_chat) when is_integer(MsgId) ->
    emysql:prepare(mark_read_single_chat_stmt, <<"UPDATE msgs SET unread = ? WHERE id = ?">>),
    emysql:execute(erlim_pool, mark_read_single_chat_stmt, [0, MsgId]);
mark_read(UserRoommsgId, group_chat) when is_integer(UserRoommsgId) ->
    emysql:prepare(mark_read_group_chat_stmt, <<"UPDATE user_roommsgs SET unread = ? WHERE id = ?">>),
    emysql:execute(erlim_pool, mark_read_group_chat_stmt, [0, UserRoommsgId]);
mark_read(NotificationId, notification) when is_integer(NotificationId) ->
    emysql:prepare(mark_read_notification_stmt, <<"UPDATE notifications SET unread = ? WHERE id = ?">>),
    emysql:execute(erlim_pool, mark_read_notification_stmt, [0, NotificationId]).

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
%% @doc 添加群成员
add_member(RoomId, Uid, NickName, NoneBother, Bg) when is_integer(RoomId), is_integer(Uid) ->
    emysql:prepare(add_member_stmt, <<"INSERT INTO room_users SET room_id = ?, user_id = ?, nick_name = ?, none_bother = ?, bg = ?, created_at = ?, updated_at = ?">>),
    Now = calendar:local_time(),
    emysql:execute(erlim_pool, add_member_stmt, [RoomId, Uid, NickName, NoneBother, Bg, Now, Now]).

add_member(RoomId, Uid) when is_integer(RoomId), is_integer(Uid) ->
    emysql:prepare(add_member_stmt, <<"INSERT INTO room_users SET room_id = ?, user_id = ?, nick_name = ?, none_bother = ?, bg = ?, created_at = ?, updated_at = ?">>),
    Now = calendar:local_time(),
    emysql:execute(erlim_pool, add_member_stmt, [RoomId, Uid, <<"">>, 0, <<"">>, Now, Now]).

%% @doc 群成员
room_members(RoomId) when is_integer(RoomId) ->
    emysql:prepare(room_members_stmt, <<"SELECT * FROM room_users WHERE room_id = ?">>),
    Result = emysql:execute(erlim_pool, room_members_stmt, [RoomId]),
    emysql_util:as_record(Result, room_users_record, record_info(fields, room_users_record)).

%% @doc 群成员数
room_members_count(RoomId) when is_integer(RoomId) ->
    length(room_members(RoomId)).

%% @doc 群
room(RoomId) when is_integer(RoomId) ->
    emysql:prepare(room_stmt, <<"SELECT * FROM rooms WHERE id = ?">>),
    Result = emysql:execute(erlim_pool, room_stmt, [RoomId]),
    Res = emysql_util:as_record(Result, room_record, record_info(fields, room_record)),
    case Res of
        [] -> [];
        [Room | _T] = Res -> Room
    end.

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

%% @doc 是否群主
is_room_leader(RoomId, Uid) when is_integer(RoomId), is_integer(Uid) ->
    emysql:prepare(is_room_leader_stmt, <<"SELECT * FROM rooms WHERE id = ? AND creator = ?">>),
    Result = emysql:execute(erlim_pool, is_room_leader_stmt, [RoomId, Uid]),
    case emysql_util:as_record(Result, room_record, record_info(fields, room_record)) of
        [] ->
            false;
        _ ->
            true
    end.

%% @doc 修改群主
change_room_leader(RoomId) when is_integer(RoomId) ->
    %% 退群以后是否为空群
    case mysql_util:room_members(RoomId) of
        [] -> mysql_util:del_room(RoomId);
        Members ->
            FirstMember = lists:nth(1, Members),
            Uid = FirstMember#room_users_record.user_id,
            emysql:prepare(change_room_leader_stmt, <<"UPDATE rooms SET creator = ? WHERE id = ?">>),
            emysql:execute(erlim_pool, change_room_leader_stmt, [Uid, RoomId])
    end.

%% @doc 修改群名
change_room_name(RoomId, NewName) when is_integer(RoomId) ->
    emysql:prepare(change_room_leader_stmt, <<"UPDATE rooms SET name = ? WHERE id = ?">>),
    emysql:execute(erlim_pool, change_room_leader_stmt, [NewName, RoomId]).

%% @doc 是否在该群
in_room(Uid, Roomid) when is_integer(Uid), is_integer(Roomid) ->
    emysql:prepare(in_room_stmt, <<"SELECT * FROM room_users WHERE user_id = ? AND room_id = ?">>),
    Result = emysql:execute(erlim_pool, in_room_stmt, [Uid, Roomid]),
    case emysql_util:as_record(Result, room_users_record, record_info(fields, room_users_record)) of
        [] -> false;
        _ -> true
    end.

%% @doc 建群
create_room(Room) when is_record(Room, room_record) ->
    emysql:prepare(create_room_stmt, <<"INSERT INTO rooms SET creator = ?, name = ?, max_member_count = ?, invitable = ?, password = ?, description = ?, subject = ?, qrcode = ?, logo = ?, created_at = ?, updated_at = ?">>),
    Now = calendar:local_time(),
    emysql:execute(erlim_pool, create_room_stmt, [Room#room_record.creator, Room#room_record.name, Room#room_record.max_member_count, Room#room_record.invitable, Room#room_record.password, Room#room_record.description, Room#room_record.subject, <<"">>, Room#room_record.logo, Now, Now]).

%% @doc 移除群成员
del_room_member(RoomId, UserId) when is_integer(RoomId), is_integer(UserId) ->
    emysql:prepare(del_room_member_stmt, <<"DELETE FROM room_users WHERE room_id = ? AND user_id = ?">>),
    emysql:execute(erlim_pool, del_room_member_stmt, [RoomId, UserId]).

%% @doc 删除全部群成员
del_room_members(RoomId, MemberIds) when is_integer(RoomId) ->
    lists:foreach(fun(Id) ->
        del_room_member(RoomId, Id)
                  end, MemberIds).

%% @doc 删除全部群成员
del_all_room_members(RoomId) when is_integer(RoomId) ->
    emysql:prepare(del_all_room_members_stmt, <<"DELETE FROM room_users WHERE room_id = ?">>),
    emysql:execute(erlim_pool, del_all_room_members_stmt, [RoomId]).

%% @doc 删除全部群消息
del_all_roommsgs(RoomId) when is_integer(RoomId) ->
    emysql:prepare(del_all_roommsgs_stmt, <<"DELETE FROM roommsgs WHERE t = ?">>),
    emysql:execute(erlim_pool, del_all_roommsgs_stmt, [RoomId]).

%% @doc 删除全部用户群消息
del_all_user_roommsgs(RoomId) when is_integer(RoomId) ->
    Roommsgs = room_msgs(RoomId),
    lists:foreach(fun(Roommsg) ->
        emysql:prepare(del_all_user_roommsgs_stmt, <<"DELETE FROM user_roommsgs WHERE roommsg_id = ?">>),
        emysql:execute(erlim_pool, del_all_user_roommsgs_stmt, [Roommsg#roommsg_record.id])
                  end, Roommsgs).

%% @doc 删除群(先删除所有群消息, 然后删除所有群成员, 最后删除群)
del_room(RoomId) when is_integer(RoomId) ->
    del_all_user_roommsgs(RoomId),
    del_all_roommsgs(RoomId),
    del_all_room_members(RoomId),
    emysql:prepare(del_room_stmt, <<"DELETE FROM rooms WHERE id = ?">>),
    emysql:execute(erlim_pool, del_room_stmt, [RoomId]).

%%% @doc notification
%% @doc 保存通知到数据库
save_notification(NR) when is_record(NR, notification_record) ->
    emysql:prepare(save_notification_stmt, <<"INSERT INTO notifications SET sender_id = ?, receiver_id = ?, notification_type = ?, notifiable_type = ?, notifiable_action = ?, notifiable_id = ?, subject = ?, body = ?, unread = ?, created_at = ?, updated_at = ?">>),
    Now = calendar:local_time(),
    emysql:execute(erlim_pool, save_notification_stmt, [NR#notification_record.sender_id, NR#notification_record.receiver_id, NR#notification_record.notification_type, NR#notification_record.notifiable_type, NR#notification_record.notifiable_action, NR#notification_record.notifiable_id, NR#notification_record.subject, NR#notification_record.body, NR#notification_record.unread, Now, Now]).

%% @doc 未读通知
user_unread_notifications(Uid) when is_integer(Uid) ->
    emysql:prepare(user_unread_notifications_stmt, <<"SELECT * FROM notifications WHERE receiver_id = ? AND unread = 1">>),
    Result = emysql:execute(erlim_pool, user_unread_notifications_stmt, [Uid]),
    emysql_util:as_record(Result, notification_record, record_info(fields, notification_record)).