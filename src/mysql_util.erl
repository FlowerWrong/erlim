-module(mysql_util).

-export([
    query_user_by_mobile/1,
    query_user_by_id/1,
    save_logout/1,
    save_msg/1,
    save_room_msg/1,
    user_msgs/2,
    room_msgs/1,
    room_members/1,
    create_room/1,
    is_an_exist_room/1,
    add_member/2,
    add_member/3,
    add_member/4,
    add_member/5,
    add_members/2,
    up_nick/3,
    up_bother/3,
    up_bg/3,
    del_member/2,
    del_members/2,
    del_room/1,
    are_friends/2,
    in_room/2
]).

-include("table.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% 通过手机号查询用户
query_user_by_mobile(Username) ->
    emysql:prepare(query_user_by_mobile_stmt, <<"SELECT * FROM users WHERE mobile = ?">>),
    Result = emysql:execute(erlim_pool, query_user_by_mobile_stmt, [Username]),
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

%% 群成员
room_members(RoomId) when is_integer(RoomId) ->
    emysql:prepare(room_members_stmt, <<"SELECT * FROM room_users WHERE room_id = ?">>),
    Result = emysql:execute(erlim_pool, room_members_stmt, [RoomId]),
    emysql_util:as_record(Result, room_users_record, record_info(fields, room_users_record)).

%%　建群
create_room(Room) when is_record(Room, room_record) ->
    emysql:prepare(create_room_stmt, <<"INSERT INTO rooms SET name = ?, creator = ?, max_member_count = ?, invitable = ?, description = ?, subject = ?, qrcode = ?">>),
    emysql:execute(erlim_pool, create_room_stmt, [Room#room_record.name, Room#room_record.creator, Room#room_record.max_member_count, Room#room_record.invitable, Room#room_record.description, Room#room_record.subject, Room#room_record.qrcode]).

is_an_exist_room(Roomid) when is_integer(Roomid) ->
    emysql:prepare(is_an_exist_room_stmt, <<"SELECT * FROM rooms WHERE id = ?">>),
    Result = emysql:execute(erlim_pool, is_an_exist_room_stmt, [Roomid]),
    case emysql_util:as_record(Result, room_record, record_info(fields, room_record)) of
        [] ->
            false;
        _ ->
            true
    end.

%% 添加群成员
add_member(Uid, Rid) when is_integer(Rid), is_integer(Uid) ->
    add_member(Uid, Rid, "", 0, "").
add_member(Uid, Rid, NickName) when is_integer(Rid), is_integer(Uid) ->
    add_member(Uid, Rid, NickName, 0, "").
add_member(Uid, Rid, NickName, NoneBother) when is_integer(Rid), is_integer(Uid) ->
    add_member(Uid, Rid, NickName, NoneBother, "").
add_member(Uid, Rid, NickName, NoneBother, Bg) when is_integer(Rid), is_integer(Uid) ->
    emysql:prepare(add_room_stmt, <<"INSERT INTO room_users SET user_id = ?, room_id = ?, nick_name = ?, none_bother = ?, bg = ?">>),
    emysql:execute(erlim_pool, add_room_stmt, [Uid, Rid, NickName, NoneBother, Bg]).

%% 批量添加群成员
add_members(Uids, Rid) when is_integer(Rid), is_list(Uids) ->
    lists:foreach(fun(Uid)->
        add_member(Uid, Rid)
    end, Uids).

%% 更新群昵称
up_nick(NickName, Rid, Uid) when is_integer(Rid), is_integer(Uid) ->
    emysql:prepare(up_nick_stmt, <<"UPDATE room_users SET nick_name = ? WHERE user_id = ? AND room_id = ?">>),
    emysql:execute(erlim_pool, up_nick_stmt, [NickName, Uid, Rid]).

%%　设置消息免打扰
up_bother(NoneBother, Rid, Uid) when is_integer(Rid), is_integer(Uid) ->
    emysql:prepare(up_bother_stmt, <<"UPDATE room_users SET none_bother = ? WHERE user_id = ? AND room_id = ?">>),
    emysql:execute(erlim_pool, up_bother_stmt, [NoneBother, Uid, Rid]).

%%　设置群聊背景
up_bg(Bg, Rid, Uid) when is_integer(Rid), is_integer(Uid) ->
    emysql:prepare(up_bg_stmt, <<"UPDATE room_users SET bg = ? WHERE user_id = ? AND room_id = ?">>),
    emysql:execute(erlim_pool, up_bg_stmt, [Bg, Uid, Rid]).

%% 删除群成员
del_member(Uid, Rid) when is_integer(Rid), is_integer(Uid) ->
    emysql:prepare(del_member_stmt, <<"DELETE FROM room_users WHERE user_id = ? AND room_id = ?">>),
    emysql:execute(erlim_pool, del_member_stmt, [Uid, Rid]).

%%　批量删除群成员
del_members(Uids, Rid) when is_integer(Rid), is_list(Uids) ->
    lists:foreach(fun(Uid)->
        del_member(Uid, Rid)
    end, Uids).

%% 删除所有群成员
del_all_members(Rid) when is_integer(Rid) ->
    emysql:prepare(del_all_members_stmt, <<"DELETE FROM room_users WHERE room_id = ?">>),
    emysql:execute(erlim_pool, del_all_members_stmt, [Rid]).

%% 删除所有群消息
del_all_roommsgs(Rid) when is_integer(Rid) ->
    emysql:prepare(del_all_roommsgs_stmt, <<"DELETE FROM roommsgs WHERE t = ?">>),
    emysql:execute(erlim_pool, del_all_roommsgs_stmt, [Rid]).

%%　删除群
del_room(Rid) when is_integer(Rid) ->
    del_all_roommsgs(Rid),
    del_all_members(Rid),
    emysql:prepare(del_room_stmt, <<"DELETE FROM rooms WHERE id = ?">>),
    emysql:execute(erlim_pool, del_room_stmt, [Rid]).


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