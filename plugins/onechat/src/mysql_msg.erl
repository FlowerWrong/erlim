%%%-------------------------------------------------------------------
%%% @author yy
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 九月 2015 下午3:42
%%%-------------------------------------------------------------------
-module(mysql_msg).
-author("yy").

-include("onechat.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([
    save_msg/1,
    save_room_msg/1,
    save_user_room_msg/2,
    user_msgs/2,
    user_roommsgs/2,
    room_msgs/1,
    mark_read/2,
    mark_read/3,
    query_msg_by_id/1
]).


%% 保存私聊消息
save_msg(Msg) when is_record(Msg, msg_record) ->
    emysql:prepare(save_msg_stmt, <<"INSERT INTO msgs SET f = ?, t = ?, msg = ?, unread = ?, created_at = ?, updated_at = ?">>),
    Now = calendar:local_time(),
    emysql:execute(onechat_pool, save_msg_stmt, [Msg#msg_record.f, Msg#msg_record.t, Msg#msg_record.msg, Msg#msg_record.unread, Now, Now]).

%% 保存群聊消息
save_room_msg(RoomMsg) when is_record(RoomMsg, roommsg_record) ->
    emysql:prepare(save_room_msg_stmt, <<"INSERT INTO roommsgs SET f = ?, t = ?, msg = ?, created_at = ?, updated_at = ?">>),
    Now = calendar:local_time(),
    emysql:execute(onechat_pool, save_room_msg_stmt, [RoomMsg#roommsg_record.f, RoomMsg#roommsg_record.t, RoomMsg#roommsg_record.msg, Now, Now]).

%% 保存用户群聊消息
save_user_room_msg(RoommsgId, UserId) when is_integer(RoommsgId), is_integer(UserId) ->
    emysql:prepare(save_user_room_msg_stmt, <<"INSERT INTO user_roommsgs SET user_id = ?, roommsg_id = ?, unread = ?, created_at = ?, updated_at = ?">>),
    Now = calendar:local_time(),
    emysql:execute(onechat_pool, save_user_room_msg_stmt, [UserId, RoommsgId, 1, Now, Now]).

%% 查询用户已读/未读消息
user_msgs(Uid, Unread) when is_integer(Uid), is_integer(Unread) ->
    emysql:prepare(msg_stmt, <<"SELECT * FROM msgs WHERE t = ? AND unread = ?">>),
    Result = emysql:execute(onechat_pool, msg_stmt, [Uid, Unread]),
    emysql_util:as_record(Result, msg_record, record_info(fields, msg_record)).

%% 查询用户已读/未读群消息
user_roommsgs(Uid, Unread) when is_integer(Uid), is_integer(Unread) ->
    emysql:prepare(user_roommsgs_stmt, <<"SELECT roommsg_id FROM user_roommsgs WHERE user_id = ? AND unread = ?">>),
    UserRoommsgsResult = emysql:execute(onechat_pool, user_roommsgs_stmt, [Uid, Unread]),
    UserRoommsgsRecords = emysql_util:as_record(UserRoommsgsResult, user_roommsgs_record, record_info(fields, user_roommsgs_record)),
    Roommsgs = lists:map(fun(UserRoommsg) ->
        RoommsgId = UserRoommsg#user_roommsgs_record.roommsg_id,
        RoommsgsResult = emysql:execute(onechat_pool, <<"SELECT * FROM roommsgs WHERE id = ?">>, [RoommsgId]),
        RoommsgsRecords = emysql_util:as_record(RoommsgsResult, roommsg_record, record_info(fields, roommsg_record)),
        [Roommsg | _T] = RoommsgsRecords,
        Roommsg
                         end, UserRoommsgsRecords),
    Roommsgs.

%% 查询所有群消息
room_msgs(Id) when is_integer(Id) ->
    emysql:prepare(roommsg_stmt, <<"SELECT * FROM roommsgs WHERE t = ?">>),
    Result = emysql:execute(onechat_pool, roommsg_stmt, [Id]),
    emysql_util:as_record(Result, roommsg_record, record_info(fields, roommsg_record)).

%% 标记私聊/群聊消息为已读
mark_read(MsgId, single_chat) when is_integer(MsgId) ->
    emysql:prepare(mark_read_single_chat_stmt, <<"UPDATE msgs SET unread = ? WHERE id = ?">>),
    emysql:execute(onechat_pool, mark_read_single_chat_stmt, [0, MsgId]);
mark_read(UserRoommsgId, group_chat) when is_integer(UserRoommsgId) ->
    emysql:prepare(mark_read_group_chat_stmt, <<"UPDATE user_roommsgs SET unread = ? WHERE id = ?">>),
    emysql:execute(onechat_pool, mark_read_group_chat_stmt, [0, UserRoommsgId]).

mark_read(RoommsgId, Uid, group_chat) when is_integer(RoommsgId), is_integer(Uid) ->
    emysql:prepare(mark_read_group_chat_stmt, <<"UPDATE user_roommsgs SET unread = ? WHERE user_id = ? AND roommsg_id = ?">>),
    emysql:execute(onechat_pool, mark_read_group_chat_stmt, [0, Uid, RoommsgId]).

%% 查询私聊消息
query_msg_by_id(MsgId) when is_integer(MsgId) ->
    emysql:prepare(query_msg_by_id_stmt, <<"SELECT * FROM msgs WHERE id = ?">>),
    Result = emysql:execute(onechat_pool, query_msg_by_id_stmt, [MsgId]),
    Recs = emysql_util:as_record(Result, user_record, record_info(fields, user_record)),
    [Msg | _T] = Recs,
    Msg.
