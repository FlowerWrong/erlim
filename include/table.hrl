%%%-------------------------------------------------------------------
%%% @author yy
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 九月 2015 下午4:11
%%%-------------------------------------------------------------------
-author("yy").

-record(session, {
    uid,
    pid,
    device,
    node,
    register_name
}).

-record(webrtc_room, {
    uuid,
    name
}).

-record(webrtc_members, {
    pid,
    nick_name,
    room_uuid
}).

%% For mysql record
-record(user_record, {
    id,
    mobile,
    password_digest,
    nick_name,
    uid,
    avatar,
    email,
    created_at,
    updated_at
}).

-record(msg_record, {
    id,
    f,
    t,
    msg,
    unread,
    created_at,
    updated_at
}).

-record(roommsg_record, {
    id,
    f,
    t,
    msg,
    created_at,
    updated_at
}).

-record(room_record, {
    id,
    creator,
    name,
    max_member_count,
    invitable,
    password,
    description,
    subject,
    qrcode,
    created_at,
    updated_at
}).

-record(room_users_record, {
    id,
    room_id,
    user_id,
    nick_name,
    none_bother,
    bg,
    created_at,
    updated_at
}).

-record(user_roommsgs_record, {
    id,
    user_id,
    roommsg_id,
    unread,
    created_at,
    updated_at
}).

-record(friendship_record, {
    id,
    user_id,
    friend_id,
    nickname,
    confirmed,
    created_at,
    updated_at
}).

-record(blockship_record, {
    id,
    user_id,
    block_id,
    created_at,
    updated_at
}).

-record(notification_record, {
    id,
    sender_id,
    receiver_id,
    notification_type,
    notifiable_type,
    notifiable_action,
    notifiable_id,
    subject,
    body,
    read,
    created_at,
    updated_at
}).
