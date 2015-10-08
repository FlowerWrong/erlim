%%%-------------------------------------------------------------------
%%% @author yy
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%   onechat records
%%% @end
%%% Created : 02. 九月 2015 下午4:11
%%%-------------------------------------------------------------------
-author("yy").

-record(session, {
    uid :: integer(),
    pid :: pid(),
    device :: binary(),
    node :: node(),
    register_name :: binary()
}).

-record(webrtc_room, {
    uuid :: binary(),
    name :: binary()
}).

-record(webrtc_members, {
    pid :: pid(),
    nick_name :: binary(),
    room_uuid :: binary()
}).

%% For mysql record
-record(user_record, {
    id :: integer(),
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
    id :: integer(),
    f :: integer(),
    t :: integer(),
    msg,
    unread :: integer(),
    created_at,
    updated_at
}).

-record(roommsg_record, {
    id :: integer(),
    f :: integer(),
    t :: integer(),
    msg,
    created_at,
    updated_at
}).

-record(room_record, {
    id :: integer(),
    creator :: integer(),
    name,
    max_member_count :: integer(),
    invitable :: integer(),
    password,
    description,
    subject,
    qrcode,
    created_at,
    updated_at
}).

-record(room_users_record, {
    id :: integer(),
    room_id :: integer(),
    user_id :: integer(),
    nick_name,
    none_bother,
    bg,
    created_at,
    updated_at
}).

-record(user_roommsgs_record, {
    id :: integer(),
    user_id :: integer(),
    roommsg_id :: integer(),
    unread :: integer(),
    created_at,
    updated_at
}).

-record(friendship_record, {
    id :: integer(),
    user_id :: integer(),
    friend_id :: integer(),
    nickname,
    confirmed :: integer(),
    created_at,
    updated_at
}).

-record(blockship_record, {
    id :: integer(),
    user_id :: integer(),
    block_id :: integer(),
    created_at,
    updated_at
}).

-record(notification_record, {
    id :: integer(),
    sender_id :: integer(),
    receiver_id :: integer(),
    notification_type,
    notifiable_type,
    notifiable_action,
    notifiable_id :: integer(),
    subject,
    body,
    unread :: integer(),
    created_at,
    updated_at
}).
