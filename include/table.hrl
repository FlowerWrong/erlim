%%%-------------------------------------------------------------------
%%% @author yy
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 九月 2015 下午4:11
%%%-------------------------------------------------------------------
-author("yy").

-record(user, {
    uid,                % user mysql id
    pid                 % client pid
}).

%% For mysql record
-record(user_record, {
    id,
    mobile,
    password_digest,
    nick_name,
    uid,
    avatar,
    email
}).

-record(msg_record, {
    id,
    f,
    t,
    msg,
    unread
}).

-record(roommsg_record, {
    id,
    f,
    t,
    msg
}).

-record(room_record, {
    id,
    creator,
    name,
    max_member_count,
    invitable,
    description,
    subject,
    qrcode
}).

-record(room_users_record, {
    id,
    room_id,
    user_id,
    nick_name,
    none_bother,
    bg
}).

-record(sm_record, {
    id,
    name,
    pid,
    last_logout_at,
    user_id
}).

-record(user_roommsgs_record, {
    id,
    user_id,
    roommsg_id,
    unread
}).

-record(friendship_record, {
    id,
    user_id,
    friend_id,
    confirmed
}).
