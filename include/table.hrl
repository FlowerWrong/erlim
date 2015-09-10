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
    name,               % user name
    token,              % token
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
    from,
    to,
    msg,
    unread
}).

-record(roommsg_record, {
    id,
    from,
    to,
    msg
}).
