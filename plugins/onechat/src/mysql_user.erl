-module(mysql_user).

-export([
    query_user_by_mobile/1,
    query_user_by_id/1,
    friendships/1,
    blockships/1,
    are_friends/2
]).

-include("onechat.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% 通过手机号查询用户
query_user_by_mobile(Mobile) ->
    emysql:prepare(query_user_by_mobile_stmt, <<"SELECT * FROM users WHERE mobile = ?">>),
    Result = emysql:execute(onechat_pool, query_user_by_mobile_stmt, [Mobile]),
    Recs = emysql_util:as_record(Result, user_record, record_info(fields, user_record)),
    [User | _T] = Recs,
    User.

%% 通过id查询用户
query_user_by_id(Uid) when is_integer(Uid) ->
    emysql:prepare(query_user_by_id_stmt, <<"SELECT * FROM users WHERE id = ?">>),
    Result = emysql:execute(onechat_pool, query_user_by_id_stmt, [Uid]),
    Recs = emysql_util:as_record(Result, user_record, record_info(fields, user_record)),
    [User | _T] = Recs,
    User.

%% 联系人列表
friendships(Uid) when is_integer(Uid) ->
    emysql:prepare(friendships_stmt, <<"SELECT * FROM friendships WHERE user_id = ? AND confirmed = 1">>),
    Result = emysql:execute(onechat_pool, friendships_stmt, [Uid]),
    case emysql_util:as_record(Result, friendship_record, record_info(fields, friendship_record)) of
        [] -> [];
        Friendships -> Friendships
    end.

%% 黑名单列表
blockships(Uid) when is_integer(Uid) ->
    emysql:prepare(blockships_stmt, <<"SELECT * FROM blockships WHERE user_id = ?">>),
    Result = emysql:execute(onechat_pool, blockships_stmt, [Uid]),
    case emysql_util:as_record(Result, blockship_record, record_info(fields, blockship_record)) of
        [] -> [];
        Blockships -> Blockships
    end.


%% 是否朋友关系
are_friends(Uid, Fid) when is_integer(Uid), is_integer(Fid) ->
    emysql:prepare(are_friends_stmt, <<"SELECT * FROM friendships WHERE user_id = ? AND friend_id = ? AND confirmed = 1">>),
    Result1 = emysql:execute(onechat_pool, are_friends_stmt, [Uid, Fid]),
    case emysql_util:as_record(Result1, friendship_record, record_info(fields, friendship_record)) of
        [] ->
            false;
        _ ->
            Result2 = emysql:execute(onechat_pool, are_friends_stmt, [Fid, Uid]),
            case emysql_util:as_record(Result2, friendship_record, record_info(fields, friendship_record)) of
                [] ->
                    false;
                _ ->
                    true
            end
    end.
