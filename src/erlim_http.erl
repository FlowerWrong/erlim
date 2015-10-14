%%%-------------------------------------------------------------------
%%% @author yy
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%   erlim http req, Thanks <a href="https://github.com/emqtt/emqttd/blob/master/src/emqttd_http.erl">erylee<a>
%%%   and <a href="https://github.com/mochi/mochiweb/blob/master/examples/https/https_store.erl">mochi</a>
%%% @end
%%% Created : 14. 十月 2015 上午10:09
%%%-------------------------------------------------------------------
-module(erlim_http).
-author("yy").

-include("table.hrl").

%% API
-export([dispatch/1]).

%% @doc <a href="https://gist.github.com/dry/4188894">router</a>
dispatch(Req) ->
    [{rc, RedisClientPid}] = ets:lookup(redis_client, rc),
    QueryParams = Req:parse_qs(),
    case lists:keyfind("token", 1, QueryParams) of
        false ->
            Req:respond({401, [], []});
        {"token", Token} ->
            {ok, UidBinary} = eredis:q(RedisClientPid, ["GET", Token]),
            Uid = binary_to_integer(UidBinary),
            CurrentUser = mysql_util:query_user_by_id(Uid),
            case CurrentUser of
                [] -> Req:respond({401, [], []});
                _ -> handle_request(Req:get(method), Req:get(path), Req, CurrentUser)
            end
    end.

%% @doc 联系人列表
handle_request('GET', "/api/v1/contacts", Req, CurrentUser) ->
    CurrentUserId = CurrentUser#user_record.id,
    Friendships = mysql_util:friendships(CurrentUserId),
    FriendshipsForJson = lists:map(fun(M) ->
        {[{id, M#friendship_record.id}, {user_id, M#friendship_record.user_id}, {friend_id, M#friendship_record.friend_id}]}
                                   end, Friendships),
    Json = jiffy:encode({[{<<"status">>, <<"ok">>}, {<<"msg">>, <<"contacts list">>}, {<<"data">>, FriendshipsForJson}]}),
    Req:ok({"application/json", Json});

%% @doc 黑名单列表
handle_request('GET', "/api/v1/blocks", Req, CurrentUser) ->
    CurrentUserId = CurrentUser#user_record.id,
    Blockships = mysql_util:blockships(CurrentUserId),
    BlockshipsForJson = lists:map(fun(M) ->
        {[{id, M#blockship_record.id}, {user_id, M#blockship_record.user_id}, {block_id, M#blockship_record.block_id}]}
                                  end, Blockships),
    Json = jiffy:encode({[{<<"status">>, <<"ok">>}, {<<"msg">>, <<"blocks list">>}, {<<"data">>, BlockshipsForJson}]}),
    Req:ok({"application/json", Json});

%% @doc 获取群基本信息, 包括群基本信息和群成员
%% query params: room_id:integer
handle_request('GET', "/api/v1/rooms", Req, CurrentUser) ->
    CurrentUserId = CurrentUser#user_record.id,
    QueryParams = Req:parse_qs(),
    RoomIdList = case lists:keyfind("room_id", 1, QueryParams) of
                     false -> Req:respond({400, [], []});
                     {"room_id", RoomIdListTmp} -> RoomIdListTmp
                 end,
    RoomId = list_to_integer(RoomIdList),

    case mysql_util:in_room(CurrentUserId, RoomId) of
        false -> Req:respond({403, [], []});
        true ->
            RR = mysql_util:room(RoomId),
            {datetime, CreatedAtD} = RR#room_record.created_at,
            CreatedAt = util:datetime2timestamp(CreatedAtD),

            RoomMembersRecords = mysql_util:room_members(RoomId),
            RoomMembersForJson = lists:map(fun(M) ->
                {[{id, M#room_users_record.id}, {user_id, M#room_users_record.user_id}, {room_id, M#room_users_record.room_id}, {nick_name, M#room_users_record.nick_name}, {none_bother, M#room_users_record.none_bother}, {bg, M#room_users_record.bg}]}
                                           end, RoomMembersRecords),

            RoomJson = {[{<<"id">>, RR#room_record.id}, {<<"creator">>, RR#room_record.creator}, {<<"name">>, RR#room_record.name}, {<<"invitable">>, RR#room_record.invitable}, {<<"description">>, RR#room_record.description}, {<<"subject">>, RR#room_record.subject}, {<<"qrcode">>, RR#room_record.qrcode}, {<<"logo">>, RR#room_record.logo}, {<<"created_at">>, CreatedAt}, {<<"members">>, RoomMembersForJson}]},
            Json = jiffy:encode({[{<<"status">>, <<"ok">>}, {<<"msg">>, <<"room info">>}, {<<"data">>, RoomJson}]}),
            Req:ok({"application/json", Json})
    end;

%% @doc 修改群昵称
%% params: room_id:integer, nickname:staring
%% test: curl -i -X PUT "http://127.0.0.1:8088/api/v1/users/room/nickname?token=token" -d "room_id=1&nickname=yangyang"
handle_request('PUT', "/api/v1/users/room/nickname", Req, CurrentUser) ->
    CurrentUserId = CurrentUser#user_record.id,
    PostParams = mochiweb_request:parse_post(Req),
    {"room_id", RoomIdList} = lists:keyfind("room_id", 1, PostParams),
    RoomId = list_to_integer(RoomIdList),
    {"nickname", Nickname} = lists:keyfind("nickname", 1, PostParams),
    mysql_util:change_room_nickname(CurrentUserId, RoomId, Nickname),
    Json = jiffy:encode({[{<<"status">>, <<"ok">>}, {<<"msg">>, <<"update room nickname success">>}, {<<"data">>, list_to_binary(Nickname)}]}),
    Req:ok({"application/json", Json});

%% @TODO 修改联系人昵称
%% @TODO 修改消息免打扰
%% @TODO 修改群聊天背景
%% @TODO 举报群
%% @TODO 查找群聊天记录


%% @TODO 获取联系人基本信息
%% @TODO 添加联系人黑名单, 无需对方同意, 无通知给对方
%% @TODO 移除联系人黑名单, 无需对方同意, 无通知给对方

%% @TODO 图片上传

%% @doc online users count
handle_request('GET', "/api/v1/admin/users/online", Req, _CurrentUser) ->
    OnlineUserCount = mnesia_util:online_members_count(),
    Res = {[{<<"status">>, <<"ok">>}, {<<"msg">>, <<"online_members_count">>}, {<<"data">>, OnlineUserCount}]},
    Json = jiffy:encode(Res),
    Req:ok({"application/json", Json});

%% @doc 404 handler
handle_request(Method, Path, Req, _CurrentUser) ->
    lager:error("Unexpected HTTP Request: ~s ~s", [Method, Path]),
    Req:not_found().
