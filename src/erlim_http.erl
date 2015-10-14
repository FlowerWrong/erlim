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


dispatch(Req) ->
    handle_request(Req:get(method), Req:get(path), Req).


%% @doc 联系人列表
handle_request('GET', "/api/v1/contacts", Req) ->
    Friendships = mysql_util:friendships(1),
    FriendshipsForJson = lists:map(fun(M) ->
        {[{id, M#friendship_record.id}, {user_id, M#friendship_record.user_id}, {friend_id, M#friendship_record.friend_id}]}
                                   end, Friendships),
    Json = jiffy:encode({[{<<"status">>, <<"ok">>}, {<<"msg">>, <<"contacts list">>}, {<<"data">>, FriendshipsForJson}]}),
    Req:ok({"application/json", Json});

%% @doc 黑名单列表
handle_request('GET', "/api/v1/blocks", Req) ->
    Blockships = mysql_util:blockships(1),
    BlockshipsForJson = lists:map(fun(M) ->
        {[{id, M#blockship_record.id}, {user_id, M#blockship_record.user_id}, {block_id, M#blockship_record.block_id}]}
                                  end, Blockships),
    Json = jiffy:encode({[{<<"status">>, <<"ok">>}, {<<"msg">>, <<"blocks list">>}, {<<"data">>, BlockshipsForJson}]}),
    Req:ok({"application/json", Json});

%% @TODO 获取群基本信息
%% @TODO 修改群昵称
%% @TODO 修改联系人昵称
%% @TODO 修改消息免打扰
%% @TODO 修改群聊天背景
%% @TODO 举报群
%% @TODO 查找群聊天记录


%% @TODO 获取联系人基本信息
%% @TODO 添加联系人黑名单, 无需对方同意, 无通知给对方
%% @TODO 移除联系人黑名单, 无需对方同意, 无通知给对方

%% @doc online users count
handle_request('GET', "/api/v1/users/online", Req) ->
    OnlineUserCount = mnesia_util:online_members_count(),
    Res = {[{<<"status">>, <<"ok">>}, {<<"msg">>, <<"online_members_count">>}, {<<"data">>, OnlineUserCount}]},
    Json = jiffy:encode(Res),
    Req:ok({"application/json", Json});

%% @doc 404 handler
handle_request(Method, Path, Req) ->
    lager:error("Unexpected HTTP Request: ~s ~s", [Method, Path]),
    Req:not_found().