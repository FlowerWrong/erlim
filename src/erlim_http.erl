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

%% API
-export([dispatch/1]).


dispatch(Req) ->
    handle_request(Req:get(method), Req:get(path), Req).

%% @doc online users count
handle_request('GET', "/users/online", Req) ->
    OnlineUserCount = mnesia_util:online_members_count(),
    Res = {[{<<"status">>, <<"ok">>}, {<<"online_users_count">>, OnlineUserCount}]},
    Json = jiffy:encode(Res),
    Req:ok({"application/json", Json});

%% @doc 404 handler
handle_request(Method, Path, Req) ->
    lager:error("Unexpected HTTP Request: ~s ~s", [Method, Path]),
    Req:not_found().