-module(friendship_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([get_json/2]).

-include("onechat.hrl").

init(_, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, get_json}
    ], Req, State}.

%% @doc contacts
get_json(Req, State) ->
    Friendships = mysql_user:friendships(1),
    FriendshipsForJson = lists:map(fun(M) ->
        {[{id, M#friendship_record.id}, {user_id, M#friendship_record.user_id}, {friend_id, M#friendship_record.friend_id}]}
                                   end, Friendships),
    Body = jiffy:encode({[{<<"status">>, 200}, {<<"msg">>, <<"contacts list">>}, {<<"data">>, FriendshipsForJson}]}),
    {Body, Req, State}.
