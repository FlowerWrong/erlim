-module(mnesia_util).

%% API functions
-export([query_pid/1, query_name/1, query_token/1, all/0]).


-include("table.hrl").
-include_lib("stdlib/include/qlc.hrl").

query_pid(Pid) ->
    Fun = fun() ->
        Query = qlc:q([X || X <- mnesia:table(user), X#user.pid =:= Pid]),
        qlc:e(Query)
        end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> false;
        {atomic, [User]} -> User
    end.

query_name(Name) ->
    Fun = fun() ->
        Query = qlc:q([X || X <- mnesia:table(user), X#user.name =:= Name]),
        qlc:e(Query)
        end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> false;
        {atomic, [User]} -> User
    end.

query_token(Token) ->
    Fun = fun() ->
        Query = qlc:q([X || X <- mnesia:table(user), X#user.token =:= Token]),
        qlc:e(Query)
        end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> false;
        {atomic, [User]} -> User
    end.

all() ->
    Fun = fun() ->
        Query = qlc:q([X || X <- mnesia:table(user)]),
        qlc:e(Query)
        end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> false;
        {atomic, Users} -> Users
    end.
