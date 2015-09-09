-module(erlim_util).

%% API functions
-export([query_pid/1, query_user/1, query_user/2]).


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

query_user(Username) ->
    Fun = fun() ->
        Query = qlc:q([X || X <- mnesia:table(user), X#user.username =:= binary_to_list(Username)]),
        qlc:e(Query)
        end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> false;
        {atomic, [User]} -> User
    end.

query_user(Username, Password) ->
    Fun = fun() ->
        Query = qlc:q([X || X <- mnesia:table(user), X#user.username =:= binary_to_list(Username), X#user.password =:= binary_to_list(Password)]),
        qlc:e(Query)
        end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> false;
        {atomic, [User]} -> User
    end.
