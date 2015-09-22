-module(mnesia_util).

%% API functions
-export([
    query_session_by_uid/1,
    query_session_by_pid_and_device/2,
    query_session_by_uid_and_device/2,
    all/0
]).

-include("table.hrl").
-include_lib("stdlib/include/qlc.hrl").

query_session_by_pid_and_device(Pid, Device) ->
    Fun = fun() ->
        Query = qlc:q([X || X <- mnesia:table(session), X#session.pid =:= Pid, X#session.device =:= Device]),
        qlc:e(Query)
    end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> false;
        {atomic, [User]} -> User
    end.

query_session_by_uid(Uid) when is_integer(Uid) ->
    Fun = fun() ->
        Query = qlc:q([X || X <- mnesia:table(session), X#session.uid =:= Uid]),
        qlc:e(Query)
    end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> false;
        {atomic, Users} -> Users
    end.

query_session_by_uid_and_device(Uid, Device) when is_integer(Uid) ->
    Fun = fun() ->
        Query = qlc:q([X || X <- mnesia:table(session), X#session.uid =:= Uid, X#session.device =:= Device]),
        qlc:e(Query)
    end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> false;
        {atomic, [User]} -> User
    end.

all() ->
    Fun = fun() ->
        Query = qlc:q([X || X <- mnesia:table(session)]),
        qlc:e(Query)
    end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> false;
        {atomic, Users} -> Users
    end.
