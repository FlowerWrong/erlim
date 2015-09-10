-module(mysql_util).

-export([query_user_by_mobile/1]).

-include("table.hrl").
-include_lib("stdlib/include/qlc.hrl").

query_user_by_mobile(Username) ->
    emysql:prepare(query_user_stmt, <<"SELECT * FROM users WHERE mobile = ?">>),
    Result = emysql:execute(erlim_pool, query_user_stmt, [Username]),
    Recs = emysql_util:as_record(Result, user_record, record_info(fields, user_record)),
    [User | _T] = Recs,
    User.
