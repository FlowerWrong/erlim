-module(mysql_demo).
-export([run/0]).

run() ->
    crypto:start(),
    application:start(emysql),

    emysql:add_pool(erlim_pool, [
                 {size,1},
                 {user, "root"},
                 {password, "root"},
                 {database, "movie_together_development"},
                 {encoding, utf8}]),

    Result = emysql:execute(erlim_pool,
            <<"select * from movies">>),

    io:format("~n~p~n", [Result]).
