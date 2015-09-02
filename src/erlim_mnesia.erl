%%%-------------------------------------------------------------------
%%% @author yy
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 九月 2015 下午3:38
%%%-------------------------------------------------------------------
-module(erlim_mnesia).
-author("yy").

-include("table.hrl").

%% API
-export([init_mnesia/0]).

init_mnesia() ->
    %% mnesia
    case mnesia:create_schema([node()]) of
        {error, Reason} ->
            io:format("Error reason is ~p~n", [Reason]),
            mnesia:start(),
            mnesia:clear_table(user),
            io:format("node is ~p", [node()]),
            mnesia:create_table(user, [{attributes, record_info(fields, user)}, {disc_only_copies, [node()]}]),
            Yang = #user{username = "yang", password = "123456", pid = 0},
            F1 = fun() ->
                mnesia:write(Yang)
                 end,
            mnesia:transaction(F1),

            Kang = #user{username = "kang", password = "123456", pid = 0},
            F2 = fun() ->
                mnesia:write(Kang)
                 end,
            mnesia:transaction(F2);
        ok ->
            mnesia:start(),
            %% mnesia:clear_table(user),
            io:format("node is ~p", [node()]),
            mnesia:create_table(user, [{attributes, record_info(fields, user)}, {disc_only_copies, [node()]}]),
            Yang = #user{username = "yang", password = "123456", pid = 0},
            F1 = fun() ->
            mnesia:write(Yang)
            end,
            mnesia:transaction(F1),

            Kang = #user{username = "kang", password = "123456", pid = 0},
            F2 = fun() ->
            mnesia:write(Kang)
            end,
            mnesia:transaction(F2)
    end.


