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
    case mnesia:create_schema([node()]) of
        {error, Reason} ->
            io:format("Error reason is ~p~n", [Reason]),
            mnesia:start(),
            mnesia:clear_table(user),
            io:format("node is ~p", [node()]),
            create_table();
        ok ->
            mnesia:start(),
            io:format("node is ~p", [node()]),
            create_table()
    end.

create_table() ->
    mnesia:create_table(user, [{
        attributes,
        record_info(fields, user)},
        {ram_copies, [node()]},
        {disc_only_copies, nodes()},
        {storage_properties,
            [{
                ets,
                [compressed]
            },
                {dets, [{auto_save, 5000}]}
            ]
        }
    ]).