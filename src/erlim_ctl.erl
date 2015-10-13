%%%-------------------------------------------------------------------
%%% @author yy
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 十月 2015 下午1:01
%%%-------------------------------------------------------------------
-module(erlim_ctl).
-author("yy").

%% API
-export([status/1, cluster_info/1, join_cluster/1, leave_cluster/1, add_user/1, del_user/1, clients/1, sessions/1, vm/1, mem/1, mnesia/1]).

status([]) ->
    io:format("status need to fix"),
    ok.

cluster_info([]) ->
    io:format("fix me"),
    ok.

join_cluster([]) ->
    io:format("fix me"),
    ok.

leave_cluster([]) ->
    io:format("fix me"),
    ok.

add_user([_Name, _Pass]) ->
    io:format("fix me"),
    ok.

del_user([_Name]) ->
    io:format("fix me"),
    ok.

clients([]) ->
    io:format("fix me"),
    ok.

sessions([]) ->
    io:format("fix me"),
    ok.

vm([_Cmd]) ->
    lists:foreach(fun(Info) ->
        {Key, Val} = Info,
        io:format("VM info ~p are ~p~n", [Key, Val])
                  end, erlim_vm:get_system_info()).

mem([]) ->
    lists:foreach(fun(Info) ->
        {Key, Val} = Info,
        io:format("Memory info ~p is ~p~n", [Key, Val])
                  end, erlim_vm:mem_info()).

mnesia([]) ->
    io:format("fix me"),
    ok.