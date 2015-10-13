%%%-------------------------------------------------------------------
%%% @author yy
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%   erlim admin ctl
%%% @end
%%% Created : 13. 十月 2015 下午1:01
%%%-------------------------------------------------------------------
-module(erlim_ctl).
-author("yy").

%% API
-export([status/1, cluster/1, join_cluster/1, leave_cluster/1, sessions/1, vm/1, mnesia/1]).

%% @doc erlim status
status([]) ->
    case nodes() of
        [] -> io:format("Running on single server mode~n");
        _ -> cluster([])
    end.

%% @doc erlim cluster info
cluster([]) ->
    io:format("Running nodes are ~p~n", [nodes()]).

%% @doc join current node to cluster
join_cluster([Remote]) ->
    RemoteNode = list_to_atom(Remote),
    ok = join_cluster:start(RemoteNode).

%% @doc leave current node from a cluster
leave_cluster([]) ->
    ok = leave_cluster:start().

%% @doc online members count
sessions([]) ->
    io:format("Online clients count is ~p~n", [mnesia_util:online_members_count()]).

%% @doc erlim vm info, include memory, and process...
vm([]) ->
    lists:foreach(fun(Info) ->
        {Key, Val} = Info,
        io:format("VM info ~p is ~p~n", [Key, Val])
                  end, erlim_vm:get_system_info());
vm([CmdList]) ->
    Cmd = list_to_atom(CmdList),
    case Cmd of
        all ->
            lists:foreach(fun(Info) ->
                {Key, Val} = Info,
                io:format("VM info ~p is ~p~n", [Key, Val])
                          end, erlim_vm:get_system_info());
        memory ->
            lists:foreach(fun(Info) ->
                {Key, Val} = Info,
                io:format("Memory info ~p is ~p~n", [Key, Val])
                          end, erlim_vm:mem_info());
        process ->
            lists:foreach(fun(Info) ->
                lists:foreach(fun(M) ->
                    {Key, Val} = M,
                    io:format("Process info ~p is ~p~n", [Key, Val])
                              end, Info)
                          end, erlim_vm:get_process_info());
        _ ->
            unknown_cmd
    end.

%% @doc erlim mneisa info
mnesia([]) ->
    mnesia:info().