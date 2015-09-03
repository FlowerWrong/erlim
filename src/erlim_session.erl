%%%-------------------------------------------------------------------
%%% @author yy
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 八月 2015 下午5:34
%%%-------------------------------------------------------------------
-module(erlim_session).
-author("yy").

%% API
-export([login/2, logout/2, get/1]).

login(_Username, _Pid) ->
    ok.

logout(_Username, _Pid) ->
    ok.

get(_Username) ->
    ok.
