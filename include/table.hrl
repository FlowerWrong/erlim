%%%-------------------------------------------------------------------
%%% @author yy
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 九月 2015 下午4:11
%%%-------------------------------------------------------------------
-author("yy").

-record(user, {
    username,               % user name
    password,               % password
    pid                     % client pid
}).