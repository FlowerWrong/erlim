-module(onechat_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    {ok,[{
        <<"database">>,
        [
            {<<"encoding">>, Encoding},
            {<<"db">>, Dbname},
            {<<"pwd">>, Pwd},
            {<<"name">>, UserName},
            {<<"size">>, Size},
            {<<"host">>, Host}
        ]
    }]} = toml_util:parse(),

    emysql:add_pool(onechat_pool, [
        {size, Size},
        {user, binary_to_list(UserName)},
        {password, binary_to_list(Pwd)},
        {host, binary_to_list(Host)},
        {database, binary_to_list(Dbname)},
        {encoding, binary_to_atom(Encoding, utf8)}
    ]),

	Dispatch = cowboy_router:compile([
		{'_', [
            {"/", index_handler, []},
            %% @doc 联系人列表
            {"/api/v1/contacts", friendship_handler, []},
            %% @doc 黑名单列表
            {"/api/v1/blocks", blockship_handler, []}
            %% @TODO 获取群基本信息
            %% @TODO 修改群昵称
            %% @TODO 修改联系人昵称
            %% @TODO 修改消息免打扰
            %% @TODO 修改群聊天背景
            %% @TODO 举报群
            %% @TODO 查找群聊天记录


            %% @TODO 获取联系人基本信息
            %% @TODO 添加联系人黑名单, 无需对方同意, 无通知给对方
            %% @TODO 移除联系人黑名单, 无需对方同意, 无通知给对方
        ]}
	]),
	cowboy:start_http(http, 100, [{port, 8084}],
		[{env, [{dispatch, Dispatch}]}]
	),
	onechat_sup:start_link().

stop(_State) ->
	ok.
