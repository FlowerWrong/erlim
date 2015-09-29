-module(blockship_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([get_json/2]).

-include("onechat.hrl").

init(_, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, get_json}
	], Req, State}.

%% @doc block list
get_json(Req, State) ->
    Blockships = mysql_user:blockships(1),
    BlockshipsForJson = lists:map(fun(M) ->
        {[{id, M#blockship_record.id}, {user_id, M#blockship_record.user_id}, {block_id, M#blockship_record.block_id}]}
                                   end, Blockships),
    Body = jiffy:encode({[{<<"status">>, 200}, {<<"msg">>, <<"blocks list">>}, {<<"data">>, BlockshipsForJson}]}),
    {Body, Req, State}.
