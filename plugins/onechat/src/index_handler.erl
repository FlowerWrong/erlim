-module(index_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([get_json/2]).

init(_, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, get_json}
	], Req, State}.

get_json(Req, State) ->
	Doc = {[{<<"msg">>, <<"welcome to onechat">>}, {<<"code">>, 200}]},
	Body = jiffy:encode(Doc),
	{Body, Req, State}.
