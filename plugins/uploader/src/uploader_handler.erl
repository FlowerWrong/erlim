-module(uploader_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State) ->
    {_Result, Req2} = acc_multipart(Req, []),
    %% write_to_file(term_to_binary(Result)),
    {ok, Req3} = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain; charset=UTF-8">>}
    ], <<"OK">>, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.

acc_multipart(Req, Acc) ->
    case cowboy_req:part(Req) of
        {ok, Headers, Req2} ->
            [Req4, Body] = case cow_multipart:form_data(Headers) of
                               {data, _FieldName} ->
                                   {ok, MyBody, Req3} = cowboy_req:part_body(Req2),
                                   [Req3, MyBody];
                               {file, _FieldName, Filename, _CType, _CTransferEncoding} ->
                                   Uuid = uuid:to_string(uuid:v4()),
                                   Ext = filename:extension(Filename),
                                   Fname = Uuid ++ Ext,
                                   io:format("File is ~p~n", [Fname]),
                                   {ok, IoDevice} = file:open(Filename, [raw, write, binary]),
                                   Req5 = stream_file(Req2, IoDevice),
                                   file:close(IoDevice),
                                   [Req5, <<"skip printing file content">>]
                           end,
            acc_multipart(Req4, [{Headers, Body} | Acc]);
        {done, Req2} ->
            Result = lists:reverse(Acc),
            {Result, Req2}
    end.

stream_file(Req, IoDevice) ->
    case cowboy_req:part_body(Req) of
        {ok, Body, Req2} ->
            file:write(IoDevice, Body),
            Req2;
        {more, Body, Req2} ->
            file:write(IoDevice, Body),
            stream_file(Req2, IoDevice)
    end.

%% write_to_file(Result) ->
%% 	{ok, IoDevice} = file:open("out.bin", [raw, write, binary]),
%% 		file:write(IoDevice, Result),
%% 		file:close(IoDevice), ok.