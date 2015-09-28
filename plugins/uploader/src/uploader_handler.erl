-module(uploader_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
    new_file_name,
    file_type
}).

init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State) ->
    {_Result, Req2, NewState} = acc_multipart(Req, [], State),
    Url = iolist_to_binary([<<"/priv/">>, NewState#state.new_file_name]),
    {ok, Req3} = cowboy_req:reply(200, [
        {<<"content-type">>, <<"application/json">>}
    ], iolist_to_binary([<<"{\"url\": \"">>, Url, <<"\"}">>]), Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.

acc_multipart(Req, Acc, State) ->
    case cowboy_req:part(Req) of
        {ok, Headers, Req2} ->
            [Req4, Body, StateTmp] = case cow_multipart:form_data(Headers) of
                               {data, _FieldName} ->
                                   {ok, MyBody, Req3} = cowboy_req:part_body(Req2),
                                   [Req3, MyBody];
                               {file, _FieldName, Filename, _CType, _CTransferEncoding} ->
                                   Name = uuid_filename(Filename),
                                   Fname = iolist_to_binary([list_to_binary(priv_dir()), Name]),
                                   NewState = State#state{new_file_name = Name, file_type = file_ext(Filename)},
                                   {ok, IoDevice} = file:open(Fname, [raw, write, binary]),
                                   Req5 = stream_file(Req2, IoDevice),
                                   file:close(IoDevice),
                                   [Req5, <<"skip printing file content">>, NewState]
                           end,
            acc_multipart(Req4, [{Headers, Body} | Acc], StateTmp);
        {done, Req2} ->
            Result = lists:reverse(Acc),
            {Result, Req2, State}
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

file_ext(Filename) ->
    filename:extension(Filename).

uuid_filename(Filename) ->
    Uuid = uuid:to_string(uuid:v4()),
    Ext = file_ext(Filename),
    iolist_to_binary([list_to_binary(Uuid), Ext]).

priv_dir() ->
    case code:priv_dir(uploader) of
        {error, bad_name} ->
            "priv";
        PrivDir ->
            PrivDir ++ "/files/"
    end.