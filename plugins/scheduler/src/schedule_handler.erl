-module(schedule_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([get_json/2]).

-include("scheduler.hrl").

init(_, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, get_json}
    ], Req, State}.

%% @doc two argvs: port and type, type is websocket, onechat or rest_api
get_json(Req, State) ->
    {Port, _} = cowboy_req:qs_val(<<"port">>, Req, <<"8080">>),
    {Type, _} = cowboy_req:qs_val(<<"type">>, Req, <<"onechat">>),
    Doc = case Port of
        <<"8080">> ->
            case Type of
                <<"onechat">> ->
                    IpNumbers = scheduler_mnesia:ip_numbers(),
                    IpRecords = scheduler_mnesia:ip_records(),
                    case IpNumbers of
                        0 ->
                            {[{<<"error">>, <<"No ips to select">>}, {<<"code">>, 10500}]};
                        _ ->
                            Selected = case IpNumbers of
                                1 ->
                                    1;
                                _ ->
                                    Total = scheduler_mnesia:total_requests(),
                                    (Total + 1) rem IpNumbers + 1
                            end,
                            IpRecord = lists:nth(Selected, IpRecords),
                            IpTmp = IpRecord#req.ip,
                            PortTmp = IpRecord#req.port,
                            scheduler_mnesia:add_total_request(),
                            scheduler_mnesia:add_ip_request(IpTmp),
                            {[{<<"ip">>, IpTmp}, {<<"port">>, PortTmp}]}
                    end
            end
    end,
    Body = jiffy:encode(Doc),
    {Body, Req, State}.
