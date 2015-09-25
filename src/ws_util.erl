%%%-------------------------------------------------------------------
%%% @author yy
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 九月 2015 下午1:53
%%%-------------------------------------------------------------------
-module(ws_util).
-author("yy").

-define(WEBSOCKET_GUID, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").

%% API
-export([key/1, is_websocket/1, get_payload_len/1, get_packet_data/1]).

key(Key) ->
    base64:encode(crypto:hash(sha, << Key/binary, ?WEBSOCKET_GUID >>)).

is_websocket(Data) ->
    case catch <<1:1, 0:3, _Opcode:4, 1:1, _Len:7, _Rest/binary>> = Data of
        Data -> true;
        _Error -> false
    end.

get_payload_len(Packet) ->
    <<_FIN: 1, _RSV1: 1, _RSV2: 1, _RSV3: 1, _OPCODE: 4, _MASK: 1, PAYLOADLEN: 7, Rest/binary>> = Packet,
    if
        PAYLOADLEN =< 125 ->
            <<_MASK_KEY: 32, PAYLOAD/binary>> = Rest,
            byte_size(PAYLOAD);
        PAYLOADLEN == 126 ->
            <<LENGTH: 16, _MASK_KEY: 32, _PAYLOAD/binary>> = Rest,
            LENGTH;
        PAYLOADLEN == 127 ->
            <<LENGTH: 64, _MASK_KEY: 32, _PAYLOAD/binary>> = Rest,
            LENGTH
    end.

get_packet_data(Packet) ->
    <<_FIN: 1, _RSV1: 1, _RSV2: 1, _RSV3: 1, _OPCODE: 4, _MASK: 1, PAYLOADLEN: 7, Rest/binary>> = Packet,
    if
        PAYLOADLEN =< 125 ->
            <<MASK_KEY1: 8, MASK_KEY2: 8, MASK_KEY3: 8, MASK_KEY4: 8, PAYLOAD/binary>> = Rest,
            MASK_KEY = [MASK_KEY1, MASK_KEY2, MASK_KEY3, MASK_KEY4],
            get_packet_data(binary_to_list(PAYLOAD), MASK_KEY, 0, []);
        PAYLOADLEN == 126 ->
            <<_LENGTH: 16, MASK_KEY1: 8, MASK_KEY2: 8, MASK_KEY3: 8, MASK_KEY4: 8, PAYLOAD/binary>> = Rest,
            MASK_KEY = [MASK_KEY1, MASK_KEY2, MASK_KEY3, MASK_KEY4],
            get_packet_data(binary_to_list(PAYLOAD), MASK_KEY, 0, []);
        PAYLOADLEN == 127 ->
            <<_LENGTH: 64, MASK_KEY1: 8, MASK_KEY2: 8, MASK_KEY3: 8, MASK_KEY4: 8, PAYLOAD/binary>> = Rest,
            MASK_KEY = [MASK_KEY1, MASK_KEY2, MASK_KEY3, MASK_KEY4],
            get_packet_data(binary_to_list(PAYLOAD), MASK_KEY, 0, [])
    end.

get_packet_data([H | T], Key, Counter, Result) ->
    get_packet_data(T, Key, Counter + 1, [H bxor lists:nth((Counter rem 4) + 1, Key) | Result]);
get_packet_data([], _, _, Result) ->
    lists:reverse(Result).