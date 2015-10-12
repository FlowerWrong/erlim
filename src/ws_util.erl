%%%-------------------------------------------------------------------
%%% @author yy
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%   websocket utils
%%% @end
%%% Created : 23. 九月 2015 下午1:53
%%%-------------------------------------------------------------------
-module(ws_util).
-author("yy").

-define(WEBSOCKET_GUID, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").

%% API
-export([key/1, is_websocket/1, get_payload_len/1, get_packet_data/1, send_ws_data/3]).

%% @doc get base64 key
key(Key) ->
    base64:encode(crypto:hash(sha, <<Key/binary, ?WEBSOCKET_GUID>>)).

%% @doc check the data is websocket?
is_websocket(Data) ->
    case catch <<1:1, 0:3, _Opcode:4, 1:1, _Len:7, _Rest/binary>> = Data of
        Data -> true;
        _Error -> false
    end.

%% @doc get the payload length
get_payload_len(Packet) ->
    <<_FIN:1, _RSV1:1, _RSV2:1, _RSV3:1, _OPCODE:4, _MASK:1, PAYLOADLEN:7, Rest/binary>> = Packet,
    if
        PAYLOADLEN =< 125 ->
            <<_MASK_KEY:32, PAYLOAD/binary>> = Rest,
            byte_size(PAYLOAD);
        PAYLOADLEN == 126 ->
            <<LENGTH:16, _MASK_KEY:32, _PAYLOAD/binary>> = Rest,
            LENGTH;
        PAYLOADLEN == 127 ->
            <<LENGTH:64, _MASK_KEY:32, _PAYLOAD/binary>> = Rest,
            LENGTH
    end.

%% @doc get payload
get_packet_data(Packet) ->
    <<_FIN:1, _RSV1:1, _RSV2:1, _RSV3:1, _OPCODE:4, _MASK:1, PAYLOADLEN:7, Rest/binary>> = Packet,
    if
        PAYLOADLEN =< 125 ->
            <<MASK_KEY1:8, MASK_KEY2:8, MASK_KEY3:8, MASK_KEY4:8, PAYLOAD/binary>> = Rest,
            MASK_KEY = [MASK_KEY1, MASK_KEY2, MASK_KEY3, MASK_KEY4],
            get_packet_data(binary_to_list(PAYLOAD), MASK_KEY, 0, []);
        PAYLOADLEN == 126 ->
            <<_LENGTH:16, MASK_KEY1:8, MASK_KEY2:8, MASK_KEY3:8, MASK_KEY4:8, PAYLOAD/binary>> = Rest,
            MASK_KEY = [MASK_KEY1, MASK_KEY2, MASK_KEY3, MASK_KEY4],
            get_packet_data(binary_to_list(PAYLOAD), MASK_KEY, 0, []);
        PAYLOADLEN == 127 ->
            <<_LENGTH:64, MASK_KEY1:8, MASK_KEY2:8, MASK_KEY3:8, MASK_KEY4:8, PAYLOAD/binary>> = Rest,
            MASK_KEY = [MASK_KEY1, MASK_KEY2, MASK_KEY3, MASK_KEY4],
            get_packet_data(binary_to_list(PAYLOAD), MASK_KEY, 0, [])
    end.

get_packet_data([H | T], Key, Counter, Result) ->
    get_packet_data(T, Key, Counter + 1, [H bxor lists:nth((Counter rem 4) + 1, Key) | Result]);
get_packet_data([], _, _, Result) ->
    lists:reverse(Result).

%% @doc send websocket payload to client
send_ws_data(Transport, Socket, Payload) ->
    Len = iolist_size(Payload),
    BinLen = payload_length_to_binary(Len),
    Transport:send(Socket, [<<1:1, 0:3, 1:4, 0:1, BinLen/bits>>, Payload]).

%% @doc payload length to binary
payload_length_to_binary(N) ->
    case N of
        N when N =< 125 -> <<N:7>>;
        N when N =< 16#ffff -> <<126:7, N:16>>;
        N when N =< 16#7fffffffffffffff -> <<127:7, N:64>>
    end.