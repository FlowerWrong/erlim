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
-export([key/1, is_websocket/1, websocket_data/1, websocket_unmask/3, send_ws_data/2]).

key(Key) ->
    base64:encode(crypto:hash(sha, << Key/binary, ?WEBSOCKET_GUID >>)).

is_websocket(Data) ->
    case websocket_data(Data) of
        <<>> -> false;
        _ -> true
    end.

%% Thanks http://www.cnblogs.com/suex/p/3669953.html
%% 仅处理长度为125以内的文本消息
websocket_data(Data) when is_list(Data) ->
    websocket_data(list_to_binary(Data));
websocket_data(<< 1:1, 0:3, 1:4, 1:1, Len:7, MaskKey:32, Rest/bits >>) when Len < 126 ->
    <<End:Len/binary, _/bits>> = Rest,
    Text = websocket_unmask(End, MaskKey, <<>>),
    Text;
%% FIXME
websocket_data(<< 1:1, 0:3, Opcode:4, 1:1, 126:7, Len:16, MaskKey:32, Rest/bits >>) when Len > 125, Opcode < 8 ->
    lager:info("126rest is ~p~n", [Rest]),
    <<End:Len/binary, _/bits>> = Rest,
    Text = websocket_unmask(End, MaskKey, <<>>),
    Text;
%% FIXME
websocket_data(<< 1:1, 0:3, Opcode:4, 1:1, 127:7, 0:1, Len:63, MaskKey:32, Rest/bits >>) when Len > 16#ffff, Opcode < 8 ->
    lager:info("127rest is ~p~n", [Rest]),
    <<End:Len/binary, _/bits>> = Rest,
    Text = websocket_unmask(End, MaskKey, <<>>),
    Text;
websocket_data(_) ->
    <<>>.

%% 由于Browser发过来的数据都是mask的,所以需要unmask
websocket_unmask(<<>>, _, Unmasked) ->
    Unmasked;
websocket_unmask(<< O:32, Rest/bits >>, MaskKey, Acc) ->
    T = O bxor MaskKey,
    websocket_unmask(Rest, MaskKey, << Acc/binary, T:32 >>);
websocket_unmask(<< O:24 >>, MaskKey, Acc) ->
    << MaskKey2:24, _:8 >> = << MaskKey:32 >>,
    T = O bxor MaskKey2,
    << Acc/binary, T:24 >>;
websocket_unmask(<< O:16 >>, MaskKey, Acc) ->
    << MaskKey2:16, _:16 >> = << MaskKey:32 >>,
    T = O bxor MaskKey2,
    << Acc/binary, T:16 >>;
websocket_unmask(<< O:8 >>, MaskKey, Acc) ->
    << MaskKey2:8, _:24 >> = << MaskKey:32 >>,
    T = O bxor MaskKey2,
    << Acc/binary, T:8 >>.

%% 发送文本给Client
send_ws_data(Socket, Payload) ->
    Len = iolist_size(Payload),
    BinLen = payload_length_to_binary(Len),
    gen_tcp:send(Socket, [<< 1:1, 0:3, 1:4, 0:1, BinLen/bits >>, Payload]).

payload_length_to_binary(N) ->
    case N of
        N when N =< 125 -> << N:7 >>;
        N when N =< 16#ffff -> << 126:7, N:16 >>;
        N when N =< 16#7fffffffffffffff -> << 127:7, N:64 >>
    end.