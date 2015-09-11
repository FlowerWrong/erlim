-module(mysql_util_tests).
-include_lib("eunit/include/eunit.hrl").
-include("table.hrl").

save_msg_test() ->
    ok = application:start(sasl),
    ok = application:start(erlim),
    OffMsg = #msg_record{f = 1, t = 2, msg = "Test msg send from 1 to 2.", unread = 1},
    {ok_packet, _, _, _, _, _, _} = mysql_util:save_msg(OffMsg).
