-module(erlim_client).

-export([chat/2]).

chat(Host, Port) ->
  {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
  ok = gen_tcp:send(Sock, "Hello how are u?").

  %% ok = gen_tcp:close(Sock).
