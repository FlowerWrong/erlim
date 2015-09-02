-module(erlim_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(StartType, StartArgs) ->
    io:format("StartType is ~p, StartArgs is ~p", [StartType, StartArgs]),
    erlim_sup:start_link().

stop(_State) ->
    ok.
