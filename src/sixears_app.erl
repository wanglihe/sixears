-module(sixears_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ScriptName = os:getenv("scriptname"),
    io:format("ScriptName is: ~p~n", [ScriptName]),
    sixears_sup:start_link(ScriptName).

stop(_State) ->
    ok.
