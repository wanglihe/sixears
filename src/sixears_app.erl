-module(sixears_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    esip:set_config_value(max_server_transactions, 10000),
    esip:set_config_value(max_client_transactions, 10000),
    esip:set_config_value(software, <<"sixears/0.0.2">>),
    esip:set_config_value(module, session),
    ScriptName = os:getenv("scriptname"),
    io:format("ScriptName is: ~p~n", [ScriptName]),
    sixears_sup:start_link(ScriptName).

stop(_State) ->
    ok.
