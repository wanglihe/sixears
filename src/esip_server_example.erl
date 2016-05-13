-module(esip_server_example).
-protocol({rfc, 3261}).

-behaviour(esip).

%% API
-export([start/0, stop/0]).

-export([data_in/2, data_out/2, message_in/2,
	 message_out/2, request/2, request/3, response/2,
	 locate/1, dialog_transaction_user/3]).

-include_lib("p1_sip/include/esip.hrl").
-define(DEBUG(Format, Args), io:format(Format, Args)).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    esip:start(),
    esip:set_config_value(max_server_transactions, 10000),
    esip:set_config_value(max_client_transactions, 10000),
    esip:set_config_value(software, <<"sixears/0.0.2">>),
    esip:set_config_value(module, ?MODULE),
    esip:add_listener(6060, udp, []),
    ok.

stop() ->
    esip:stop().

data_in(Data, #sip_socket{type = Transport,
                          addr = {MyIP, MyPort},
                          peer = {PeerIP, PeerPort}}) ->
    ?DEBUG(
       "SIP [~p/in] ~s:~p -> ~s:~p:~n~s~n~n",
       [Transport, inet_parse:ntoa(PeerIP), PeerPort,
	inet_parse:ntoa(MyIP), MyPort, Data]).

data_out(Data, #sip_socket{type = Transport,
                           addr = {MyIP, MyPort},
                           peer = {PeerIP, PeerPort}}) ->
    ?DEBUG(
       "SIP [~p/out] ~s:~p -> ~s:~p:~n~s~n~n",
       [Transport, inet_parse:ntoa(MyIP), MyPort,
	inet_parse:ntoa(PeerIP), PeerPort, Data]).

message_in(_, _) ->
    ok.

message_out(_, _) ->
    ok.

response(_Resp, _SIPSock) ->
    ok.

request(_Req, _SIPSock) ->
    ok.

request(Req, _SIPSock, _TrID) ->
    LocalTag = esip:make_tag(),
    esip:open_dialog(Req, LocalTag, 200, fun dialog_transaction_user/3),
    esip:make_response(Req, #sip{ type = response
                                , status = 200
                                , body = "v=0"}, LocalTag).

locate(_SIPMsg) ->
    ok.

dialog_transaction_user(#sip{type = request, method = <<"ACK">>},_,_) ->
    ok;
dialog_transaction_user(#sip{type = request, method = <<"BYE">>} = Req,_,_) ->
    esip:close_dialog(esip:dialog_id(uas, Req)),
    esip:make_response(Req, #sip{ type = response
                                , status = 200}).

