-module(esip_client_example).
-protocol({rfc, 3261}).

-behaviour(esip).

%% API
-export([start/0, stop/0]).

-export([data_in/2, data_out/2, message_in/2,
	 message_out/2, request/2, request/3, response/2,
	 locate/1, dialog_transaction_user/4, send_bye/1]).

-include_lib("p1_sip/include/esip.hrl").
-define(DEBUG(Format, Args), io:format(Format, Args)).
-define(TOHOSTPORT, {"127.0.0.1",5060}).
-define(FROMHOSTPORT, {"127.0.0.2",5060}).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    esip:start(),
    esip:set_config_value(max_server_transactions, 10000),
    esip:set_config_value(max_client_transactions, 10000),
    esip:set_config_value(software, <<"sixears/0.0.2">>),
    esip:set_config_value(module, ?MODULE),
    esip:add_listener(5060, udp, []),
    SIPMsg = gen_invite(?TOHOSTPORT, ?FROMHOSTPORT, <<>>),
    {ok, SIPSock} = esip:connect(SIPMsg),
    esip:request(SIPSock, SIPMsg, {?MODULE, dialog_transaction_user, [{SIPMsg, SIPSock}]}),
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

request(_Req, _SIPSock, _TrID) ->
    ok.

locate(_SIPMsg) ->
    ok.

dialog_transaction_user(#sip{type = response, status = S},_,_,_) when S < 200->
    ok;
dialog_transaction_user(#sip{type = response, status = S, method = <<"INVITE">>} = Resp,_,_,{Req, SIPSock}) when S =:= 200->
    Sdp = Resp#sip.body,
    NReq = gen_ack(?TOHOSTPORT, ?FROMHOSTPORT, Resp, Sdp),
    esip:open_dialog(Req, Resp, uac, {?MODULE, dialog_transaction_user,[{NReq, SIPSock}]}),
    %%Ack = esip_dialog:prepare_request(esip:dialog_id(uac, NReq), NReq),
    esip_transport:send(SIPSock, NReq),
    timer:apply_after(3000, ?MODULE, send_bye, [{NReq, SIPSock}]),
    ok;
dialog_transaction_user(#sip{type = response, status = S, method = <<"BYE">>} = Resp,_,_,{_Req, _SIPSock}) when S =:= 200->
    ?DEBUG("dialog_transaction_user bye 200~n~n", []),
    esip:close_dialog(esip:dialog_id(uac, Resp)),
    ok;
dialog_transaction_user(A,B,C,_) ->
    ?DEBUG("dialog_transaction_user: ~p~n~p~n~p~n~n", [A,B,C]).

gen_invite(ToHostPort, FromHostPort, Body) ->
    #sip{ type = request
        , method = <<"INVITE">>
        , uri = gen_uri("sip", "service", ToHostPort)
        , hdrs = esip:make_hdrs()
              ++ [ {via, [gen_via(FromHostPort)]}
                 , {from, {<<"sixears">>, gen_uri("sip", "sixears", FromHostPort),[{<<"tag">>, esip:make_tag()}]}}
                 , {to, {<<"service">>, gen_uri("sip", "service", ToHostPort), []}}
                 , {contact, [{<<>>, gen_uri("sip", "service", FromHostPort), []}]}
                 , {subject, <<"Test">>}
                 , {'content-type', {<<"application/sdp">>, []}}
                 ]
        , body = Body
    }.
gen_via({FromHost, FromPort}) ->
    #via{ transport = <<"UDP">>
        , host = list_to_binary(FromHost)
        , port = FromPort
        , params = [{<<"branch">>, esip:make_branch()}]}.
gen_uri(Scheme, User, {Host, Port}) ->
    #uri{ scheme = list_to_binary(Scheme)
        , user = list_to_binary(User)
        , host = list_to_binary(Host)
        , port = Port}.

gen_ack(ToHostPort, FromHostPort, Sip, Body) ->
    Header = esip:filter_hdrs([ 'from'
                              , 'to'
                              , 'call-id'
                              , 'max-forwards'
                              , 'subject'
                              , 'content-type'
                              , 'content-length'], Sip#sip.hdrs),
    Sip#sip{ type = request
           , method = <<"ACK">>
           , uri = gen_uri("sip", "service", ToHostPort)
           , hdrs = [ {contact, [{<<>>, gen_uri("sip", "service", FromHostPort), []}]}
                    , {via, [gen_via(FromHostPort)]}
                    , {cseq, 999}  |Header]
           , body = Body}.

send_bye({Ack, SIPSock}) ->
    NReq = Ack#sip{type = request, method = <<"BYE">>, body = <<>>},
    %%Bye = esip_dialog:prepare_request(esip:dialog_id(uac, NReq), NReq),
    esip:request(SIPSock, NReq, {?MODULE, dialog_transaction_user, [{NReq, SIPSock}]}).
