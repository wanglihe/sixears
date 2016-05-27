%%%-------------------------------------------------------------------
%%% @author wanglihe <wanglihe.programmer@gmail.com>
%%% @copyright (C) 2015, wanglihe
%%% @doc
%%%
%%% @end
%%% Created : 10 Jul 2015 by wanglihe <wanglihe.programmer@gmail.com>
%%%-------------------------------------------------------------------
-module(session).

-behaviour(esip).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([data_in/2, data_out/2, message_in/2,
	 message_out/2, request/2, request/3, response/2,
	 locate/1, dialog_transaction_user/4]).

-include_lib("p1_sip/include/esip.hrl").

-define(SERVER, ?MODULE).
-define(CONFLOCAl, {"the3pcc.local", 5060}).
-define(CLIENTLOCAL, {"the3pcc.local", 5060}).

-define(DEBUG(Format, Args), io:format(Format, Args)).

-record(state, { server
               , script_pid
               , callid
               , conf_inv
               , client_inv
               , conf_inv200
               , client_inv200
               , conf_ack
               , client_ack
               , conf_sock
               , client_sock
               , step}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Server) ->
    gen_server:start_link(?MODULE, [self(), Server], []).

%%%===================================================================
%%% esip callbacks
%%%===================================================================
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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([ScriptPid, Server]) ->
    gen_server:cast(self(), self_start),
    {ok, #state{ server = Server
               , script_pid = ScriptPid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(conf_to_tag, _From, State) ->
    Reply = get(conf_to_tag),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(self_start, State) ->
    #state{ server = Server
          , callid = CallId} = State,
    {server, {conf, ConfHost, ConfPort}, _} = Server,
    SIPMsg = gen_invite({ConfHost, ConfPort}, ?CONFLOCAl, <<>>),
    {ok, SIPSock} = esip:connect(SIPMsg),
    esip:request(SIPSock, SIPMsg, {?MODULE, dialog_transaction_user, [self()]}),
    {noreply, State#state{ conf_sock = SIPSock
                         , conf_inv = SIPMsg
                         , step = wait_conf_invite}};
handle_cast({inv_200, Resp}, #state{step = wait_conf_invite} = State) ->
    #state{ server = Server
          , callid = CallId} = State,
    {server, _, {client, ClientHost, ClientPort}} = Server,
    Sdp =  Resp#sip.body,
    SIPMsg = gen_invite({ClientHost, ClientPort}, ?CLIENTLOCAL, Sdp),
    {ok, SIPSock} = esip:connect(SIPMsg),
    esip:request(SIPSock, SIPMsg, {?MODULE, dialog_transaction_user, [self()]}),
    {noreply, State#state{ client_sock = SIPSock
                         , client_inv = SIPMsg
                         , conf_inv200 = Resp
                         , step = wait_client_invite}};

handle_cast({inv_200, ClientResp}, #state{step = wait_client_invite} = State) ->
    Sdp =  ClientResp#sip.body,
    #state{ server = Server
          , conf_inv = ConfReq
          , conf_inv200 = ConfResp
          , conf_sock = ConfSock} = State,
    {server, {conf, ConfHost, ConfPort}, {client, ClientHost, ClientPort}} = Server,

    NConfReq = gen_ack({ConfHost, ConfPort}, ?CONFLOCAl, ConfResp, Sdp),
    esip:open_dialog(ConfReq, ConfResp, uac, {?MODULE, dialog_transaction_user,[{NConfReq}]}),
    ConfAck = esip_dialog:prepare_request(esip:dialog_id(uac, NConfReq), NConfReq),
    esip_transport:send(ConfSock, NConfReq),
    io:format("sent conf ack~n"),

    #state{ client_inv = ClientReq
          , client_sock = ClientSock} = State,
    %%NClientReq = ClientResp#sip{type = request, method = <<"ACK">>, body = <<>>},
    NClientReq = gen_ack({ClientHost, ClientPort}, ?CLIENTLOCAL, ClientResp, <<>>),
    esip:open_dialog(ClientReq, ClientResp, uac, {?MODULE, dialog_transaction_user,[{NClientReq}]}),
    ClientAck = esip_dialog:prepare_request(esip:dialog_id(uac, NClientReq), NClientReq),
    esip_transport:send(ClientSock, NClientReq),
    io:format("sent client ack~n"),
    gen_server:cast(State#state.script_pid, init_complete),
    {noreply, State#state{ client_inv200 = ClientResp
                         , conf_ack = NConfReq
                         , client_ack = NClientReq
                         , step = none}};

handle_cast(destroy, State) ->
    #state{ conf_ack = ConfAck
          , client_ack = ClientAck
          , conf_sock = ConfSock
          , client_sock = ClientSock} = State,
    ConfReq = ConfAck#sip{type = request, method = <<"BYE">>, body = <<>>},
    ConfBye = esip_dialog:prepare_request(esip:dialog_id(uac, ConfReq), ConfReq),
    esip:request(ConfSock, ConfReq, {?MODULE, dialog_transaction_user, [self()]}),

    ClientReq = ClientAck#sip{type = request, method = <<"BYE">>, body = <<>>},
    ClientBye = esip_dialog:prepare_request(esip:dialog_id(uac, ClientReq), ClientReq),
    esip:request(ClientSock, ClientReq, {?MODULE, dialog_transaction_user, [self()]}),
    {noreply, State#state{step = wait_bye}};

handle_cast({bye_200, Resp}, State) when State#state.step =:= wait_bye ->
    esip:close_dialog(esip:dialog_id(uac, Resp)),
    {noreply, State#state{step = wait_bye_1}};
handle_cast({bye_200, Resp}, State) when State#state.step =:= wait_bye_1 ->
    esip:close_dialog(esip:dialog_id(uac, Resp)),
    {stop, normal, State};

handle_cast({confserver, Comm}, State) ->
    #state{ server = Server
          , conf_sock = ConfSock
          , conf_inv200 = Conf200} = State,
    io:format("confserver goes ~p~n", [Comm]),
    {server, {conf, ConfHost, ConfPort}, _} = Server,
    Msml = gen_msml(Comm),
    case Msml of
        [] ->
            {noreply, State};
        _ ->
            ConfInfo = gen_info({ConfHost, ConfPort}, ?CONFLOCAl, Conf200, Msml),
            esip:request(ConfSock, ConfInfo, {?MODULE, dialog_transaction_user, [self()]}),
            {noreply, State}
    end;

handle_cast({clientserver, Comm}, State) ->
    #state{ server = Server
          , client_sock = ClientSock
          , client_inv200 = Client200} = State,
    io:format("clientserver goes ~p~n", [Comm]),
    {server, _, {client, ClientHost, ClientPort}} = Server,
    Msml = gen_msml(Comm),
    case Msml of
        [] ->
            {noreply, State};
        _ ->
            ClientInfo = gen_info({ClientHost, ClientPort}, ?CLIENTLOCAL, Client200, Msml),
            esip:request(ClientSock, ClientInfo, {?MODULE, dialog_transaction_user, [self()]}),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({destroy, Pid}, State) ->
    gen_server:cast(Pid, destroy),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
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

gen_uri(Scheme, User, {Host, Port}) ->
    #uri{ scheme = list_to_binary(Scheme)
        , user = list_to_binary(User)
        , host = list_to_binary(Host)
        , port = Port}.

gen_via({FromHost, FromPort}) ->
    #via{ transport = <<"UDP">>
        , host = list_to_binary(FromHost)
        , port = FromPort
        , params = [{<<"branch">>, esip:make_branch()}]}.

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
                    , {cseq, 998} |Header]
           , body = Body}.

gen_bye(ToHostPort, FromHostPort, Sip) ->
    Header = esip:filter_hdrs([ 'from'
                              , 'to'
                              , 'call-id'
                              , 'max-forwards'
                              , 'subject'
                              , 'content-type'
                              , 'content-length'], Sip#sip.hdrs),
    #sip{ type = request
        , method = <<"BYE">>
        , uri = gen_uri("sip", "service", ToHostPort)
        , hdrs = [ {contact, [{<<>>, gen_uri("sip", "service", FromHostPort), []}]}
                 , {via, [gen_via(FromHostPort)]}
                 , {cseq, 3} |Header]}.
gen_info(ToHostPort, FromHostPort, Sip, Body) ->
    Header = esip:filter_hdrs([ 'from'
                              , 'to'
                              , 'call-id'
                              , 'max-forwards'
                              , 'subject'
                              , 'content-length'], Sip#sip.hdrs),
    Sip#sip{ type = request
           , method = <<"INFO">>
           , uri = gen_uri("sip", "service", ToHostPort)
           , hdrs = [ {contact, [{<<>>, gen_uri("sip", "service", FromHostPort), []}]}
                    , {via, [gen_via(FromHostPort)]}
                    , {cseq, 997}
                    , {'content-type', {<<"application/msml+xml">>, []}} |Header]
           , body = Body}.
gen_msml({play, Filename}) ->
    Format =
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r
        <msml version=\"1.1\">\r
        <dialogstart target=\"conn:12345\" name=\"12345\">\r
        <play barge=\"false\" iterate=\"50\" interval=\"0\" maxtime=\"600s\">\r
        <audio uri=\"~s\"/>\r
        <playexit>\r
        <send target=\"source\" event=\"done\" namelist=\"play.end play.amt\"/>\r
        </playexit>\r
        </play>\r
        </dialogstart>\r
        </msml>",
    Msml = lists:flatten(io_lib:format(Format, [Filename])),
    list_to_binary(Msml);
gen_msml({play_digit, Filename}) ->
    Format =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r
            <msml version=\"1.1\">\r
                <dialogstart target=\"conn:12345\" name=\"12345\">\r
                    <dtmf fdt=\"5\" idt=\"3\" maxtime=\"123s\" starttimer=\"true\" >\r
                        <play barge=\"true\" maxtime=\"444\">\r
                            <audio uri=\"~s\"/>\r
                        </play>\r
                        <pattern digits=\"min=2;max=5;cancel=*;rtk=#\" format=\"moml+digits\">\r
                            <send event=\"done\" namelist=\"dtmf.digits dtmf.end\" target=\"source\"/>\r
                        </pattern>\r
                        <noinput> \r
                                <send target=\"source\" event=\"done\" namelist=\"dtmf.end\"/> \r
                        </noinput> \r
                        <nomatch> \r
                                <send target=\"source\" event=\"done\" namelist=\"dtmf.end\"/> \r
                        </nomatch>\r
                        <dtmfexit>\r
                            <send event=\"done\" namelist=\"dtmf.end\" target=\"source\"/>\r
                        </dtmfexit>\r
                    </dtmf>\r
                </dialogstart>\r
            </msml>\r",
    Msml = lists:flatten(io_lib:format(Format, [Filename])),
    list_to_binary(Msml);
gen_msml({conf, create, Name}) ->
    Format =
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r
      <msml version=\"1.1\">\r
          <createconference name=\"~s\" deletewhen=\"never\">\r
               <reserve>\r
                  <resource n=\"20\">\r
                  </resource>\r
               </reserve>\r
               <audiomix id=\"mix1\">\r
                  <n-loudest n=\"3\"/>\r
               </audiomix>\r
          </createconference>\r
      </msml>",
    Msml = lists:flatten(io_lib:format(Format, [Name])),
    list_to_binary(Msml);
gen_msml({conf, destroy, Name}) ->
    Format =
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r
        <msml version=\"1.1\">\r
            <destroyconference name=\"~s\">\r
            </destroyconference>\r
        </msml>",
    Msml = lists:flatten(io_lib:format(Format, [Name])),
    list_to_binary(Msml);
gen_msml({conf, join, ConfName, SPid}) ->
    Format =
      "<?xml version=\"1.0\" encoding=\"utf-8\"?>\r
      <msml version=\"1.1\">\r
        <join id1=\"conn:~s\" id2=\"conf:~s\">\r
              <stream media=\"audio\" dir=\"from-id1\"/>\r
              <stream media=\"audio\" dir=\"to-id1\"/>\r
        </join>\r
      </msml>",
    %%ConfToTag = get(conf_to_tag),
    Self = self(),
    ConfToTag = case SPid of
                    Self ->
                        get(conf_to_tag);
                    _ ->
                        gen_server:call(SPid, conf_to_tag)
                end,
    Msml = lists:flatten(io_lib:format(Format, [ConfToTag, ConfName])),
    list_to_binary(Msml);

gen_msml({conf, unjoin, ConfName, SPid}) ->
    Format =
      "<?xml version=\"1.0\" encoding=\"utf-8\"?>\r
      <msml version=\"1.1\">\r
          <unjoin id1=\"conn:~s\" id2=\"conf:~s\">\r
              <stream dir=\"from-id1\" media=\"audio\"/>\r
              <stream dir=\"to-id1\" media=\"audio\"/>\r
          </unjoin>\r
      </msml>",
    %%ConfToTag = get(conf_to_tag),
    Self = self(),
    ConfToTag = case SPid of
                    Self ->
                        get(conf_to_tag);
                    _ ->
                        gen_server:call(SPid, conf_to_tag)
                end,
    Msml = lists:flatten(io_lib:format(Format, [ConfToTag, ConfName])),
    list_to_binary(Msml);

%%gen_msml({conf, media, ConfName, Cmds}) ->
%%    FormatHead =
%%      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r
%%      <msml version=\"1.1\">\r
%%          <dialogstart target=\"conf:~s\" name=\"audioPlay\" mark=\"7\">\r
%%              <record maxtime=\"10s\" dest=\"file://~s\" format=\"g711u\" termkey=\"9\">\r\n",
%%
%%    FormatPlay =
%%      "           <play>\r
%%                      <audio uri=\"file://~s\"/>\r
%%                  </play>\r\n",
%%    FormatTail =
%%      "           <recordexit>\r
%%                      <send target=\"source\" event=\"done\" valuelist=\"record.len record.end\"/>\r
%%                  </recordexit>\r
%%              </record>\r
%%          </dialogstart>\r
%%      </msml>",
%%    Format = case lists:keyfind(play, 1, Cmds) of
%%        false ->
%%            FormatHead ++ FormatTail;
%%        {play, PlayName} ->
%%            FormatHead ++ io_lib:format(FormatPlay, [PlayName]) ++ FormatTail
%%    end,
%%    {record, RecName} = lists:keyfind(record, 1, Cmds),
%%    Msml = lists:flatten(io_lib:format(Format, [ConfName, RecName])),
%%    list_to_binary(Msml);

gen_msml({conf, media, ConfName, Cmds}) ->
    Format =
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r
      <msml version=\"1.1\">\r
          <dialogstart target=\"conf:~s\" name=\"audioPlay\" mark=\"7\">\r
                 <play>\r
                      <audio uri=\"file://~s\"/>\r
                 </play>\r
                 <send target=\"source\" event=\"done\" valuelist=\"play.end\"/>\r
          </dialogstart>\r
      </msml>",
    {play, PlayName} = lists:keyfind(play, 1, Cmds),
    Msml = lists:flatten(io_lib:format(Format, [ConfName, PlayName])),
    list_to_binary(Msml);

gen_msml({session, media, SessionName, Cmds}) ->
    io:format("session cmds ~p~n", [Cmds]),
    FormatHead =
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r
      <msml version=\"1.1\">\r
          <dialogstart target=\"conn:~s\" name=\"audioPlay\" mark=\"7\">\r
              <record maxtime=\"10s\" dest=\"file://~s\" format=\"g711u\" termkey=\"9\">\r\n",

    FormatPlay =
      "           <play>\r
                      <audio uri=\"file://~s\"/>\r
                  </play>\r\n",
    FormatTail =
      "           <recordexit>\r
                      <send target=\"source\" event=\"done\" valuelist=\"record.len record.end\"/>\r
                  </recordexit>\r
              </record>\r
          </dialogstart>\r
      </msml>",
    Format = case lists:keyfind(play, 1, Cmds) of
        false ->
            FormatHead ++ FormatTail;
        {play, PlayName} ->
            FormatHead ++ io_lib:format(FormatPlay, [PlayName]) ++ FormatTail
    end,
    {record, RecName} = lists:keyfind(record, 1, Cmds),
    Msml = lists:flatten(io_lib:format(Format, [SessionName, RecName])),
    list_to_binary(Msml);
gen_msml(UnSupport) ->
    io:format("msml not gen for ~p~n", [UnSupport]),
    [].

find_totag(#sip{hdrs = Headers}) ->
    {_, _, ToParams} = esip:get_hdr('to', Headers),
    esip:to_lower(esip:get_param(<<"tag">>, ToParams)).

dialog_transaction_user(#sip{type = response, status = S},_,_,_) when S < 200->
    ok;
dialog_transaction_user(#sip{type = response, status = S, method = <<"INVITE">>} = Resp,_,_,Pid) when S =:= 200 ->
    gen_server:cast(Pid, {inv_200, Resp}),
    ok;
dialog_transaction_user(#sip{type = response, status = S, method = <<"BYE">>} = Resp,_,_,Pid) when S =:= 200->
    gen_server:cast(Pid, {bye_200, Resp}),
    ok;
dialog_transaction_user(#sip{type = response, status = S} = Resp,_,_,Pid) when S =:= 200->
    ok;
dialog_transaction_user(A,B,C,_) ->
    ?DEBUG("dialog_transaction_user: ~p~n~p~n~p~n~n", [A,B,C]).
