%%%-------------------------------------------------------------------
%%% @author wanglihe <wanglihe.programmer@gmail.com>
%%% @copyright (C) 2015, wanglihe
%%% @doc
%%%
%%% @end
%%% Created : 10 Jul 2015 by wanglihe <wanglihe.programmer@gmail.com>
%%%-------------------------------------------------------------------
-module(session).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("p1_sip/include/esip.hrl").

-define(SERVER, ?MODULE).
-define(CONFLOCAl, {"the3pcc.local", 5060}).
-define(CLIENTLOCAL, {"the3pcc.local", 5061}).

-record(state, { server
               , script_pid
               , callid
               , conf_inv200
               , client_inv200
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
    CallId = gen_server:call(core_dispatch, callid),
    io:format("session ~p get callid ~p~n", [self(), CallId]),
    gen_server:cast(self(), self_start),
    {ok, #state{ server = Server
               , callid = CallId
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
    ConfInvite = gen_invite(CallId, {ConfHost, ConfPort}, ?CONFLOCAl, <<>>),
    gen_server:cast(core_dispatch, {confserver, esip:encode(ConfInvite)}),
    {noreply, State#state{step = wait_conf_invite}};
handle_cast({confserver, #sip{} = Sip}, #state{step = wait_conf_invite} = State) ->
    #sip{body = Sdp} = Sip,
    #state{ server = Server
          , callid = CallId} = State,
    {server, _, {client, ClientHost, ClientPort}} = Server,
    ConfToTag = find_totag(Sip),
    put(conf_to_tag, ConfToTag),
    ClientInvite = gen_invite(CallId, {ClientHost, ClientPort}, ?CLIENTLOCAL, Sdp),
    gen_server:cast(core_dispatch, {clientserver, esip:encode(ClientInvite)}),
    {noreply, State#state{ step = wait_client_invite
                         , conf_inv200 = Sip}};
handle_cast({clientserver, #sip{} = Sip}, #state{step = wait_client_invite} = State) ->
    #state{ server = Server
          , conf_inv200 = Conf200} = State,
    {server, {conf, ConfHost, ConfPort}, {client, ClientHost, ClientPort}} = Server,
    #sip{body = Sdp} = Sip,
    ConfAck = gen_ack({ConfHost, ConfPort}, ?CONFLOCAl, Conf200, Sdp),
    gen_server:cast(core_dispatch, {confserver, esip:encode(ConfAck)}),
    ClientAck = gen_ack({ClientHost, ClientPort}, ?CLIENTLOCAL, Sip, <<>>),
    gen_server:cast(core_dispatch, {clientserver, esip:encode(ClientAck)}),

    io:format("~p init completed, send to ~p ~n", [self(), State#state.script_pid]),
    gen_server:cast(State#state.script_pid, init_complete),
    {noreply, State#state{ client_inv200 = Sip
                         , step = none}};

handle_cast(destroy, State) ->
    #state{ server = Server
          , conf_inv200 = Conf200
          , client_inv200 = Client200} = State,
    {server, {conf, ConfHost, ConfPort}, {client, ClientHost, ClientPort}} = Server,
    ConfBye = gen_bye({ConfHost, ConfPort}, ?CONFLOCAl, Conf200),
    gen_server:cast(core_dispatch, {confserver, esip:encode(ConfBye)}),
    ClientBye = gen_bye({ClientHost, ClientPort}, ?CLIENTLOCAL, Client200),
    gen_server:cast(core_dispatch, {clientserver, esip:encode(ClientBye)}),
    {noreply, State#state{step = wait_bye}};

handle_cast({_S, #sip{} = _Sip}, State) when State#state.step =:= wait_bye ->
    {noreply, State#state{step = wait_bye_1}};
handle_cast({_S, #sip{} = _Sip}, State) when State#state.step =:= wait_bye_1 ->
    {stop, normal, State};

handle_cast({_S, #sip{status = 200} = _Sip}, State) ->
    {noreply, State};
handle_cast({S, #sip{} = Sip}, State) ->
    io:format("unexpect ~p sip ~p~n", [S, Sip]),
    {noreply, State};

handle_cast({confserver, Comm}, State) ->
    #state{ server = Server
          , conf_inv200 = Conf200} = State,
    io:format("confserver goes ~p~n", [Comm]),
    {server, {conf, ConfHost, ConfPort}, _} = Server,
    Msml = gen_msml(Comm),
    ConfInfo = gen_info({ConfHost, ConfPort}, ?CONFLOCAl, Conf200, Msml),
    gen_server:cast(core_dispatch, {confserver, esip:encode(ConfInfo)}),
    {noreply, State};

handle_cast({clientserver, Comm}, State) ->
    #state{ server = Server
          , client_inv200 = Client200} = State,
    io:format("clientserver goes ~p~n", [Comm]),
    {server, _, {client, ClientHost, ClientPort}} = Server,
    Msml = gen_msml(Comm),
    ClientInfo = gen_info({ClientHost, ClientPort}, ?CLIENTLOCAL, Client200, Msml),
    gen_server:cast(core_dispatch, {clientserver, esip:encode(ClientInfo)}),
    {noreply, State};

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
gen_invite(CallId, ToHostPort, FromHostPort, Body) ->
    #sip{ type = request
        , method = <<"INVITE">>
        , uri = gen_uri("sip", "service", ToHostPort)
        , hdrs = [ {via, [gen_via(FromHostPort)]}
                 , {from, {<<"sixears">>, gen_uri("sip", "sixears", FromHostPort),[{<<"tag">>, esip:make_tag()}]}}
                 , {to, {<<"service">>, gen_uri("sip", "service", ToHostPort), []}}
                 , {'call-id', CallId}
                 , {cseq, 1}
                 , {contact, [{<<>>, gen_uri("sip", "service", FromHostPort), []}]}
                 , {'max-forwards', 70}
                 , {subject, <<"Conference Test">>}
                 , {'content-type', {<<"application/sdp">>, []}}
                 , {'content-length', 0}]
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
                    , {cseq, 2} |Header]
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
                    , {cseq, 2}
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
gen_msml({conf, create, Name}) ->
    Format =
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r
      <msml version=\"1.1\">\r
          <createconference name=\"~s\" deletewhen=\"never\">    \r
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
gen_msml({conf, join, ConfName, _SessionName}) ->
    Format =
      "<?xml version=\"1.0\" encoding=\"utf-8\"?>\r
      <msml version=\"1.1\">\r
        <join id1=\"conn:~s\" id2=\"conf:~s\">\r
              <stream media=\"audio\" dir=\"from-id1\"/>\r
              <stream media=\"audio\" dir=\"to-id1\"/>\r
        </join>\r
      </msml>",
    %%it will use SessionName to find conf_to_tag
    ConfToTag = get(conf_to_tag),
    Msml = lists:flatten(io_lib:format(Format, [ConfToTag, ConfName])),
    list_to_binary(Msml);

gen_msml({conf, unjoin, ConfName, _SessionName}) ->
    Format =
      "<?xml version=\"1.0\" encoding=\"utf-8\"?>\r
      <msml version=\"1.1\">\r
          <unjoin id1=\"conn:~s\" id2=\"conf:~s\">\r
              <stream dir=\"from-id1\" media=\"audio\"/>\r
              <stream dir=\"to-id1\" media=\"audio\"/>\r
          </unjoin>\r
      </msml>",
    ConfToTag = get(conf_to_tag),
    Msml = lists:flatten(io_lib:format(Format, [ConfToTag, ConfName])),
    list_to_binary(Msml).

find_totag(#sip{hdrs = Headers}) ->
    {_, _, ToParams} = esip:get_hdr('to', Headers),
    esip:to_lower(esip:get_param(<<"tag">>, ToParams)).
