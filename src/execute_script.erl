%%%-------------------------------------------------------------------
%%% @author wanglihe <wanglihe.programmer@gmail.com>
%%% @copyright (C) 2015, wanglihe
%%% @doc
%%%
%%% @end
%%% Created : 15 Jul 2015 by wanglihe <wanglihe.programmer@gmail.com>
%%%-------------------------------------------------------------------
-module(execute_script).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include_lib("p1_sip/include/esip.hrl").

-record(state, { script
               , server
               , callid
               , script_step
               , sip_step}).

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
start_link(Script, Server, CallId) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Script, Server, CallId], []).

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
init([Script, Server, CallId]) ->
    gen_server:cast(self(), self_start),
    {ok, #state{ script = Script
               , server = Server
               , callid = CallId
               , script_step = start
               , sip_step = none}}.

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
    #state{ server = Server} = State,
    {server, {conf, ConfHost, ConfPort}, {client, ClientHost, ClientPort}} = Server,
    io:format("gen conf invite: ~p~n", [list_to_binary(gen_invite({ConfHost, ConfPort}, {"localhost.local", 5060}, <<>>))]),
    io:format("gen client invite: ~p~n", [list_to_binary(gen_invite({ClientHost, ClientPort}, {"localhost.local", 5060}, <<>>))]),
    io:format("script run over~n"),
    {stop, normal, State};
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

gen_invite(ToHostPort, FromHostPort, Body) ->
    Invite = #sip{ type = request
                 , method = <<"INVITE">>
                 , uri = gen_uri("sip", "service", ToHostPort)
                 , hdrs = [ {via, [gen_via(FromHostPort)]}
                          , {from, {<<"sixears">>, gen_uri("sip", "sixears", FromHostPort),[{<<"tag">>, esip:make_tag()}]}}
                          , {to, {<<"service">>, gen_uri("sip", "service", ToHostPort), []}}
                          , {'call-id', esip:make_callid()}
                          , {cseq, 1}
                          , {contact, [{<<>>, gen_uri("sip", "service", ToHostPort), []}]}
                          , {'max-forwards', 70}
                          , {subject, <<"Conference Test">>}
                          , {'content-type', {<<"application/sdp">>, []}}
                          , {'content-length', 0}]
                 , body = Body
              },
    esip:encode(Invite).

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
