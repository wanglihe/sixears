%%%-------------------------------------------------------------------
%%% @author wanglihe <wanglihe.programmer@gmail.com>
%%% @copyright (C) 2015, wanglihe
%%% @doc
%%%
%%% @end
%%% Created : 10 Jul 2015 by wanglihe <wanglihe.programmer@gmail.com>
%%%-------------------------------------------------------------------
-module(core_dispatch).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("p1_sip/include/esip.hrl").

-define(SERVER, ?MODULE).
-define(CONFPORT, 5060).
-define(CLIENTPORT, 5061).

-record(state, { conf_port
               , client_port
               , server
               , script
               , callid}).

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
start_link(ScriptName) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ScriptName], []).

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
init([ScriptName]) ->
    process_flag(trap_exit, true),
    io:format("core dispatch started with: ~p~n", [ScriptName]),
    case file:consult(ScriptName) of
        {ok, Script} ->
            [Server|RealScript] = Script,
            {ConfPort, ClientPort} = init_server(Server),
            gen_server:cast(self(), {start}), %%启动相关参数加在这里，
                                                 %%如每秒新发，最大并发
            {A,B,C} = os:timestamp(),
            random:seed(A,B,C),
            CallId = random:uniform(1000000),
            {ok, #state{ conf_port = ConfPort
                       , client_port = ClientPort
                       , callid = CallId
                       , server = Server
                       , script = RealScript }};
        {error, Reason} ->
            io:format("load script get error ~p~n", [Reason]),
            {stop, Reason}
    end.

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
handle_cast({start}, State) ->
    #state{ script = Script
          , server = Server
          , callid = CallIdNum} = State,
    CallId = integer_to_binary(CallIdNum),
    case execute_script:start_link(Script, Server, CallId) of
        {ok, Pid} ->
            put(Pid, CallId),
            put(CallId, Pid);
        {error, _Reason} ->
            ok
    end,
    {noreply, State#state{ callid = CallIdNum + 1}};
handle_cast({confserver, Msg}, State) ->
    #state{conf_port = Socket} = State,
    io:format("conf ---------------------->~p~n", [Msg]),
    gen_udp:send(Socket, Msg),
    {noreply, State};

handle_cast({clientserver, Msg}, State) ->
    #state{client_port = Socket} = State,
    io:format("client =====================>~p~n", [Msg]),
    gen_udp:send(Socket, Msg),
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("get cast ~p~n", [Msg]),
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
handle_info({'EXIT', Pid, _}, State) ->
    CallId = erase(Pid),
    erase(CallId),
    io:format("erase complete~n"),
    {noreply, State};
handle_info({udp, Port, _A, _P , Msg}, #state{conf_port = Port} = State) ->
    {ok, Sip} = esip:decode(Msg),
    io:format("conf <------------------~p~n", [Sip]),
    CallId = find_callid(Sip),
    Pid = get(CallId),
    gen_server:cast(Pid, {confserver, Sip}),
    {noreply, State};
handle_info({udp, Port, _A, _P , Msg}, #state{client_port = Port} = State) ->
    {ok, Sip} = esip:decode(Msg),
    io:format("client <================= ~p~n", [Sip]),
    CallId = find_callid(Sip),
    Pid = get(CallId),
    gen_server:cast(Pid, {clientserver, Sip}),
    {noreply, State};
handle_info(Info, State) ->
    io:format("get info ~p~n", [Info]),
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
init_server({server, Conf, Client}) ->
    {init_port(?CONFPORT, Conf), init_port(?CLIENTPORT, Client)}.

init_port(LocalPort, {_, Addr, Port}) ->
    {ok, Socket} = gen_udp:open(LocalPort, [binary]),
    ok = gen_udp:connect(Socket, Addr, Port),
    Socket.

find_callid(#sip{hdrs = Headers}) ->
    {_, CallId} = lists:keyfind('call-id', 1, Headers),
    CallId.
