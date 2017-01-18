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

-include_lib("esip.hrl").

-define(SERVER, ?MODULE).
-define(CONFPORT, 5060).
-define(CLIENTPORT, 5061).

-record(state, { conf_port
               , client_port
               , server
               , script
               , callid
               , rate
               , timer}).

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
    esip:add_listener(5060, udp, []),
    process_flag(trap_exit, true),
    io:format("core dispatch started with: ~p~n", [ScriptName]),
    case file:consult(ScriptName) of
        {ok, Script} ->
            [Server|RealScript] = Script,
            gen_server:cast(self(), {start, {rate, 1}}), %%启动相关参数加在这里，
                                                 %%如每秒新发，最大并发
            {ok, #state{ server = Server
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
handle_cast({start, {rate, N}}, State) ->
    {ok, TRef} = timer:send_interval(1000, rate),
    {noreply, State#state{ rate = N
                         , timer = TRef}};

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
    case erase(Pid) of
        undefined ->
            io:format("erase script ~p~n", [Pid]),
            {noreply, State};
        CallId ->
            erase(CallId),
            io:format("clean callid ~p~n", [CallId]),
            {noreply, State}
    end;

handle_info(rate, State) ->
    #state{ script = Script
          , server = Server
          , rate = N
          , timer = TRef} = State,
    timer:cancel(TRef),
    lists:foreach(fun(_) ->
        case execute_script:start_link(Script, Server) of
            {ok, _Pid} ->
                ok;
            {error, _Reason} ->
                ok
        end
        end, lists:seq(1,N)),
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
