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
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include_lib("p1_sip/include/esip.hrl").

-record(state, { script
               , server
               , script_step
               , sessions
               , sess_init_num = 0
               , sess_num = 0}).

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
start_link(Script, Server) ->
    gen_server:start_link(?MODULE, [Script, Server], []).

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
init([Script, Server]) ->
    process_flag(trap_exit, true),
    gen_server:cast(self(), self_start),
    {ok, #state{ script = Script
               , server = Server
               , script_step = start}}.

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
          , script = Script} = State,
    case Script of
        [] ->
            {stop, normal, State};
        [{create, session, Names}|RestCommand] ->
            Sessions = lists:map(fun(S) ->
                                  io:format("new session ~p~n", [S]),
                                  {ok, Pid} = session:start_link(Server),
                                  put(Pid, S),
                                  put(S, Pid),
                                  Pid
                          end
                          , Names),
            {noreply, State#state{ sess_num = length(Sessions)
                                 , sessions = Sessions
                                 , script = RestCommand}};
        _ ->
            io:format("Why not create session first?~n"),
            {stop, normal, State}
    end;
handle_cast(init_complete, #state{ sess_num = Num
                                 , sess_init_num = InitNum} = State) when Num > InitNum + 1->
    {noreply, State#state{sess_init_num = InitNum + 1}};

handle_cast(init_complete, #state{ sess_num = Num
                                 , sess_init_num = InitNum
                                 , script = Script} = State) when Num =:= InitNum + 1->
    io:format("init complete goto ~p~n", [Script]),
    run_script(Script, State);

handle_cast(Msg, State) ->
    io:format("~p get unknow cast ~p~n", [?MODULE, Msg]),
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
    SName = erase(Pid),
    erase(SName),
    io:format("erase session ~p with pid ~p~n", [SName, Pid]),
    case State#state.sess_num - 1 of
        0 ->
            {stop, normal, State};
        NSessNum ->
            {noreply, State#state{sess_num = NSessNum}}
    end;

handle_info(timeout, #state{script = Script} = State) ->
    io:format("after pause~n"),
    run_script(Script, State);

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
run_script(Script, State) ->
    case Script of
        [] ->
            {stop, normal, State};
        [Command|RestCommand] ->
            case Command of
                {destroy, session, _} ->
                    %% now only support destroy all
                    Sessions = State#state.sessions,
                    lists:foreach(fun(S) ->
                                          gen_server:cast(S, destroy)
                                  end, Sessions),
                    {noreply, State#state{script = RestCommand}};
                {Server, Sess, Comms} when Server =:= confserver
                                        ; Server =:= clientserver ->
                    Pid = get(Sess),
                    io:format("send ~p to ~p~n", [{Server, Comms}, Pid]),
                    lists:foreach(fun(Comm) ->
                                          gen_server:cast(Pid, {Server, Comm})
                                  end, Comms),
                    run_script(RestCommand, State);
                {pause, Timeout} ->
                    io:format("pause for ~pms~n", [Timeout]),
                    {noreply, State#state{script = RestCommand}, Timeout};
                _ ->
                    io:format("unknow comm ~p~n", [Command]),
                    run_script(RestCommand, State)
            end;
        _ ->
            io:format("bad script? ~p~n", [Script]),
            {stop, normal, State}
    end.
