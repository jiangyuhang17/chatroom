-module(connection).

-behaviour(gen_server).

-include("com_def.hrl").

-export([start_link/0, stop/1]).
-export([create_or_login/1]).

%% gen_server callbacks
-export([
  init/1
  ,handle_call/3
  ,handle_cast/2
  ,handle_info/2
  ,terminate/2
  ,code_change/3
]).

-define(CONN_PID_ETS, conn_pid_ets).
-define(MSG_REPLY_TIME_OUT, 1000).
-define(HEART_BEAT_INTERVAL, 1000). %% purge connection timer, 5 sec check once

-record(state, {ws_conn, m_ref, stream_ref, hb_cnt, uname}).

start_link() ->
  {ok, Host} = config:get(chatroom, host),
  {ok, Port} = config:get(chatroom, port),
  {ok, Pid}  = gen_server:start(?MODULE, [Host, Port], []),
  ets:new(?CONN_PID_ETS, [set, public, named_table]),
  ets:insert(?CONN_PID_ETS, {pid, Pid}),
  erlang:send_after(?HEART_BEAT_INTERVAL, Pid, "heart_beat"),
  {ok, Pid}.

create_or_login(UName) ->
  Reply = gen_server:call(get_pid(), {create_or_login, UName}),
  ?INFO("--------------create_or_login reply ~p", [Reply]).


get_pid() ->
  [{pid, Pid}|_] = ets:lookup(?CONN_PID_ETS, pid),
  Pid.

%% gen_server callbacks

init([Host, Port]) ->
  process_flag(trap_exit, true),
  {WsConn, MRef, StreamRef} = ws_login(Host, Port),
  {ok, #state{ws_conn = WsConn, m_ref = MRef, stream_ref = StreamRef, hb_cnt = 0, uname = "anonymous"}}.

ws_login(Host, Port) ->
  ?INFO("[~p]ws_login is called, self:~p, Host:~p, Port:~p ~n", [?MODULE, self(), Host, Port]),
  {ok, _} = application:ensure_all_started(gun),
  {ok, ConnPid} = gun:open(Host, Port),
  MRef = monitor(process, ConnPid),
  {ok, _Protocol} = gun:await_up(ConnPid),
  gun:ws_upgrade(ConnPid, "/chatroom", [], #{compress => true}),
  receive
    {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
      upgrade_success(ConnPid, StreamRef),
      {ConnPid, MRef, StreamRef};
    {gun_response, ConnPid, _, _, Status, Headers} ->
      exit({ws_upgrade_failed, Status, Headers});
    {gun_error, _ConnPid, _StreamRef, Reason} ->
      exit({ws_upgrade_failed, Reason});
    Reply ->
      ?INFO("[~p][http -> websocket] fail  self:~w, msg:~p ~n", [?MODULE, self(), Reply])
  after ?MSG_REPLY_TIME_OUT ->
    ?ERROR("[~p] ws_login is timeout after 2 sec self:~w, ~n", [?MODULE, self()]),
    exit(timeout)
  end.

upgrade_success(ConnPid, StreamRef) ->
  ?INFO("[~p][http -> websocket] success  self:~w, connect:~w, ref:~p ~n", [?MODULE, self(), ConnPid,  StreamRef]),
  {ok, ConnPid}.

handle_call({create_or_login, UserName} = Msg, _From, State = #state{ws_conn = ConnPid, m_ref = MRef}) ->
  ok = gun:ws_send(ConnPid, {binary, term_to_binary({login, {uname, UserName}})}),  
  wait_receive(Msg, ConnPid, MRef, State);
handle_call(_Request, _From, State) ->
  ?DEBUG("[~p] ws_client handle_call,  msg:~p , from:~p, state:~p ~n", [?MODULE, _Request, _From, State]),
  {reply, ok, State}.

wait_receive(Msg, ConnPid, MRef, State = #state{hb_cnt = HbCnt}) ->
  receive
    {gun_ws, _Pid, _StreamRef, {text, <<"heart_beat">>}} ->
      {noreply, State#state{hb_cnt = HbCnt + 1}};
    {gun_ws, _Pid, _StreamRef,  {binary, RecvData} = _Frame} ->
      ?INFO("[~p] ok, recvdata:~p  ~n", [?MODULE, binary_to_term(RecvData)]),
      {reply, {}, State};
    {gun_down, Pid, ws, _, _, _} ->
      close(Pid, MRef),
      {reply, {fail, ""}, State};
    {'DOWN', MRef, process, Pid, normal} ->
      close(Pid, MRef),
      {reply, {fail, ""}, State};
    {gun_ws, _Pid, _StreamRef,  {close, RecvData} = _Frame} ->
      ?INFO("[~p] ws_client is closed, recvdata:~p  ~n", [?MODULE, RecvData]),
      close(ConnPid, MRef);
    Other ->
      ?ERROR("[~p:~p] Unexpected message ~p", [?MODULE, ?FUNCTION_NAME, Other]),
      close(ConnPid, MRef),
      {reply, {fail,""}, State}
  after ?MSG_REPLY_TIME_OUT ->
    ?ERROR("[~p] ws_client wait for response, msg:~p  ~n", [?MODULE, Msg]),
    {reply, {fail,""}, State}
  end.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({gun_ws, Pid, _StreamRef, close}, State) ->
  gun:ws_send(Pid, close),
  {noreply, State};
handle_info("heart_beat", State = #state{ws_conn = ConnPid, m_ref = MRef}) ->
  gun:ws_send(ConnPid, {text, "heart_beat"}),
  erlang:send_after(?HEART_BEAT_INTERVAL, self(), "heart_beat"),
  wait_receive({text, "heart_beat"}, ConnPid, MRef, State);
handle_info({'DOWN', MRef, process, Pid, normal}, State) ->
  ?DEBUG("[~p:~p] ws_client handle_info,  down is called ~n", [?MODULE, ?LINE]),
  close(Pid, MRef),
  {noreply, State};
handle_info(stop, _State) ->
  ?DEBUG("[~p:~p] ws_client handle_info,  stop is called ~n", [?MODULE, ?LINE]),
  exit(normal);
handle_info(_Info, State) ->
  ?DEBUG("[~p:~p]  ws_client handle_info,  unexpect Msg:~p, ~n", [?MODULE, ?LINE, _Info]),
  {noreply, State}.

terminate(Reason, State = #state{ws_conn = ConnPid, m_ref = MRef}) ->
  close(ConnPid, MRef),
  ?DEBUG("[~p] is terminate, reason:~p, state:~p ------ self:~w ~n", [?MODULE, Reason, State, self()]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

stop(WsPid) ->
  ?INFO("[~p], ws_client stop is called, ~p:~p ~n", [?MODULE, WsPid, self()]),
  WsPid ! stop.

close(Pid, MRef) ->
  demonitor(MRef),
  gun:close(Pid),
  ?INFO("[~p], ws_client close is called, ~p:~p ~n", [?MODULE, self(), Pid]),
  gun:flush(Pid).