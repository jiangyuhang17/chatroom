-module(connection).

-behaviour(gen_server).

-include("com_def.hrl").

-include("err_code.hrl").

-export([start_link/0, stop/1]).

%% client api
-export([login/1, send/1, get_pid/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CONN_PID_ETS, conn_pid_ets).
-define(CLIENT_DB, "client_db").
-define(LOCAL_ID, local_id).
-define(CHAT_PRE, "chat_").
-define(MSG_REPLY_TIME_OUT, 1000).
-define(HEART_BEAT_INTERVAL, 1000).

-record(state, {ws_conn, m_ref, stream_ref, hb_cnt, uname}).
-record(chat, {id, uname, ts, content}).

start_link() ->
  {ok, Host} = config:get(chatroom, host),
  {ok, Port} = config:get(chatroom, port),
  {ok, Pid}  = gen_server:start(?MODULE, [Host, Port], []),
  ets:new(?CONN_PID_ETS, [set, public, named_table]),
  ets:insert(?CONN_PID_ETS, {pid, Pid}),
  erlang:send_after(?HEART_BEAT_INTERVAL, Pid, "heart_beat"),
  {ok, Pid}.

stop(WsPid) ->
  ?INFO("[~p], ws_client stop is called, ~p:~p ~n", [?MODULE, WsPid, self()]),
  WsPid ! stop.

%% client api

login(UName) ->
  gen_server:call(get_pid(), {login, UName}).

send(Content) ->
  gen_server:call(get_pid(), {content, Content}).

get_pid() ->
  [{pid, Pid}|_] = ets:lookup(?CONN_PID_ETS, pid),
  Pid.

%% gen_server callbacks

init([Host, Port]) ->
  process_flag(trap_exit, true),
  LocalID  = get_local_id_(),
  show_local_record_(LocalID, 1),
  {WsConn, MRef, StreamRef} = ws_login_(Host, Port),
  {ok, #state{ws_conn = WsConn, m_ref = MRef, stream_ref = StreamRef, hb_cnt = 0, uname = "anonymous"}}.

handle_call({login, _} = Msg, _From, State = #state{ws_conn = ConnPid, m_ref = MRef}) ->
  ok = gun:ws_send(ConnPid, {binary, term_to_binary(Msg)}),  
  wait_receive_(Msg, ConnPid, MRef, State);
handle_call({content, _} = Msg, _From, State = #state{ws_conn = ConnPid, m_ref = MRef}) ->
  ok = gun:ws_send(ConnPid, {binary, term_to_binary(Msg)}),  
  wait_receive_(Msg, ConnPid, MRef, State);
handle_call(_Request, _From, State) ->
  ?DEBUG("[~p] ws_client handle_call,  msg:~p , from:~p, state:~p ~n", [?MODULE, _Request, _From, State]),
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({gun_ws, Pid, _StreamRef, close}, State) ->
  gun:ws_send(Pid, close),
  {noreply, State};
handle_info("heart_beat", State = #state{ws_conn = ConnPid, m_ref = MRef}) ->
  LocalID = get_local_id_(),
  gun:ws_send(ConnPid, {binary, term_to_binary({get, LocalID})}),
  erlang:send_after(?HEART_BEAT_INTERVAL, self(), "heart_beat"),
  wait_receive_({get, LocalID}, ConnPid, MRef, State);
handle_info({'DOWN', MRef, process, Pid, normal}, State = #state{}) ->
  ?DEBUG("[~p:~p] ws_client handle_info,  down is called ~n", [?MODULE, ?LINE]),
  close_(Pid, MRef),
  {noreply, State};
handle_info(stop, #state{ws_conn = ConnPid, m_ref = MRef}) ->
  ?DEBUG("[~p:~p] ws_client handle_info,  stop is called ~n", [?MODULE, ?LINE]),
  close_(ConnPid, MRef),
  exit(normal);
handle_info(_Info, State) ->
  ?DEBUG("[~p:~p]  ws_client handle_info,  unexpect Msg:~p, ~n", [?MODULE, ?LINE, _Info]),
  {noreply, State}.

terminate(Reason, State = #state{ws_conn = ConnPid, m_ref = MRef}) ->
  close_(ConnPid, MRef),
  ?DEBUG("[~p] is terminate, reason:~p, state:~p ------ self:~w ~n", [?MODULE, Reason, State, self()]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% internal functions

get_local_id_() ->
  case kv_util:get(?CLIENT_DB, ?LOCAL_ID) of
    not_found -> 0;
    {ok, Bin} -> binary_to_term(Bin)
  end.

show_local_record_(LocalID, IterID) when LocalID >= IterID ->
  ChatID = ?CHAT_PRE ++ integer_to_list(IterID),
  case kv_util:get(?CLIENT_DB, ChatID) of
    not_found -> ok;
    {ok, Bin} ->
      #chat{id = IterID, uname = N, ts = T, content = C} = binary_to_term(Bin),
      io:format("~n~p ~p ~p~n", [N, T, C])
  end,
  show_local_record_(LocalID, IterID + 1);
show_local_record_(_, _) ->
  ok.

ws_login_(Host, Port) ->
  ?INFO("[~p]ws_login is called, self:~p, Host:~p, Port:~p ~n", [?MODULE, self(), Host, Port]),
  {ok, _} = application:ensure_all_started(gun),
  {ok, ConnPid} = gun:open(Host, Port),
  MRef = monitor(process, ConnPid),
  {ok, _Protocol} = gun:await_up(ConnPid),
  gun:ws_upgrade(ConnPid, "/chatroom", [], #{compress => true}),
  receive
    {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
      upgrade_success_(ConnPid, StreamRef),
      {ConnPid, MRef, StreamRef};
    {gun_response, ConnPid, _, _, Status, Headers} ->
      exit({ws_upgrade_failed, Status, Headers});
    {gun_error, _ConnPid, _StreamRef, Reason} ->
      exit({ws_upgrade_failed, Reason});
    Reply ->
      ?DEBUG("[~p][http -> websocket] fail  self:~w, msg:~p ~n", [?MODULE, self(), Reply])
  after ?MSG_REPLY_TIME_OUT ->
    ?ERROR("[~p] ws_login is timeout after 2 sec self:~w, ~n", [?MODULE, self()]),
    exit(timeout)
  end.

upgrade_success_(ConnPid, StreamRef) ->
  ?DEBUG("[~p][http -> websocket] success  self:~w, connect:~w, ref:~p ~n", [?MODULE, self(), ConnPid,  StreamRef]),
  {ok, ConnPid}.

wait_receive_(Msg, ConnPid, MRef, State) ->
  receive
    {gun_ws, _Pid, _StreamRef,  {binary, RecvData} = _Frame} ->
      do_reply_handle_(binary_to_term(RecvData), State);
    {gun_down, Pid, ws, _, _, _} ->
      close_(Pid, MRef),
      {reply, {fail, ""}, State};
    {'DOWN', MRef, process, Pid, normal} ->
      close_(Pid, MRef),
      {reply, {fail, ""}, State};
    {gun_ws, _Pid, _StreamRef,  {close, RecvData} = _Frame} ->
      ?DEBUG("[~p] ws_client is closed, recvdata:~p  ~n", [?MODULE, RecvData]),
      close_(ConnPid, MRef);
    Other ->
      ?ERROR("[~p:~p] Unexpected message ~p", [?MODULE, ?FUNCTION_NAME, Other]),
      close_(ConnPid, MRef),
      {reply, {fail,""}, State}
  after ?MSG_REPLY_TIME_OUT ->
    ?ERROR("[~p] ws_client wait for response, msg:~p  ~n", [?MODULE, Msg]),
    {reply, {fail,""}, State}
  end.

do_reply_handle_({ok, {login, UName}}, State) ->
  ?DEBUG("[~p] do_reply_handle_ login user name ~p ~n", [?MODULE, UName]),
  {reply, {ok, login}, State#state{uname = UName}};
do_reply_handle_({ok, {content, Value}}, State) ->
  ?DEBUG("[~p] do_reply_handle_ chat record ~p ~n", [?MODULE, Value]),
  {reply, {ok, content}, State};
do_reply_handle_({ok, {get, ChatID, Records}}, State = #state{hb_cnt = HbCnt}) ->
  kv_util:set(?CLIENT_DB, ?LOCAL_ID, ChatID),
  lists:foreach(fun(R = #chat{id = ID, uname = N, ts = T, content = C}) -> 
                io:format("~n~p ~p ~p~n", [N, T, C]),
                Key = ?CHAT_PRE ++ integer_to_list(ID),
                kv_util:set(?CLIENT_DB, Key, R)
              end, Records),
  {noreply, State#state{hb_cnt = HbCnt + 1}};
do_reply_handle_(_, State) ->
  ?DEBUG("[~p] do_reply_handle_ unexpected request ~n"),
  {reply, {fail, ?ERR_UNEXPECTED_REQUEST}, State}.

close_(Pid, MRef) ->
  demonitor(MRef),
  gun:close(Pid),
  ?INFO("[~p], ws_client close is called, ~p:~p ~n", [?MODULE, self(), Pid]),
  gun:flush(Pid).