-module(handler).

-include("com_def.hrl").
-include("err_code.hrl").

-export([start_link/0]).

-export([init/2]).

-export([websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-define(CLIENT_PID_ETS, client_pid_ets).
-define(SERVER_DB, "chat_db").
-define(CHAT_ID, chat_id).
-define(CHAT_PRE, "chat_").

-record(state, {uname, ip, hb_cnt}).
-record(client, {uname, pid}).

start_link() ->
  ets:new(?CLIENT_PID_ETS, [set, public, {keypos, #client.uname}, named_table]),
  {ok, WsPort} = config:get(chatroom, port),
  ?INFO("roomchat port: ~p------------------", [WsPort]),
  Dispatch     = cowboy_router:compile([
    {'_', [
      {"/status", chatroom_status, []},
      {"/chatroom", ?MODULE, []}
    ]}
  ]),
  cowboy:start_clear(http, [{port, WsPort}], #{
    env => #{dispatch => Dispatch}
  }).

init(Req0, _State) ->
  process_flag(trap_exit, true),
  IP = cowboy_util:get_ip(Req0),
  {cowboy_websocket, Req0, #state{uname = "anonymous", ip = IP, hb_cnt = 0}}.

websocket_init(State) ->
  ?DEBUG("[~p] ~p websocket_init, ip=~p", [?MODULE, self(), State#state.ip]),
  {ok, State}.

websocket_handle(NetMsg = {binary, Bin}, State) ->
  do_log_msg_(up, NetMsg, State),
  do_websocket_handle_(binary_to_term(Bin), State);
websocket_handle(NetMsg = {text, <<"heart_beat">>}, State = #state{hb_cnt = HbCnt}) ->
  do_log_msg_(up, NetMsg, State),
  {reply, {text, "heart_beat"}, State#state{hb_cnt = HbCnt + 1}};
websocket_handle(NetMsg, State) ->
  do_log_msg_(up, NetMsg, State),
  {reply, {fail, []}, State}.

websocket_info({text, "another login"} = NetMsg, State) ->
  do_log_msg_(info, NetMsg, State),
  {reply, {text, "another login"}, State};
websocket_info({binary, DnMsg} = NetMsg, State) ->
  do_log_msg_(info, NetMsg, State),
  {reply, {binary, DnMsg}, State};
websocket_info(NetMsg, State) ->
  do_log_msg_(info, NetMsg, State),
  {ok, State}.

terminate(Reason, _Req, State = #state{uname = UName}) ->
  case Reason of
    stop ->
      %% 服务器主动{reply, {close, Payload}}关闭
      ?INFO("[~p]~p UName:~p, websocket closed by server, state:~p", [?MODULE, self(), UName, State]);
    timeout ->
      %% 客户端idle timeout
      ?INFO("[~p]~p UName:~p, websocket closed due to timeout, state:~p", [?MODULE, self(), UName, State]);
    {remote,Code,Payload} ->
      %% 客户端主动关闭
      ?INFO("[~p]~p UName:~p, websocket closed by client (code:~p,payload:~p), state:~p", [?MODULE, self(), UName, Code, Payload, State]);
    Error ->
      %% 其他错误
      ?INFO("[~p]~p UName:~p, websocket closed unexpectedly (reason:~p), state:~p", [?MODULE, self(), UName, Error, State])
  end,
  ets:delete(?CLIENT_PID_ETS, UName),
  ok.

%% internal functions

do_websocket_handle_({signup, {UName, Token, LocalID}}, State) ->
  case kv_util:get(?SERVER_DB, "UName_" ++ UName) of
    not_found ->
      kv_util:set(?SERVER_DB, "UName_" ++ UName, Token),
      ets:insert(?CLIENT_PID_ETS, #client{uname = UName, pid = self()}),
      ChatID  = get_chat_id_(),
      Records = get_record_from_bitcask_(LocalID, ChatID, []),
      DnMsg = {ok, {signup, UName, ChatID, Records}},
      do_log_msg_(down, DnMsg, State),
      {reply, {binary, term_to_binary(DnMsg)}, State#state{uname = UName}};
    _         ->
      DnMsg = {fail, ?ERR_USER_ALREADY_EXIST},
      do_log_msg_(down, DnMsg, State),
      {reply, {binary, term_to_binary(DnMsg)}, State}
  end;
do_websocket_handle_({signin, {UName, Token, LocalID}}, State) ->
  Bin = term_to_binary(Token),
  case kv_util:get(?SERVER_DB, "UName_" ++ UName) of
    not_found ->
      DnMsg = {fail, ?ERR_USER_NOT_EXIST},
      do_log_msg_(down, DnMsg, State),
      {reply, {binary, term_to_binary(DnMsg)}, State};
    {ok, Bin} ->
      case ets:lookup(?CLIENT_PID_ETS, UName) of
        [] -> ok;
        [#client{pid = Pid}] ->
          Pid ! {text, "another login"}
      end,
      ets:insert(?CLIENT_PID_ETS, #client{uname = UName, pid = self()}),
      ChatID  = get_chat_id_(),
      Records = get_record_from_bitcask_(LocalID, ChatID, []),
      DnMsg = {ok, {signin, UName, ChatID, Records}},
      do_log_msg_(down, DnMsg, State),
      {reply, {binary, term_to_binary(DnMsg)}, State#state{uname = UName}};
    _ ->
      DnMsg = {fail, ?ERR_WRONG_TOKEN},
      do_log_msg_(down, DnMsg, State),
      {reply, {binary, term_to_binary(DnMsg)}, State}
  end;
do_websocket_handle_({content, Content}, State = #state{uname = UName}) ->
  NChatID = get_chat_id_() + 1,
  Key   = "chat_" ++ integer_to_list(NChatID),
  Value = #chat{id = NChatID, uname = UName, ts = ?NOW, content = Content},
  kv_util:set(?SERVER_DB, Key, Value),
  kv_util:set(?SERVER_DB, ?CHAT_ID, NChatID),
  DnMsg = {ok, {broadcast, Value}},
  do_log_msg_(down, DnMsg, State),
  Acc = ets:foldl(fun(#client{pid = Pid}, Acc) ->
                    Pid ! {binary, term_to_binary(DnMsg)},
                    Acc + 1
                  end, 0, ?CLIENT_PID_ETS),
  {reply, {binary, term_to_binary({ok, {content, Acc}})}, State};
do_websocket_handle_(_, State) ->
  DnMsg = {fail, ?ERR_UNEXPECTED_REQUEST},
  do_log_msg_(down, DnMsg, State),
  {reply, {binary, term_to_binary(DnMsg)}, State}.

get_record_from_bitcask_(ID, ChatID, ACC) when ChatID > ID ->
  Key   = ?CHAT_PRE ++ integer_to_list(ChatID),
  NACC  = case kv_util:get(?SERVER_DB, Key) of
            not_found -> ACC;
            {ok, Bin} -> [binary_to_term(Bin) | ACC]
          end,
  get_record_from_bitcask_(ID, ChatID - 1, NACC);
get_record_from_bitcask_(_, _, ACC) ->
  ACC.

get_chat_id_() ->
  case kv_util:get(?SERVER_DB, ?CHAT_ID) of
    not_found -> 0;
    {ok, Bin} -> binary_to_term(Bin)
  end.

do_log_msg_(up, {binary, Bin}, State) ->
  ?DEBUG("[~p] ~p UP NetMsg={binary, ~p} State=~p", [?MODULE, self(), binary_to_term(Bin), State]);
do_log_msg_(up, NetMsg, State) ->
  ?DEBUG("[~p] ~p UP NetMsg=~p State=~p", [?MODULE, self(), NetMsg, State]);
do_log_msg_(down, NetMsg, State) ->
  ?DEBUG("[~p] ~p DOWN NetMsg=~p State=~p", [?MODULE, self(), NetMsg, State]);
do_log_msg_(info, NetMsg, State) ->
  ?DEBUG("[~p] INFO ~p NetMsg=~p State=~p", [?MODULE, self(), NetMsg, State]).