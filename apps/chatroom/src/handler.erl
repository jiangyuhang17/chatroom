-module(handler).

-include("com_def.hrl").
-include("err_code.hrl").

-export([start_link/0]).

-export([init/2]).

-export([websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-record(state, {uname, ip, hb_cnt}).

-define(PID_NAME_ETS, pid_name_ets).

start_link() ->
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
  do_websocket_handle(binary_to_term(Bin), State);
websocket_handle({text, <<"heart_beat">>}, State = #state{hb_cnt = HbCnt}) ->
  {reply, {text, "heart_beat"}, State#state{hb_cnt = HbCnt + 1}};
websocket_handle(NetMsg, State) ->
  do_log_msg_(up, NetMsg, State),
  {reply, {fail, []}, State}.

do_websocket_handle({login, {uname, UName}}, State) ->
  {reply, {binary, term_to_binary({ok, login})}, State#state{uname = UName}};
do_websocket_handle(_, State) ->
  {reply, {?ERR_UNEXPECTED_REQUEST}, State}.

websocket_info(NetMsg, State) ->
  do_log_msg_(info, NetMsg, State),
  {ok, State}.

terminate(Reason, _Req, State) ->
  %% TODO chatroom delete UName from ets about broadcast
  terminate_(Reason, State);
terminate(Reason, _Req, State) ->
  terminate_(Reason, State).

terminate_(Reason, State = #state{uname = UName}) ->
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
  ok.

do_log_msg_(up, {binary, Bin}, State) ->
    ?DEBUG("[~p] ~p UP NetMsg={binary, ~p} State=~p", [?MODULE, self(), binary_to_term(Bin), State]);
do_log_msg_(up, NetMsg, State) ->
  ?DEBUG("[~p] ~p UP NetMsg=~p State=~p", [?MODULE, self(), NetMsg, State]);
do_log_msg_(down, NetMsg, State) ->
  ?DEBUG("[~p] ~p DOWN NetMsg=~p State=~p", [?MODULE, self(), NetMsg, State]);
do_log_msg_(info, NetMsg, State) ->
  ?DEBUG("[~p] INFO ~p NetMsg=~p State=~p", [?MODULE, self(), NetMsg, State]).