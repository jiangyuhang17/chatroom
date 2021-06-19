-module(connection).

-include("com_def.hrl").

-export([start_link/0]).

-export([init/2]).

-export([websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-record(state, {uname, ip}).

start_link() ->
  {ok, WsPort} = config:get(chatroom, port),
  ?INFO("roomchat port: ~p------------------", [WsPort]),
  Dispatch     = cowboy_router:compile([
    {'_', [
      {"/status", chatroom_status, []},
      {'_', ?MODULE, []}
    ]}
  ]),
  cowboy:start_clear(http, [{port, WsPort}], #{
    env => #{dispatch => Dispatch}
  }).

init(Req0, _State) ->
  process_flag(trap_exit, true),
  IP = cowboy_util:get_ip(Req0),
  {cowboy_websocket, Req0, #state{ip = IP}}.

websocket_init(State) ->
  ?DEBUG("[~p] ~p websocket_init, ip=~p", [?MODULE, self(), State#state.ip]),
  {ok, State}.

websocket_handle(NetMsg = {login, {uname, UName}}, State) ->
  do_log_msg_(up, NetMsg, State),
  {reply, {ok, login}, State#state{uname = UName}};
websocket_handle(NetMsg, State) ->
  do_log_msg_(up, NetMsg, State),
  {ok, State}.

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

do_log_msg_(up, NetMsg, State) ->
  ?DEBUG("[~p] ~p UP NetMsg=~p State=~p", [?MODULE, self(), NetMsg, State]);
do_log_msg_(down, NetMsg, State) ->
  ?DEBUG("[~p] ~p DOWN NetMsg=~p State=~p", [?MODULE, self(), NetMsg, State]);
do_log_msg_(info, NetMsg, State) ->
  ?DEBUG("[~p] INFO ~p NetMsg=~p State=~p", [?MODULE, self(), NetMsg, State]).