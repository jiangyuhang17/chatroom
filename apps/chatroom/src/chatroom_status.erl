-module(chatroom_status).

-include("com_def.hrl").

-export([init/2]).

init(Req0, State) ->
  IP     = cowboy_util:get_ip(Req0),
  RetMsg = [{status, open}, {ip, list_to_binary(IP)}],
  Req    = cowboy_req:reply(200, #{
    <<"content-type">> => <<"application/json">>
  }, json_util:encode(RetMsg), Req0),
  {ok, Req, State}.