-module(chatroom_status).

-include("com_def.hrl").

-export([init/2]).

init(Req, Opts) ->
  IP = cowboy_util:get_ip(Req),
  RetMsg = [{status, open}, {ip, list_to_binary(IP)}],
  NReq = cowboy_req:reply(200, #{
    <<"content-type">> => <<"application/json">>
  }, json_util:encode(RetMsg), Req),
  {ok, NReq, Opts}.