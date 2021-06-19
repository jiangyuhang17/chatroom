-module(cowboy_util).

-export([get_ip/1]).

get_ip(Req) ->
  {IP, _Port} = cowboy_req:peer(Req),
  inet_parse:ntoa(IP).