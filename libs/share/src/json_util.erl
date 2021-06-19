-module(json_util).

-export([encode/1, decode/1]).

%%%=========================================================================
%%% 数组格式为:[1,2,3,4,5]
%%% 字典格式为:[{key, val}, {key, val}]
%%% List转Json可用atom表示字符串
%%% Json转List字符串用List表示
%%%=========================================================================

encode(List) when is_list(List) ->
  jiffy:encode(list_json_format(List)).

decode(Json) ->
  json_list_format(jiffy:decode(Json)).

% jiffy对json结构进行解码得到的list输出结构 -> proplist
json_list_format({V}) ->
  json_list_format(V);
json_list_format({K, V}) ->
  {json_list_format(K), json_list_format(V)};
json_list_format(List) when is_list(List) ->
  [json_list_format(One) || One <- List];
json_list_format(V) ->
  case is_binary(V) of 
    true  ->
      binary_to_list(V);
    false ->
      V
  end.

% proplist -> 能被jiffy接受的list输入格式
list_json_format([]) ->
  [];
list_json_format(List) when is_list(List) ->
  V = [list_json_format(One) || One <- List],
  case List =/= [] andalso is_tuple(lists:last(List)) of
    true  ->
      {V};
    false ->
      V
  end;
list_json_format({K, V}) ->
  {list_json_format(K), list_json_format(V)};
list_json_format(V) when is_atom(V) orelse is_integer(V) orelse is_float(V) orelse is_binary(V) ->
  V;
list_json_format(V) ->
  list_to_binary(V).