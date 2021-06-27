-module(time_util).

-include("com_def.hrl").

-export([datetime/0]).

%% @spec "2021-5-29 22:24:36"
datetime() ->
  {{Y, M, D}, {H, MM, S}} = time_util:local_time(),
  lists:concat([Y, '-', M, '-', D, '_', H, ':', MM, ':', S]).