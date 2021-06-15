%%%-------------------------------------------------------------------
%% @doc chatroom public API
%% @end
%%%-------------------------------------------------------------------

-module(chatroom_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    chatroom_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
