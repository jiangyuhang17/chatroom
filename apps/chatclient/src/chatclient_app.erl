%%%-------------------------------------------------------------------
%% @doc chatclient public API
%% @end
%%%-------------------------------------------------------------------

-module(chatclient_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    chatclient_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
