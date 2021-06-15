-module(connection).

-include("com_def.hrl").

-export([start_link/0]).

start_link() ->
    {ok, WsPort} = config:get(chatroom, port),
    ?INFO("roomchat port: ~p------------------", [WsPort]),
    Dispatch     = cowboy_router:compile([
        {'_', [
            {"/hello", hello, []},
            {'_', ?MODULE, []}
        ]}
    ]),
    {ok, Pid} = cowboy:start_clear(http, [{port, WsPort}], #{
        env => #{dispatch => Dispatch}
    }),
    {ok, Pid}.