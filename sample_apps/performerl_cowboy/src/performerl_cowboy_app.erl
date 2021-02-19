%%%-------------------------------------------------------------------
%% @doc performerl_cowboy public API
%% @end
%%%-------------------------------------------------------------------

-module(performerl_cowboy_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", hello_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 4242}],
        #{env => #{dispatch => Dispatch}}
    ),
    performerl_cowboy_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
