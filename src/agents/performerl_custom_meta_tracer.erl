-module(performerl_custom_meta_tracer).

-export([enabled/3, trace/5, load/0]).

-on_load(load/0).

-ifdef(TEST).
load() ->
    % this is the path in which the compiled NIF will be placed
    % in the target nodes during eunit tests
    ok = erlang:load_nif("src/agents/performerl_custom_meta_tracer", []).
-else.
load() ->
    % this is the path in which the compiled NIF will be placed
    % in the target nodes during PerformErl tests
    ok = erlang:load_nif("performerl_tmp/performerl_custom_meta_tracer", []).
-endif.

enabled(_, _, _) ->
    error.

trace(_, _, _, _, _) ->
    error.