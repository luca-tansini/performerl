-module(custom_meta_tracer_test).

-include_lib("eunit/include/eunit.hrl").

custom_meta_tracer_test() ->
    Self = self(),
    Tracer = spawn(fun() ->
        receive M -> Self ! M end
    end),
    erlang:trace_pattern({?MODULE, hello, 0}, true,
                         [{meta, performerl_custom_meta_tracer, Tracer}]),
    timer:sleep(100),
    hello(),
    receive
        {custom_trace, Self, {?MODULE, hello, 0}} -> ok
    after
        1000 -> throw(timeout)
    end,
    ok.

respect_ms_test() ->
    Self = self(),
    Tracer = spawn(fun() -> 
        receive M -> Self ! M end
    end),
    erlang:trace_pattern({?MODULE, hello, 0},
                         [{'_', [{'==',{self},{const, Self}}], []}],
                         [{meta, performerl_custom_meta_tracer, Tracer}]),
    timer:sleep(100),
    hello(),
    receive
        {custom_trace, Self, {?MODULE, hello, 0}} -> ok
    after
        1000 -> throw(timeout)
    end,
    spawn(fun() -> hello end),
    receive
        {custom_trace, _, {?MODULE, hello, 0}} -> throw(unexpected_trace)
    after
        1000 -> ok
    end,
    ok.

hello() -> hello.
