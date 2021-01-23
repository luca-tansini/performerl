-module(garbage_collection_tracing_test).

-include_lib("eunit/include/eunit.hrl").

trace_gc_and_call_with_different_procs_test() ->
    Self = self(),
    GCTracer = spawn(fun() ->
        erlang:trace(Self, true, [garbage_collection]),
        receive
            Any -> Self ! {self(), Any}
        after 2000 ->
            Self ! {self(), got_nothing}
        end
    end),
    CallTracer = spawn(fun() ->
        erlang:trace(Self, true, [call]),
        erlang:trace_pattern({?MODULE, hello, 0}, true, call),
        receive
            Any -> Self ! {self(), Any}
        after 2000 ->
            Self ! {self(), got_nothing}
        end
    end),
    timer:sleep(100),
    hello(),
    erlang:garbage_collect(),
    receive 
        {GCTracer, got_nothing} -> gc = got_nothing;
        {GCTracer, TraceMsg} -> ?debugFmt("~nGCTrace got: ~p~n", [TraceMsg])
    end,
    % this means CallTracer is dead, because erlang:trace raised badarg
    ?assertEqual(undefined, process_info(CallTracer)).

hello() ->
    hello_world.