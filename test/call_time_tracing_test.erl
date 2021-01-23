-module(call_time_tracing_test).

-include_lib("eunit/include/eunit.hrl").

-export([hello/0, goodbye/0]).

%*****************************************************************************
% CALL_TIME tracing interaction experiments
%*****************************************************************************

call_time_tracing_test() ->

    Self = self(),

    % 1 - What happens if we set call_time before any tracer is set?
    set_trace_pattern(),
    % this returning an empty list means call_time tracing is active,
    % otherwise it would be false, but it's not working
    % because the counter is not incremented
    ?assertEqual({call_time, []}, get_call_time_info()),
    make_traced_call(),
    ?assertEqual({call_time, []}, get_call_time_info()),
    % proof: for a different function it's false
    ?assertEqual({call_time, false}, get_wrong_call_time_info()),

    % 1: call_time is set, but it doesn't work until someone registers
    %    as a tracer for the process, seems promising.

    % 2 - Now we set a tracer for all processes (which is also a
    %     different process).
    %     Do we have to set call_time again for the counters to work?
    Tracer = spawn_tracer(),
    make_traced_call(),
    {_, [{Self,1,_,_}]} = get_call_time_info(),

    % 2 : No, it starts working right away, GREAT!

    % 3 - What happens if we set the call_time flag again?
    {_, [{Self,1,_,_}]} = get_call_time_info(),
    set_trace_pattern(),
    {_, [{Self,0,_,_}]} = get_call_time_info(),
    make_traced_call(),
    {_, [{Self,1,_,_}]} = get_call_time_info(),
    % it's restarted, too BAD

    % what about using pause mspec?
    pause_trace_pattern(),
    make_traced_call(),
    {_, [{Self,1,_,_}]} = get_call_time_info(),
    restart_trace_pattern(),
    {_, [{Self,0,_,_}]} = get_call_time_info(),
    make_traced_call(),
    {_, [{Self,1,_,_}]} = get_call_time_info(),
    % not helpful

    % even if someone else does it?
    {_, [{Self,1,_,_}]} = get_call_time_info(),
    spawn(fun() -> set_trace_pattern() end),
    sync(),
    {_, []} = get_call_time_info(),
    make_traced_call(),
    {_, [{Self,1,_,_}]} = get_call_time_info(),
    % it's actually emptied

    % what if we use a different flag?
    {_, [{Self,1,_,_}]} = get_call_time_info(),
    set_trace_pattern([call_count, local, meta]),
    {call_time, [{Self,1,_,_}]} = get_call_time_info(),
    make_traced_call(),
    {call_time, [{Self,2,_,_}]} = get_call_time_info(),
    % not restarted with this ones, good

    % what about global?
    {_, [{Self,2,_,_}]} = get_call_time_info(),
    set_trace_pattern([global]),
    {call_time, false} = get_call_time_info(),
    % global shuts it down but it's expected

    % 3: Whenever we call trace_pattern with the same pattern:
    %     - if call_time is specified again,the counter is restarted
    %     - if call_time is not specified again, the counter goes on
    %     - if global is specified, call_time is removed, but it's expected
    %    Verdict: if repeating the call_time flag didn't restart the counter
    %    it would be better, but overall it's GOOD.

    % let's re enable call_time tracing
    set_trace_pattern(),
    make_traced_call(),
    {_, [{Self,1,_,_}]} = get_call_time_info(),

    % 4 - What happens if the tracer dies, do we lose the counters?
    teardown_tracer(Tracer),
    {_, X = [{Self,1,_,_}]} = get_call_time_info(),
    % seems not, but of course the counters are not increased anymore
    make_traced_call(),
    {_, X} = get_call_time_info(),

    % 4: Even if the tracing process dies, we don't lose the counter, GREAT!

    % 5 - What happens if then a new tracer is set?
    Tracer1 = spawn_tracer(),
    make_traced_call(),
    {_, [{Self,2,_,_}]} = get_call_time_info(),
    teardown_tracer(Tracer1),
    Tracer2 = spawn_tracer([call, return_to, set_on_spawn]),
    make_traced_call(),
    {_, [{Self,3,_,_}]} = get_call_time_info(),
    teardown_tracer(Tracer2),
    % and without the call flag?
    Tracer3 = spawn_tracer([send]),
    make_traced_call(),
    {_, [{Self,3,_,_}]} = get_call_time_info(),
    teardown_tracer(Tracer3).
    % 5: it doesnt't restart the counter (unless the call flag is not used),
    %    GREAT!

%*****************************************************************************
% internal functions
%*****************************************************************************

set_trace_pattern() ->
    erlang:trace_pattern({?MODULE, hello, 0}, true, [call_time]).
set_trace_pattern(Flags) ->
    erlang:trace_pattern({?MODULE, hello, 0}, true, Flags).
pause_trace_pattern() ->
    erlang:trace_pattern({?MODULE, hello, 0}, pause, [call_time]).
restart_trace_pattern() ->
    erlang:trace_pattern({?MODULE, hello, 0}, restart, [call_time]).
remove_trace_pattern() ->
    erlang:trace_pattern({?MODULE, hello, 0}, false, [call_time, call_count,
                                                      meta, local]).

get_call_time_info() ->
    erlang:trace_info({?MODULE, hello, 0}, call_time).

get_wrong_call_time_info() ->
    erlang:trace_info({?MODULE, goodbye, 0}, call_time).

make_traced_call() ->
    hello().

spawn_tracer() -> spawn_tracer([call]).
spawn_tracer(Flags) ->
    Pid = spawn_link(fun() ->
        set_tracer(Flags),
        receive
            stop ->
                erlang:trace(all, false, Flags)
        end
    end),
    sync(),
    Pid.

set_tracer(Flags) ->
    erlang:trace(all, true, Flags).

teardown_tracer(Pid) ->
    Pid ! stop,
    sync().

sync() ->
    timer:sleep(10).

%*****************************************************************************
% dummy functions
%*****************************************************************************
hello() -> hello.
goodbye() -> bye.
