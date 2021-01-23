-module(performerl_tracing_impact_benchmark).

%*******************************************************************************
% This benchmark is meant to determine the impact of tracing and
% meta-tracing, so that we can sanitize the call_time results accordingly
%*******************************************************************************

% this number defines how many function calls are made to get an average.
% It doesn't matter for the impact of the tacing,
% but increasing it slows down the test.
-define(NUM_OF_CALLS, 4000).

-export([benchmark/1]).

% the result of the benchmark is, for each single test size, the impact of the
% meta-tracing on one single function call
benchmark(0) -> 0;
benchmark(Size) ->
    % first, run a reference test, without any
    % tracing enabled, on one single process
    Ref = benchmark_work(?NUM_OF_CALLS),

    % then run it with the both the discovery and call_time tracing 
    Self = self(),
    Pids = [First|_] = spawn_loop(Size, [], Self),
    set_tracing(Pids),
    First ! start,
    Avg = receive_and_calc_mean(Size),
    remove_tracing(),
    % impact = avg time for a single traced call - reference time
    Avg - Ref.

%*******************************************************************************
% internal functions
%*******************************************************************************

set_tracing(Pids) ->
    erlang:trace(processes, false, [call]),
    erlang:trace(processes, true, [call]),
    erlang:trace_pattern({?MODULE, work, 1}, true, [call_time]),
    MS = performerl_process_discoverer_agent:create_pid_ms(Pids, tree),
    erlang:trace_pattern({?MODULE, work, 1}, MS, [meta]),
    timer:sleep(50).

remove_tracing() ->
    erlang:trace(processes, false, [call]),
    erlang:trace_pattern({?MODULE, work, 1}, false, [call_time, meta]).

% the first one to spawn (Pids=[]) is the last one to start and will not send
% any start message
spawn_loop(N, [], MainPid) ->
    Pid = spawn(fun() ->
        receive start -> ok end,
        MainPid ! {time, benchmark_work(?NUM_OF_CALLS)}
    end),
    spawn_loop(N-1, [Pid], MainPid);

% the last one to spawn (N=1) is the first one to be started, manually
spawn_loop(1, [Next|TL], MainPid) ->
    Pid = spawn(fun() ->
        receive start -> ok end,
        MainPid ! {time, benchmark_work(?NUM_OF_CALLS)},
        Next ! start
    end),
    [Pid, Next | TL];

% the ones in the middle receive the start message
% from the previous one and send it to the next
spawn_loop(N, [Next|TL], MainPid) ->
    Pid = spawn(fun() ->
        receive start -> ok end,
        MainPid ! {time, benchmark_work(?NUM_OF_CALLS)},
        Next ! start
    end),
    spawn_loop(N-1, [Pid,Next|TL], MainPid).

receive_and_calc_mean(NumOfProcs) ->
    receive_and_calc_mean(NumOfProcs,0) / NumOfProcs.
receive_and_calc_mean(0, Acc) -> Acc;
receive_and_calc_mean(NumOfProcs, Acc) ->
    receive
        {time, T} -> T
    end,
    receive_and_calc_mean(NumOfProcs-1, Acc+T).

% return the average time to make one of this calls, in microseconds
% (more than 4000 reds so there should also be scheduling involved)
benchmark_work(NumOfCalls) ->
    {T, _} = timer:tc(fun() ->
        work(NumOfCalls) 
    end),
    T / NumOfCalls.

work(1) -> ok;
work(N) -> work(N-1).
