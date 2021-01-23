-module(performerl_process_discoverer_agent_ms_benchmark).

-include_lib("eunit/include/eunit.hrl").

%******************************************************************************%
% This benchmark uses the actual processes_discoverer agent implementation to  %
% test the impact of the different tracing approaches, with increasing number  %
% of processes, assuming the processes have already been discovered.           %
% The results of this test were removed from the article, but may be kept      %
% in the thesis.                                                               %
%******************************************************************************%

% TODO: apply the same sampling technique of the tracing_overhead_test and
%       increase the processes numbers

% this number defines how many function calls are made to get an average.
% It doesn't matter for the impact of the tracing,
% increasing it improves precision but slows down the test.
% 4000 should always cause the process executing to reach the reductions limit
% and be scheduled out
-define(NUM_OF_CALLS, 4000).

meta_tracing_discoverer_benchmark_test_() ->
    {
        timeout,
        600,
        fun() ->
            ?debugFmt("\nMS Benchmark, this will take a while...\n",[]),
            L = lists:map(fun(N) ->
                        {N,meta_tracing_discoverer_timing_benchmark(N)}
                    end,
                    [10, 100, 250, 500, 1000, 2000]),
            ?debugFmt("~p\n", [L])
        end
    }.

%***********************************************
% benchmark functions
%***********************************************

meta_tracing_discoverer_timing_benchmark(NumOfProcesses) ->
    RT1 = benchmark(),
    RT2 = traced_benchmark(NumOfProcesses, list),
    RT3 = traced_benchmark(NumOfProcesses, tree),
    [{list, RT2-RT1}, {tree, RT3-RT1}].

traced_benchmark(NumOfProcs, MSType) ->
    Self = self(),
    spawn_discoverer(get_benchmark_patterns(), MSType),
    timer:sleep(50),
    Res = traced_benchmark2(NumOfProcs, Self),
    {ok, Procs}=performerl_process_discoverer_agent:get_discovered_processes(), 
    ?assertEqual(NumOfProcs, length(Procs)),
    stop_discoverer(),
    % wait for the processes to die
    timer:sleep(100),
    Res.

% every process runs the function, then we take the average
traced_benchmark2(NumOfProcs, Self) ->
    Pids = spawn_loop(Self, NumOfProcs, []),
    Res = receive_and_calc_mean(NumOfProcs),
    lists:foreach(fun(Pid) -> Pid ! stop end, Pids),
    Res.

spawn_loop(_Self, 0, [Pid|TL]) -> 
    % send the first start message to start the chain
    % after a small timeout to allow the discoverer to syncronize
    timer:sleep(500),
    Pid ! start,
    [Pid|TL];
% the first one to spawn will be the last one to receive
% the start message and doesn't send any start message
spawn_loop(Self, N, []) ->
    Pid = spawn(fun() ->
        make_call_to_be_detected(),
        receive start -> ok end,
        Self ! {time, benchmark()},
        receive stop -> ok end
    end),
    spawn_loop(Self, N-1, [Pid]);
% all the others start the previous one as soon as they receive
% the start message and finish their benchmark
spawn_loop(Self, N, [Next|TL]) ->
    Pid = spawn(fun() ->
        make_call_to_be_detected(),
        receive start -> ok end,
        Self ! {time, benchmark()},
        Next ! start,
        receive stop -> ok end
    end),
    spawn_loop(Self, N-1, [Pid,Next|TL]).

receive_and_calc_mean(NumOfProcs) ->
    receive_and_calc_mean(NumOfProcs,0) / NumOfProcs.
receive_and_calc_mean(0, Acc) -> Acc;
receive_and_calc_mean(NumOfProcs, Acc) ->
    receive
        {time, T} -> T
    end,
    receive_and_calc_mean(NumOfProcs-1, Acc+T).

% calls the dummy function ?NUM_OF_CALLS times and
% returns the average execution time
benchmark() ->
    benchmark(?NUM_OF_CALLS, 0)/?NUM_OF_CALLS.

benchmark(0, Acc) -> Acc;
benchmark(NumOfCalls, Acc) ->
    {T, _} = timer:tc(fun() -> dummy() end),
    benchmark(NumOfCalls-1, Acc+T).

get_benchmark_patterns() ->
    [{?MODULE, dummy, 0}].

make_call_to_be_detected() ->
    dummy().

dummy() -> ok.

spawn_discoverer(Patterns, MSType) ->
    {ok, Pid} = performerl_process_discoverer_agent:start_link(
        #{patterns => Patterns, ms_type => MSType}
    ).

stop_discoverer() ->
    ok = performerl_process_discoverer_agent:stop(),
    % small sleep to wait for the discoverer to stop
    timer:sleep(100).
