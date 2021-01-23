-module(tracing_overhead_test).

-include_lib("eunit/include/eunit.hrl").

-define(NUM_OF_CALLS, 4000).
-define(ARITY_NUM_OF_CALLS, 100000).
-define(ACTUALLY_BENCHD_PROCS, 1024).
-define(TRACE_MFA, {?MODULE, dummy, 2}).

tracing_overhead_test_() ->
    {
        timeout,
        6000,
        fun() ->
            ?debugFmt("\ntracing_overhead_test, this will take a while...\n",[]),
            L = lists:map(fun(N) ->
                        {N,tracing_overhead(N)}
                    end,
                    [round(math:pow(2,N)) || N <- lists:seq(10,17)]),
            ?debugFmt("\n~p\n", [L]),
            print_chart_series(L)
        end
    }.

arity_flag_overhead_comparison_test_() ->
    {
        timeout,
        6000,
        fun() ->
            L = lists:map(fun(N) ->
                        {N,arity_flag_overhead_comparison(N)}
                    end,
                    [round(math:pow(2,N)) || N <- lists:seq(0,14)]),
            ?debugFmt("\n~p\n", [L]),
            print_chart_series(L)
        end
    }.

%*******************************************************************************
% tracing overhead functions
% this it the test that compares the overhead of differente tracing techniques
% with an increasing number of processes
%*******************************************************************************

% functions called once for each test size
tracing_overhead(NumOfProcs) ->

    % no tracing, reference time
    TRef = benchmark(NumOfProcs, none),
    % call_time tracing only overhead
    TCallTime = benchmark(NumOfProcs, call_time),
    % meta-tracing with MSTrue, always sending a message
    TMSTrue = benchmark(NumOfProcs, true),
    % custom meta-tracing with MSTrue, always sending a message (with arity)
    % TMSCustomTrue = benchmark(NumOfProcs, custom_true),
    % meta-tracing with MSFalse, never sends a message
    % TMSFalse = benchmark(NumOfProcs, false),
    % custom meta-tracing with MSTree (not sending messages)
    TMSTree = benchmark(NumOfProcs, tree),
    % custom meta-tracing with MSTree (sending messages)
    TMSTreeMsg = benchmark(NumOfProcs, tree_msg),
    [
        {none, TRef}
        ,{call_time, TCallTime - TRef}
        ,{true, TMSTrue - TRef}
        % ,{custom_true, TMSCustomTrue - TRef}
        % ,{false, TMSFalse - TRef}
        ,{tree, TMSTree - TRef}
        ,{tree_msg, TMSTreeMsg}
    ].

% function called once for each test size and tracing configuration
benchmark(NumOfProcs, TracingType) ->
    ?debugFmt("~nrunning {~p, ~p}\n", [NumOfProcs, TracingType]),
    OneIn = NumOfProcs div ?ACTUALLY_BENCHD_PROCS,
    [First | _]= Pids = spawn_loop(self(), OneIn, NumOfProcs, []),
    setup_tracing(TracingType, Pids),
    First ! start,
    Res = receive_and_calc_mean(?ACTUALLY_BENCHD_PROCS),
    lists:foreach(fun(Pid) -> Pid ! stop end, Pids),
    teardown_tracing(TracingType),
    % wait for the processes to die
    timer:sleep(1000),
    Res.

% function to spawn the required number of processes
% only ?ACTUALLY_BENCHD_PROCS out of NumOfProcesses will actually be
% measured, the others only take up PIDs. We have to do this otherwise
% it would take ages to test millions of processes
spawn_loop(_Self, _OneIn, 0, Pids) -> Pids;
% the first one to spawn will be the last one to receive
% the start message and doesn't send any start message
spawn_loop(Self, OneIn, N, []) ->
    Pid = spawn(fun() ->
        receive start -> ok end,
        case (N-1) rem OneIn of
            0 -> Self ! {time, benchmark_work()};
            _ -> ok
        end,
        receive stop -> ok end
    end),
    spawn_loop(Self, OneIn, N-1, [Pid]);
% all the others start the previous one as soon as they receive
% the start message and finish their benchmark
spawn_loop(Self, OneIn, N, [Next|TL]) ->
    Pid = spawn(fun() ->
        receive start -> ok end,
        case (N-1) rem OneIn of
            0 -> Self ! {time, benchmark_work()};
            _ -> ok
        end,
        Next ! start,
        receive stop -> ok end
    end),
    spawn_loop(Self, OneIn, N-1, [Pid,Next|TL]).

% function to retrieve the measurements done by the processes
% and get the average
receive_and_calc_mean(NumOfProcs) ->
    receive_and_calc_mean(NumOfProcs,0) / NumOfProcs.
receive_and_calc_mean(0, Acc) -> Acc;
receive_and_calc_mean(NumOfProcs, Acc) ->
    receive
        {time, T} -> T
    end,
    receive_and_calc_mean(NumOfProcs-1, Acc+T).

benchmark_work() ->
    benchmark_work(?NUM_OF_CALLS, 0)/?NUM_OF_CALLS.

benchmark_work(0, Acc) -> Acc;
benchmark_work(NumOfCalls, Acc) ->
    % stop for a while every 1000 calls to give the tracer time
    % to flush the message queue and reset reduction count
    erlang:yield(),
    {T, _} = timer:tc(fun() ->
        dummy([],1000)
    end),
    benchmark_work(NumOfCalls-1000, Acc+T).

%*******************************************************************************
% arity_flag_overhead_comparison functions
% this it the test that compares the overhead of tracing with the standard
% back-end (that sends a message with the explicit list of arguments) with our
% custom meta tracer module (that implements the arity flag), with an increasing
% size for the arguments of the dummy function
%*******************************************************************************

arity_flag_overhead_comparison(ArgumentSize) ->
    % reference
    TRef = arity_benchmark(ArgumentSize),

    % regular meta-tracing sending a message with full arguments list
    setup_tracing(true, []),
    TTrue = arity_benchmark(ArgumentSize),
    teardown_tracing(true),
    timer:sleep(100),

    % custom meta-tracing sending a message with arguments arity
    setup_tracing(custom_true, []),
    TArity = arity_benchmark(ArgumentSize),
    teardown_tracing(custom_true),

    [
        {arguments_list, TTrue - TRef},
        {custom_arity, TArity - TRef}
    ].

arity_benchmark(ArgumentSize) ->
    L = [rand:uniform(100) || _ <- lists:seq(1,ArgumentSize)],
    arity_benchmark_work(L, ?ARITY_NUM_OF_CALLS, 0)/?ARITY_NUM_OF_CALLS.

arity_benchmark_work(_L, 0, Acc) -> Acc;
arity_benchmark_work(L, NumOfCalls, Acc) ->
    % stop for a while every 1000 calls to give the tracer time
    % to flush the message queue and reset reduction count
    timer:sleep(10),
    {T, _} = timer:tc(fun() ->
        dummy(L,1000)
    end),
    arity_benchmark_work(L, NumOfCalls-1000, T+Acc).

%*******************************************************************************
% Common functions to setup the tracing infrastructure with different configs
%*******************************************************************************

% dummy function to trace
dummy(_L,0) -> ok;
dummy(L,N) -> dummy(L,N-1).

setup_tracing(none, _) -> ok;
setup_tracing(call_time, _) ->
    Pid = spawn(fun tracer_loop/0),
    register(tracer, Pid),
    erlang:trace(processes, true, [call, {tracer, Pid}]),
    erlang:trace_pattern(?TRACE_MFA, true, [call_time]);
setup_tracing(true, _) ->
    Pid = spawn(fun tracer_loop/0),
    register(tracer, Pid),
    erlang:trace_pattern(?TRACE_MFA, true, [{meta, Pid}]);
setup_tracing(custom_true, _) ->
    Pid = spawn(fun tracer_loop/0),
    register(tracer, Pid),
    % still sends a message for every call, but uses our own
    % performerl_custom_meta_tracer, which sends the arity instead
    % of the actual arguments (should be faster)
    erlang:trace_pattern(?TRACE_MFA, true,
                         [{meta, performerl_custom_meta_tracer, Pid}]);
setup_tracing(false, _) ->
    Pid = spawn_link(fun() -> 
        receive 
            stop -> ok; 
            _ -> throw(unwanted_message) 
        end end),
    register(tracer, Pid),
    % MS that is always false
    MS = [{'_', [{'==',{const, 42},{const, 24}}], []}],
    erlang:trace_pattern(?TRACE_MFA, MS, [{meta, Pid}]);
setup_tracing(tree, Pids) ->
    Pid = spawn_link(fun() -> 
        receive 
            stop -> ok; 
            _ -> throw(unwanted_message) 
        end end),
    register(tracer, Pid),
    % since we build this MS with all the processes already in it
    % (as if they were already discovered) it is always false.
    % uses our performerl_custom_meta_tracer, but never sends any message
    MS = performerl_process_discoverer_agent:create_pid_ms(Pids, tree),
    erlang:trace_pattern(?TRACE_MFA, MS,
                         [{meta, performerl_custom_meta_tracer, Pid}]);
setup_tracing(tree_msg, Pids) ->
    Pid = spawn(fun tracer_loop/0),
    register(tracer, Pid),
    % this MS should cost both the time to execute the tree MS and
    % the cost of sending the msg
    MS = performerl_process_discoverer_agent:create_pid_ms(Pids, tree),
    % trick: to have the processes look for their PID in the tree and then send
    % the message without having to edit the MS we negate its result (by
    % removing the already present negation)
    [{MS1,[{'not', MS2}], MS3}] = MS,
    NewMS = [{MS1, [MS2], MS3}],
    erlang:trace_pattern(?TRACE_MFA, NewMS,
                         [{meta, performerl_custom_meta_tracer, Pid}]).


% we need to discard trace messages otherwise we run out of memory
tracer_loop() ->
    receive
        stop -> ok;
        _ -> tracer_loop()
    end.

teardown_tracing(none) -> ok;
teardown_tracing(call_time) ->
    tracer ! stop,
    erlang:trace_pattern(?TRACE_MFA, false, [call_time]),
    erlang:trace(processes, false, [call]),
    {flags, []} = erlang:trace_info(new_processes, flags),
    {tracer, []} = erlang:trace_info(new_processes, tracer),
    {all, false} = erlang:trace_info(?TRACE_MFA, all);
teardown_tracing(_) ->
    tracer ! stop,
    erlang:trace_pattern(?TRACE_MFA, false, [meta]),
    {all, false} = erlang:trace_info(?TRACE_MFA, all).

%*******************************************************************************
% Functions to print the results series in a HighCharts JS fashion
%*******************************************************************************

print_chart_series(L) ->
    print_chart_series(L, [], maps:new()).
print_chart_series([], Categories, Map) ->
    ?debugFmt("~ncategories: ~p~n", [Categories]),
    [ ?debugFmt("~n{name: \'~p\',\n data: ~p}~n",
                [K, maps:get(K, Map)]) ||
      K <- maps:keys(Map)];
print_chart_series([{Cat, Data} | TL], Categories, Map) ->
    NewMap = lists:foldl(
        fun({K, V}, D) ->
            maps:update_with(K,fun(L) -> L++[V] end, [V], D)
        end,
        Map,
        Data
    ),
    print_chart_series(TL, Categories++[Cat], NewMap).

%*******************************************************************************
% Some (currently unused) statistical analysys functions
%*******************************************************************************

% analyse(Values=[V|_]) ->
%     {Len, Min, Max, Avg, Stdev} = analyse0(Values, 0, V, V, 0, 0),
%     ?debugFmt("
%         Len: ~p
%         Min: ~p
%         Max: ~p
%         Avg: ~p
%         Dev: ~p\n",
%         [Len, Min, Max, Avg, Stdev]),
%     Avg.

% analyse0([], Len, Min, Max, Sum, SqSum) ->
%     Stdev = 1/Len * math:sqrt(Len*SqSum - Sum*Sum),
%     {Len, Min, Max, Sum/Len, Stdev};
% analyse0([H|TL], Len, Min, Max, Sum, SqSum) ->
%     NewMin = min(H,Min),
%     NewMax = max(H,Max),
%     analyse0(TL, Len+1, NewMin, NewMax, Sum+H, SqSum+H*H).

% stdev(Values, Len, Avg) ->
%     math:sqrt(stdev0(Values, Avg, 0)/Len).

% stdev0([], _, SqDiffSum) -> SqDiffSum;
% stdev0([H|TL], Avg, SqDiffSum) ->
%     Sq = (H - Avg) * (H - Avg),
%     stdev0(TL, Avg, SqDiffSum+Sq).