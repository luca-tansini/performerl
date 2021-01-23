-module(performerl).

-include_lib("../include/performerl.hrl").

%% API exports
-export([main/1, test/2]).

-define(TRACER_MOD, performerl_tracer_agent).
-define(META_TRACER_MOD, performerl_custom_meta_tracer).
-define(META_TRACER_SO, "performerl_custom_meta_tracer.so").
-define(DISCOVERER_MOD, performerl_process_discoverer_agent).
-define(BENCHMARK_MOD, performerl_tracing_impact_benchmark).
-define(METRICS_MOD, performerl_metrics_collector_agent).

-ifdef(TEST).
    -compile([export_all]).
-endif.

%%====================================================================
%% rebar3 plugin callbacks
%%====================================================================

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = performerl_prv:init(State),
    {ok, State1}.


%%====================================================================
%% escript main
%%====================================================================

main(Args) ->
    case 1 =< length(Args) of 
        true -> ok;
        false -> usage()
    end,
    LoadModulePath = lists:nth(1,Args),
    Opts = get_opts(Args),
    test(LoadModulePath, Opts).

%%====================================================================
%% Test functions
%%====================================================================

test(LoadModulePath, Opts0) ->
    % locally loads the binary for the load generator module
    {ok,LoadModule,Bin} = compile:file(LoadModulePath,[binary]),
    code:load_binary(LoadModule, LoadModulePath, Bin),

    {SizeLabel, Sizes} = LoadModule:get_test_sizes(),
    TestName = LoadModule:get_test_name(),

    io:format("~nSTARTING TEST: ~p with sizes ~p [~p] and Opts:~p~n",
              [TestName, Sizes, SizeLabel, Opts0]),

    CustomAgentsMods = compile_custom_agents(maps:get(custom_agents, Opts0)),
    Opts = maps:put(custom_agents_mods, CustomAgentsMods, Opts0),
    
    % setup the test environment (config common to all tests)
    {ok, TestInfo} = LoadModule:test_setup(),

    % execute a run for each test size
    RawResults = try
        [{Size, run(LoadModule, Size, Opts)} || Size <- Sizes]
    catch
        _:R ->
            io:format("run failed with reason: ~p\n", [R]),
            io:format("Stacktrace:~n~p~n", [erlang:get_stacktrace()]),
            io:format("aborting test...\n"),
            LoadModule:test_teardown(TestInfo),
            erlang:halt()
    end,

    % teardown test environment
    ok = LoadModule:test_teardown(TestInfo),

    % Reorganize collected data
    ResultsByNode = performerl_results_transformer:from_raw(RawResults),

    % OUTPUTS THE DATA
    io:format("~nPRODUCING OUTPUT~n"),
    Timestamp = performerl_lib:get_timestamp(),
    file:make_dir("performerl_results"),
    ResultsFolder = "performerl_results/"++TestName++"@"++
                     performerl_lib:format_date(Timestamp)++"/",
    file:make_dir(ResultsFolder),
    TestResults = #test_results{
        test_name = TestName,
        timestamp = Timestamp,
        test_sizes = Sizes,
        size_label = SizeLabel,
        results_by_node = ResultsByNode},    
    ok = file:write_file(ResultsFolder++"/results.raw",
                         term_to_binary(TestResults)),
    {ok, HTML} = performerl_html_test_results:generate(TestResults),
    ok = performerl_html_test_results:write_results(ResultsFolder, HTML),
    ok.

% runs the given test instance
-spec run(module(), performerl_results_ransformer:test_size(), map()) ->
            failed |
            list({
                % for each node
                NodeName :: node(),
                {
                    CallTime :: list({mfa(), count(), time()}),
                    Discovered :: list({pid(), proc_name()}),
                    MetrcisHist :: list({
                        timestamp(),
                        list({pid(), list({metric_name(), value()})})
                    })
                }
            }).
run(LoadModule, TestSize, Opts) ->

    io:format("~nSTARTING RUN: with size ~p~n", [TestSize]),
    % starts the test node(s) and sets up the environment
    {run_started, TestNodes} = LoadModule:setup_run(TestSize),
    performerl_lib:sleep_progress(500),

    CustomAgentsMods =  maps:get(custom_agents_mods, Opts),

    try
        % INJECT THE AGENTS INTO THE TEST NODES
        io:format("~nINJECTING AGENTS~n"),
        inject_agents(TestNodes),
        inject_custom_agents(TestNodes, CustomAgentsMods),

        % START THE AGENTS
        io:format("~nSTARTING AGENTS~n"),
        start_agents(TestNodes, LoadModule),
        start_custom_agents(TestNodes, CustomAgentsMods, LoadModule, TestSize),
        performerl_lib:sleep_progress(1000),

        % START LOAD
        io:format("~nSTARTING LOAD with size: ~p~n",[TestSize]),
        {LoadStartTime,{load_started,StartLoadInfo}} = timer:tc(fun() -> 
            LoadModule:start_load(TestNodes,TestSize)
        end),

        % WAIT FOR THE LOAD TO FINISH
        SleepTime = LoadModule:get_load_duration() - (LoadStartTime div 1000),
        case SleepTime =< 50 of
            true -> io:format("start_load took very long");
            false ->
                io:format("~nWAITING FOR LOAD TO FINISH~n"),
                performerl_lib:sleep_progress(SleepTime)
        end,

        % STOP LOAD
        io:format("~nSTOP LOAD~n"),
        {load_stopped,StopLoadInfo} = LoadModule:stop_load(StartLoadInfo),

        io:format("~nSTOPPING AGENTS and COLLECTING DATA~n"),
        _CustomAgentsResultsByNode =
            stop_custom_agents(TestNodes, CustomAgentsMods),

        {DiscoveredByNode, MaxActiveByNode,
         ProcessMetricsByNode, CallTimeByNode} = stop_agents(TestNodes),

        % RUN IMPACT BENCHMARK and COMPENSATE CALL TIME DATA
        CompCallTimeByNode =
            run_impact_benchmark(Opts, CallTimeByNode, MaxActiveByNode),

        % PURGE INJECTED MODULES
        purge_injected_modules(TestNodes),

        % tears down the run environment
        io:format("~nSTOPPING RUN~n"),
        run_ended = LoadModule:teardown_run(StopLoadInfo),

        ResultsByNode = join_results(CompCallTimeByNode, DiscoveredByNode, 
                                     MaxActiveByNode, ProcessMetricsByNode),

        io:format("~nFINISHING RUN~n"),
        performerl_lib:sleep_progress(3000),
        ResultsByNode

    catch Error:Reason ->
        io:format("~nrun failed with reason: ~p~n", [{Error,Reason}]),
        io:format("Stacktrace:~n~p~n", [erlang:get_stacktrace()]),
        io:format("Fallback: trying to stop test nodes...~n"),
        lists:foreach(fun(TestNode) ->
                rpc:call(TestNode, init, stop, [])
            end,
            TestNodes),
        throw(failed)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

usage() ->
    io:format("usage: performerl load_gen [opts]~n"),
    halt(1).

compensate_per_process(PerProcess, TotalImpact) ->
    lists:map(fun({Pid, Count, Time}) ->
            {Pid, Count, round0(Time - Count*TotalImpact)}
        end, PerProcess).

round0(N) ->
    case round(N) < -1000 of
        true -> io:format("DEBUG: compensating some function"++
                          " was way below 0: ~p~n", [round(N)]);
        false -> ok
    end,
    max(0, round(N)).

get_opts(Args) -> 
    Map = #{
        benchmark_comp => true,
        custom_agents => undefined
    },
    get_opts(Args, Map).
get_opts([], Opts) -> Opts;
get_opts(["--no-benchmark-comp"|TL], Opts) ->
    get_opts(TL, Opts#{benchmark_comp => false});
get_opts([_|TL], Opts) -> get_opts(TL, Opts).

inject_mod(Node, ModName, ModBin) ->
    case rpc:call(Node, code, load_binary, 
             [ModName, atom_to_list(ModName)++".erl", ModBin]) of
        {module, ModName} -> ok;
        {error, Reason} -> {error, Reason}
    end.

inject_agents(TestNodes) ->
    {?TRACER_MOD, TracerBin, _} = code:get_object_code(?TRACER_MOD),
    {?DISCOVERER_MOD, DiscBin, _} = code:get_object_code(?DISCOVERER_MOD),
    {?METRICS_MOD, MetricsBin, _} = code:get_object_code(?METRICS_MOD),
    {?META_TRACER_MOD, MetaTracerBin, _} = 
        code:get_object_code(?META_TRACER_MOD),
    % get the custom_meta_tracer NIF shared object file
    Dir = filename:dirname(code:which(?MODULE))++"/../src/agents/",
    {ok, MetaTracerSoBin} = file:read_file(Dir++?META_TRACER_SO),
    lists:foreach(fun(TestNode) ->
            ok = inject_mod(TestNode, ?TRACER_MOD, TracerBin),
            ok = inject_mod(TestNode, ?DISCOVERER_MOD, DiscBin),
            rpc:call(TestNode, file, make_dir, ["performerl_tmp"]),
            ok = rpc:call(TestNode, file, write_file, 
                          ["performerl_tmp/"++?META_TRACER_SO,
                          MetaTracerSoBin]),
            ok = inject_mod(TestNode, ?META_TRACER_MOD, MetaTracerBin),
            ok = inject_mod(TestNode, ?METRICS_MOD, MetricsBin)
        end,
        TestNodes).

start_agents(TestNodes, LoadModule) ->
    % make sure there are no duplicate patterns
    % TODO: what happens with overlapping (supersets) of patterns?
    Patterns = lists:usort(LoadModule:get_trace_patterns()),
    % TODO: add metrics collection interval
    Config = #{patterns => Patterns,
               ms_type => tree},
    lists:foreach(fun(TestNode) ->
        % TODO: start each agent separately
        {ok,Pid} = rpc:call(TestNode, ?TRACER_MOD, start, [Config]),
        {tracer,Pid} = rpc:call(TestNode, erlang, trace_info, [new,tracer])
    end,TestNodes).

stop_agents(TestNodes) ->
    % STOP DISCOVERER AND COLLECTOR AGENTS AND GET THEIR DATA
    {DiscoveredByNode, MaxActiveByNode,
     ProcessMetricsByNode, CallTimeByNode} =
        lists:foldl(
            fun(TestNode, {Disc, MaxActive, Metrics, CallTime}) ->
                % stop the metrics first, because it depends on discoverer
                {ok, M} =
                    rpc:call(TestNode, ?METRICS_MOD, get_metrics_and_stop, []),
                {ok, {D,A}} =
                    rpc:call(TestNode, ?DISCOVERER_MOD,
                             get_results_and_stop, []),
                {ok, C} =
                    rpc:call(TestNode, ?TRACER_MOD, get_results_and_stop, []),
                {[{TestNode, D}| Disc],
                 [{TestNode, A}| MaxActive],
                 [{TestNode, M}| Metrics],
                 [{TestNode, C}| CallTime]}
            end,
            {[],[],[], []},
            TestNodes),
    {DiscoveredByNode, MaxActiveByNode, ProcessMetricsByNode, CallTimeByNode}.

%*******************************************************************************
% custom agents functions
%*******************************************************************************

% this function takes the comma separated list of custom agents modules path
% and compiles them into binaries. Returns {ModName, Bin}
compile_custom_agents(undefined) -> [];
compile_custom_agents(CustomAgentsPath) ->
    Paths = string:split(CustomAgentsPath, ","),
    lists:map(
        fun(Path) ->
            {ok,ModName,Bin} = compile:file(Path,[binary]),
            {module, ModName} = code:load_binary(ModName, "", Bin),
            {ModName, Bin}
        end,
        Paths
    ).

inject_custom_agents(TestNodes, CustomAgentsMods) ->
    lists:foreach(
        fun(TestNode) ->
            lists:foreach(
                fun({ModName, Bin}) ->
                    ok = inject_mod(TestNode, ModName, Bin)
                end,
                CustomAgentsMods)
        end,
        TestNodes),
    ok.

% custom agents are started after the default ones, in case they depend on those
start_custom_agents(TestNodes, CustomAgentsMods, LoadModule, TestSize) ->
    lists:foreach(
        fun(TestNode) ->
            lists:foreach(
                fun({CustomAgentMod, _}) ->
                    Config = CustomAgentMod:get_config(TestNode,
                                                       LoadModule, TestSize),
                    {ok, _Pid} = rpc:call(TestNode, CustomAgentMod,
                                          start, [Config])
                end,
                CustomAgentsMods)
        end,
        TestNodes),
    ok.

stop_custom_agents(TestNodes, CustomAgentsMods) ->
    lists:map(
        fun(TestNode) ->
            CustomAgentsResults = lists:map(
                fun({CustomAgentMod, _}) ->
                    {ok, Res} = rpc:call(TestNode, CustomAgentMod,
                                          get_results_and_stop, []),
                    CustomAgentMod:process_results(TestNode, Res),
                    {CustomAgentMod, Res}
                end,
                CustomAgentsMods),
            {TestNode, CustomAgentsResults}
        end,
        TestNodes).

% Runs the tracing impact benchmark, if enabled, and
% uses the results to refine the call_time data
run_impact_benchmark(Opts, CallTimeByNode, MaxActiveByNode) ->
    case maps:get(benchmark_comp, Opts) of
        false -> CallTimeByNode;
        true ->
            {?BENCHMARK_MOD, BenchBin, _} =
                code:get_object_code(?BENCHMARK_MOD),
            io:format("~nRUNNING TRACING IMPACT BENCHMARK~n"),
            lists:map(fun({TestNode, CallTime}) ->

                ok = inject_mod(TestNode, ?BENCHMARK_MOD, BenchBin),

                MaxActive = proplists:get_value(TestNode, MaxActiveByNode),

                % RUN THE IMPACT BENCHMARK
                Impact = rpc:call(TestNode, ?BENCHMARK_MOD,
                                  benchmark, [MaxActive]),

                % REFINE CALL TIME DATA
                Comp =  lists:map(fun({MFA, Count, Time, PerProcess}) -> 
                            {MFA, Count, round0(Time - Count*Impact),
                            compensate_per_process(PerProcess, Impact)}
                        end, CallTime),
                {TestNode, Comp}
            end,
            CallTimeByNode)
    end.

% TODO: also purge custom agents
purge_injected_modules(TestNodes) ->
    Injected = [?DISCOVERER_MOD, ?TRACER_MOD, ?BENCHMARK_MOD,
                ?METRICS_MOD, ?META_TRACER_MOD],
    % purge agents and other injected modules
    lists:foreach(fun(TestNode) ->
            lists:foreach(fun(Mod) ->
                    rpc:call(TestNode, code, purge, [Mod]),
                    rpc:call(TestNode, code, delete, [Mod]),
                    rpc:call(TestNode, code, purge, [Mod])
                end, Injected),
            % delete dynamic library file
            ok = rpc:call(TestNode, file, delete, 
                          ["performerl_tmp/"++?META_TRACER_SO]),
            ok = rpc:call(TestNode, file, del_dir, ["performerl_tmp"])
        end, TestNodes).

-spec join_results(
        CallTimeByNode       :: list({node(), list({mfa(), count(), time()})}),
        DiscoveredByNode     :: list({node(), list({pid(), proc_name()})}),
        MaxActiveByNode      :: list({node(), non_neg_integer()}),
        ProcessMetricsByNode :: list({node(),
                                      list({
                                        timestamp(),
                                        list({pid(),
                                              list({metric_name(),
                                                    value()})
                                            })
                                      })
                                })
    ) -> list({
                % for each node
                NodeName :: node(),
                {
                    CallTime :: list({mfa(), count(), time()}),
                    Discovered :: list({pid(), proc_name()}),
                    MaxActive  :: non_neg_integer(),
                    MetrcisHist :: list({
                        timestamp(),
                        list({pid(), list({metric_name(), value()})})
                    })
                }
            }).
join_results(CallTimeByNode, DiscoveredByNode,
             MaxActiveByNode, ProcessMetricsByNode) ->
    lists:map(
        fun({Node,CallTime}) ->
            Discovered = proplists:get_value(Node, DiscoveredByNode),
            Metrics = proplists:get_value(Node, ProcessMetricsByNode),
            MaxActive = proplists:get_value(Node, MaxActiveByNode),
            {Node,{CallTime,Discovered,MaxActive,Metrics}}
        end, CallTimeByNode).
