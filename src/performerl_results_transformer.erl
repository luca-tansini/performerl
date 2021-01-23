-module(performerl_results_transformer).

-include_lib("../include/performerl.hrl").

-export([from_raw/1]).

-ifdef(TEST).
    -compile([export_all]).
-endif.

-spec from_raw(raw_results()) -> results_by_node().
from_raw(ResultsByTestSize) ->
    TestSizes = lists:sort(proplists:get_keys(ResultsByTestSize)),
    TestNodes = lists:usort(
                    lists:flatten([proplists:get_keys(SizeResults) ||
                                   {_Size, SizeResults} <- ResultsByTestSize])),
    lists:map(fun(TestNode) ->
        TestNodeRes = lists:map(fun(TestSize) ->
                TestSizeResults = proplists:get_value(TestSize,
                                                      ResultsByTestSize),
                case proplists:get_value(TestNode ,TestSizeResults) of
                    undefined ->
                        % it can be that a TestNode doesn't exist for
                        % a specific test size, then we return no_data
                        {TestSize, no_data};
                    {CallTime, Discovered, MaxActive, MetricsHist} ->
                        % first thing we need is the processes Pid -> Name map
                        {PidNameMap, NamePidMap} = 
                            get_name_pid_maps(Discovered),
                        {FunsCallTime, ProcUsedFuns} =
                            get_funs_call_time(PidNameMap, CallTime),
                        FunsByCallTime =
                            sort_funs_by_call_time(FunsCallTime),
                        % we still need this temp value for the following
                        % step, but not for the final results
                        TmpProcsMetricsHist =
                            replace_pid_with_name(MetricsHist, PidNameMap),
                        ProcsByMetrics =
                            sort_procs_by_metrics(TmpProcsMetricsHist),
                        ConvMetricsHist =
                            convert_pids_to_local(MetricsHist),
                        NodeRunData = #node_run_data{
                            funs_call_time = FunsCallTime,
                            funs_by_call_time = FunsByCallTime,
                            procs_name_pid = NamePidMap,
                            procs_used_funs = ProcUsedFuns,
                            procs_by_metrics = ProcsByMetrics,
                            procs_metric_hist = ConvMetricsHist,
                            max_active = MaxActive
                        },
                        {TestSize, NodeRunData}
                end
            end, TestSizes),
        {TestNode, TestNodeRes}
        end, TestNodes).


% From the Discovered list get the Pid -> Name map and
% the Name -> Pid map.
% In the second one, all processes with the same name are aggregated.
-spec get_name_pid_maps(list({pid(), proc_name() |
                                    {inactive, proc_name()}})) ->
    {maps:map(pid(), proc_name()), maps:map({proc_name(), [pid()]})}.

get_name_pid_maps(Discovered) ->
    lists:foldl(fun({Pid, Name}, {PidNameMap, NamePidMap}) ->
            CPid = convert_pid(Pid),
            FinalName = case Name of
                {inactive, N} -> N;
                N -> N
            end,
            {maps:put(CPid, FinalName, PidNameMap),
             maps:update_with(FinalName, fun(Acc) -> [CPid|Acc] end,
                         [CPid], NamePidMap)}
        end, {maps:new(), maps:new()}, Discovered).


% From the Pid -> Name map and the CallTime proplist, return the
% FunsCallTime (simply update pids with proc_names and aggregate same names)
% and ProcUsedFuns, which is, for each process name, the list of used functions 
-spec get_funs_call_time(
    PidNameMap :: maps:map(pid(), proc_name()),
    CallTime :: list({mfa(), count(), time()})
) -> {
    FunsCallTime :: maps:map(mfa(), {count(), time(), per_proc_info()}),
    ProcUsedFuns :: maps:map(proc_name(), list({mfa(), count(), time()}))
}.

get_funs_call_time(PidNameMap, CallTime) ->
    lists:foldl(fun({MFA, Count, Time, PPInfo}, {FunsCallTime, ProcUsedFun}) ->
            % first part: convert pids to names and aggregate same names
            TmpMap = lists:foldl(fun({Pid, C, T}, AccMap) ->
                    CPid = convert_pid(Pid),
                    Name = maps:get(CPid, PidNameMap),
                    maps:update_with(Name, fun({OldC, OldT}) ->
                            {OldC + C, OldT + T}
                        end, {C, T}, AccMap)
                end, maps:new(), PPInfo),
            % go from map to list({Name, C, T}) and sort it on T
            ConvertedPPInfo =
                    lists:sort(fun({_,_,T1}, {_,_,T2}) ->
                            T1 >= T2
                        end, lists:map(fun({Name, {C, T}}) -> {Name, C, T} end,
                                       maps:to_list(TmpMap))),
            % second part: aggregate used functions 
            NewUsedFunMap = lists:foldl(fun({Pid, C, T}, AccMap) ->
                    CPid = convert_pid(Pid),
                    Name = maps:get(CPid, PidNameMap),
                    maps:update_with(Name, fun(ListOfUsedFuns) ->
                            % if the ListOfUsedFuns starts with {MFA, _, _}
                            % we have to sum it up, otherwise just add it
                            case ListOfUsedFuns of
                                [{MFA, PPC, PPT} | TL] ->
                                    [{MFA, PPC+C, PPT+T} | TL];
                                L ->
                                    [{MFA, C, T} | L]
                            end
                        end, [{MFA, C, T}], AccMap)
                end, ProcUsedFun, PPInfo),
            {maps:put(MFA, {Count, Time, ConvertedPPInfo}, FunsCallTime),
             NewUsedFunMap}
        end, {maps:new(), maps:new()}, CallTime).


% Sorts the functions by call time in decreasing order.
% Returns the list of sorted MFAs.
-spec sort_funs_by_call_time(
    FunsCallTime :: maps:map(mfa(), {count(), time(), per_proc_info()})
) ->
    sorted_list(mfa()).

sort_funs_by_call_time(FunsCallTime) ->
    Sorted = lists:sort(
        fun({_MFA1, {_C1, Time1, _PPInfo1}}, {_MFA2, {_C2, Time2, _PPInfo2}}) ->
            Time1 >= Time2
        end,
        maps:to_list(FunsCallTime)
    ),
    lists:map(fun({MFA, _}) -> MFA end, Sorted).


% From the history of metrics and the Pid -> Name map,
% return the history of metrics with proc_names instead of pids and aggregate
% all processes with the same name by summing up their metrics
-spec replace_pid_with_name(
    MetricsHist :: list({
                    timestamp(),
                    list({pid(), list({metric_name(), value()})})
    }),
    PidNameMap :: maps:map(pid(), proc_name())
) -> list({timestamp(),
           list({proc_name(),
                 list({metric_name(), value()})
           })
     }).

replace_pid_with_name(MetricsHist, PidNameMap) ->
    lists:map(
        fun({Timestamp, L}) ->
            {Timestamp,
             lists:reverse(lists:foldl(
                % in case of processes with the same name we take the sum
                fun({Pid, Metrics}, Acc) ->
                    CPid = convert_pid(Pid),
                    Name = maps:get(CPid, PidNameMap),
                    case lists:keytake(Name, 1, Acc) of
                        false -> [{Name, Metrics} | Acc];
                        {value, {Name, Metrics1}, Rest} ->
                            M = lists:map(
                                fun({MetricName, Value}) ->
                                    {MetricName,
                                     % it can't happen that we have different
                                     % metric names, they must be all there
                                     Value + proplists:get_value(MetricName,
                                                                 Metrics1)}
                                end,
                                Metrics
                            ),
                            [{Name, M} | Rest]
                    end
                end,
                [],
                L
            ))}
        end,
        MetricsHist
    ).


% From the ProcsMetricsHist returns, for each metric name, a sorted list of
% all processes and their peak value
-spec sort_procs_by_metrics(
    ProcsMetricsHist :: list({timestamp(),
           list({proc_name(),
                 list({metric_name(), value()})
           })
    })
) ->
    list({metric_name(),
        sorted_list({proc_name(), value()})
    }).

sort_procs_by_metrics(ProcsMetricsHist) ->
    % TODO: this will fail on empty list
    % going trough each timestamp, build a map that, for each process,
    % has a list of peak values for each metric
    PeakMap = lists:foldl(
        % for each timestamp
        fun({_Ts, Procs}, AccMap) ->
            % for each process in this timestamp
            lists:foldl(
                fun({ProcName, Metrics}, AccMap2) ->
                    maps:update_with(
                        ProcName,
                        fun(OldMetrics) ->
                            lists:map(
                                fun({MetricName, OldValue}) ->
                                    % TODO: are we always going to want to
                                    % take the max for any metric?
                                    {MetricName,
                                     max(OldValue,
                                         proplists:get_value(MetricName,
                                                             Metrics))}
                                end,
                                OldMetrics
                            )
                        end,
                        Metrics,
                        AccMap2
                    )
                end,
                AccMap,
                Procs
            )
        end,
        maps:new(),
        ProcsMetricsHist
    ),
    {_,Metrics} = erlang:hd(maps:to_list(PeakMap)),
    MetricNames = proplists:get_keys(Metrics),
    lists:map(
        fun(MetricName) ->
            L = lists:map(
                fun({ProcName, PeakValues}) ->
                    {ProcName, proplists:get_value(MetricName, PeakValues)}
                end,
                maps:to_list(PeakMap)
            ),
            {
                MetricName,
                lists:sort(fun({_, V1}, {_, V2}) -> V1 >= V2 end, L)
            }
        end,
        MetricNames
    ).


% takes the original metrics hist and simply converts
% the remote pids to local ones
convert_pids_to_local(MetricsHist) ->
    lists:map(fun({Ts, MetricsByPid}) ->
            {Ts, [{convert_pid(Pid),Metrics} || {Pid,Metrics} <- MetricsByPid]}
        end,
        MetricsHist
    ).


convert_pid(Pid) ->
    try 
        S = lists:flatten(io_lib:format("~p",[Pid])),
        re:replace(S, "<\\d*\\.", "<0.", [{return,list}])
    catch
        _:_ ->
            io:format(lists:flatten(
                io_lib:format("pid: ~p could not be converted!~n", [Pid]))),
            Pid
    end.