-module(performerl_html_test_results_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../include/performerl.hrl").

get_dummy_node_results() ->
    CallTimeResSize1 = [
        {{fake,name,1}, {'_', 3, '_'}},
        {{fake,name,2}, {'_', 4, '_'}},
        {{fake,name,3}, {'_', 3, '_'}},
        {{fake,name,4}, {'_', 5, '_'}},
        {{fake,name,5}, {'_', 1, '_'}}],
    CallTimeResSize2 = [
        {{fake,name,1}, {'_', 3, '_'}},
        {{fake,name,3}, {'_', 1, '_'}},
        {{fake,name,4}, {'_', 2, '_'}},
        {{fake,name,5}, {'_', 5, '_'}}],
    CallTimeResSize3 = [
        {{fake,name,1}, {'_', 10, '_'}},
        {{fake,name,2}, {'_', 11, '_'}},
        {{fake,name,3}, {'_', 8, '_'}},
        {{fake,name,4}, {'_', 9, '_'}}],
    
    [
        {size1, #node_run_data{
            funs_call_time = maps:from_list(CallTimeResSize1),
            funs_by_call_time = [{fake,name,4},{fake,name,2},{fake,name,1},
                                 {fake,name,3},{fake,name,5}],
            procs_by_metrics = [
                {reductions, [
                    {proc_name, 3},
                    {cool_name, 1}
                ]},
                {memory, [
                    {cool_name, 3},
                    {proc_name, 1}
                ]}
            ]
        }},
        {size2, #node_run_data{
            funs_call_time = maps:from_list(CallTimeResSize2),
            funs_by_call_time = [{fake,name,5},{fake,name,1},
                                 {fake,name,4},{fake,name,3}],
            procs_by_metrics = [
                {reductions, [
                    {proc_name, 3},
                    {{worker,func,1}, 2},
                    {cool_name, 1}
                ]},
                {memory, [
                    {cool_name, 3},
                    {{worker,func,1}, 2},
                    {proc_name, 1}
                ]}
            ]
        }},
        {size3, #node_run_data{
            funs_call_time = maps:from_list(CallTimeResSize3),
            funs_by_call_time = [{fake,name,2},{fake,name,1},
                                 {fake,name,4},{fake,name,3}],
            procs_by_metrics = [
                {reductions, [
                    {{worker,func,1}, 2},
                    {cool_name, 1}
                ]},
                {memory, [
                    {cool_name, 3},
                    {{worker,func,1}, 2}
                ]}
            ]
        }}
    ].

get_topN_call_time_series_test() ->
    NodeResults = get_dummy_node_results(),
    Expected = [{{fake,name,1}, [3,3,10]},
                {{fake,name,2}, [4,null,11]},
                {{fake,name,4}, [5,2,9]},
                {{fake,name,5}, [1,5,null]}],
    Actual =
        performerl_html_front_page:get_topN_call_time_series(3, NodeResults),
    ?assertEqual(Expected, Actual).

generate_series_test() ->
    FakeSeries = [{{fake,name,1}, [3,3,10]},
                      {{fake,name,2}, [4,null,11]},
                      {{fake,name,4}, [5,2,9]},
                      {{fake,name,5}, [1,5,null]}],
    Expected = 
"series: [
    {name: 'fake:name:1', data: [3,3,10]},
    {name: 'fake:name:2', data: [4,null,11]},
    {name: 'fake:name:4', data: [5,2,9]},
    {name: 'fake:name:5', data: [1,5,null]},
]",
    Actual = performerl_html_test_results:generate_series(FakeSeries),
    ?assertEqual(Expected, Actual).

get_topN_peak_memory_series_test() ->
    NodeResults = get_dummy_node_results(),
    Expected = [{cool_name, [3,3,3]},
                {proc_name, [1,1,null]},
                {{worker,func,1}, [null,2,2]}],
    Actual =
        performerl_html_front_page:get_topN_peak_metric_series(3, memory,
                                                              NodeResults),
    ?assertEqual(Expected, Actual).

get_topN_peak_reduction_series_test() ->
    NodeResults = get_dummy_node_results(),
    Expected = [{cool_name, [1,1,1]},
                {proc_name, [3,3,null]},
                {{worker,func,1}, [null,2,2]}],
    Actual =
        performerl_html_front_page:get_topN_peak_metric_series(3, reductions,
                                                              NodeResults),
    ?assertEqual(Expected, Actual).
