-module(performerl_html_front_page).

-include_lib("../include/performerl.hrl").

-export([generate/1]).

-ifdef(TEST).
    -compile([export_all]).
-endif.

generate(#test_results{test_name=TestName, timestamp=Timestamp,
                         test_sizes=TestSizes, size_label=SizeLabel,
                         results_by_node=Results=[First | Others]}) ->
    TestNodes = [TestNode || {TestNode, _} <- Results],
    HTML =
    performerl_html_test_results:get_static_header("Test Results")++
    "<body>\n"++
    get_nav()++
    get_subpage_select(TestNodes)++
    % one hidden subpage for each node
    generate_node_subpage(TestName, TestSizes, Timestamp,
                          First, {display, "block"})++
    lists:flatten([generate_node_subpage(TestName, TestSizes, Timestamp, Result)
                   || Result <- Others])++
    "<script type=\"text/javascript\">\n"++
    % JS with all the charts
    lists:flatten([generate_graphs(TestSizes, SizeLabel, NodeResults, 4)
                   || NodeResults <- Results])++
    "</script>\n"
	"</body>\n"
    "</html>\n",
    {ok, HTML}.

get_nav() ->
"<script src=\"highcharts/highcharts.js\"></script>
<script src=\"highcharts/modules/series-label.js\"></script>
<script src=\"highcharts/modules/exporting.js\"></script>
<script src=\"highcharts/modules/export-data.js\"></script>
<div id=\"nav\">
    <ul class=\"navul\">
        <li class=\"navli\"><a class=\"navlia navactive\">
            Front Page
        </a></li>
        <li class=\"navli\"><a class=\"navlia\" href=\"functions.html\">
            Functions
        </a></li>
        <li class=\"navli\"><a class=\"navlia\" href=\"processes.html\">
            Processes
        </a></li>
    </ul>
</div>\n".

get_subpage_select(TestNodes) ->
"<script>
    function changePage(selectBox){
        var divsToHide = document.getElementsByClassName(\"page\");
        for(var i = 0; i < divsToHide.length; i++){
            divsToHide[i].style.display = \"none\";
        }
        pageId = selectBox.options[selectBox.selectedIndex].value;
        document.getElementById(pageId).style.display = \"block\";
    };
</script>
<div style=\"display: flex; align-items: center; justify-content: center\">
    <p style= \"font-family:Lucida Grande,Lucida Sans Unicode,Arial, "
                "Helvetica, sans-serif; font-size: 22px\">
        Test Results on node:
    </p>
    <select name=\"nodeSelect\" id=\"nodeSelect\""
        "onchange=\"changePage(this)\" "
        "style=\"
            font-size: 20px;
            font-family: Lucida Grande,Lucida Sans Unicode,Arial,
                            Helvetica, sans-serif;
            background-color: #bfe1f8;
            height: 35px;
            border-color: black;\">\n"++
    lists:flatten(["            <option value=\""++
                   atom_to_list(TestNode)++"page\">"++
                   atom_to_list(TestNode)++"</option>\n"
                   || TestNode <- TestNodes])++
"    </select>
</div>\n".

generate_node_subpage(TestName, TestSizes, Timestamp, Res) ->
    generate_node_subpage(TestName, TestSizes, Timestamp,
                          Res, {display, "none"}).
generate_node_subpage(TestName, TestSizes, Timestamp,
                      {TestNode,NodeResults}, {display, Mode}) ->
    NodeName = atom_to_list(TestNode),
"<div class=\"page\" id=\""++NodeName++"page\" style=\"display: "++Mode++"\">
    <div clasteardown_tests=\"row\" style=\"display: flex\">
        <div class=\"container\" id=\""++NodeName++"container1\"></div>
        <div class=\"container\" id=\""++NodeName++"container2\""
                "style=\"align-items: center; justify-content: center; "
                      "display: flex;\">"++
    generate_info_table(NodeName, TestName, TestSizes, Timestamp, NodeResults)++
"        </div>
    </div>
    <br><br>
    <div class=\"row\" style=\"display: flex\">
        <div class=\"container\" id=\""++NodeName++"container3\"></div>
        <div class=\"container\" id=\""++NodeName++"container4\"></div>
    </div>
</div>".

generate_info_table(NodeName, TestName, TestSizes, Timestamp, NodeResults) ->
    NumbersOfFuns = get_numbers_of_funs(NodeResults),
    NumbersOfProcs = get_numbers_of_procs(NodeResults),
    MaxActiveProcs = get_max_active_procs(NodeResults),
"<table style= \"margin: 50px\">
    <tbody>
        <tr>
            <th colspan=\""++performerl_html_test_results:format("~p",
                                                [length(TestSizes)+1])++"\""
                " style=\"text-align: center;\">
                Test Info
            </th>
        </tr>
        <tr>
            <td>Test Name</td>
            <td style=\"text-align: right;\" colspan=\""++
                performerl_html_test_results:format("~p",[length(TestSizes)])
                ++"\">"++TestName++"</td>
        </tr>
        <tr>
            <td>Test Date</td>
            <td style=\"text-align: right;\" colspan=\""++performerl_html_test_results:format("~p",
                                            [length(TestSizes)])++"\">"++
                performerl_html_test_results:pretty(Timestamp)++"</td>
        </tr>
        <tr>
            <td>Test Node</td>
            <td style=\"text-align: right;\" colspan=\""++
                performerl_html_test_results:format("~p",[length(TestSizes)])
                ++"\">"++
                performerl_html_test_results:pretty(NodeName)++"</td>
        </tr>
        <tr>
            <td>Test Sizes</td>"++
            lists:flatten([
                "<td style=\"text-align: right;\">"++
                performerl_html_test_results:format("~p", [Size])++"</td>\n" ||
                Size <- TestSizes
            ])++
"        </tr>
        <tr>
            <td>Number of Functions Detected</td>"++
            lists:flatten([
                "<td style=\"text-align: right;\">"++
                performerl_html_test_results:format("~p", [N])++
                "</td>\n" ||
                N <- NumbersOfFuns
            ])++
"        </tr>
        <tr>
            <td>Number of Processes Discovered</td>"++
            lists:flatten([
                "<td style=\"text-align: right;\">"++
                performerl_html_test_results:format("~p", [N])++
                "</td>\n" ||
                N <- NumbersOfProcs
            ])++
"        </tr>
        <tr>
            <td>Highest Number of Active Processes</td>"++
            lists:flatten([
                "<td style=\"text-align: right;\">"++
                performerl_html_test_results:format("~p", [N])++
                "</td>\n" ||
                N <- MaxActiveProcs
            ])++
"        </tr>
    </tbody>
</table>".

get_numbers_of_funs(NodeResults) ->
    [length(FBCT) ||
     {_Size,#node_run_data{funs_by_call_time=FBCT}} <- NodeResults].

get_numbers_of_procs(NodeResults) ->
    [maps:fold(fun(_, Pids, Acc) -> Acc+length(Pids) end, 0, PNP) ||
     {_Size,#node_run_data{procs_name_pid=PNP}} <- NodeResults].

get_max_active_procs(NodeResults) ->
    [MaxActive || {_Size,#node_run_data{max_active=MaxActive}} <- NodeResults].

generate_graphs(TestSizes, SizeLabel, {TestNode, Results}, N) ->
    NodeName = atom_to_list(TestNode),
    generate_call_time_graph(SizeLabel, TestSizes, NodeName, Results, N)++
    generate_memory_peak_graph(SizeLabel, TestSizes, NodeName, Results, N)++
    generate_reductions_peak_graph(SizeLabel, TestSizes, NodeName, Results, N).

generate_call_time_graph(SizeLabel, TestSizes, NodeName, NodeResults, N) ->
    TopCallTimeSeries = get_topN_call_time_series(N, NodeResults),
    performerl_html_test_results:line_graph(
        NodeName++"container1",
        "Function Total Call Time",
        "top"++integer_to_list(N)++
            " time consuming functions for each test run",
        "Microseconds",
        performerl_html_test_results:pretty(SizeLabel),
        TestSizes,
        TopCallTimeSeries,
        {click, NodeName++"/functions/"}
    ).

generate_memory_peak_graph(SizeLabel, TestSizes, NodeName, NodeResults, N) ->
    TopPeakMemorySeries = get_topN_peak_metric_series(N, memory, NodeResults),
    performerl_html_test_results:area_graph(
        NodeName++"container3",
        "Peak Total Memory",
        "top"++integer_to_list(N)++
            " memory consuming processes for each test run",
        "Bytes",
        performerl_html_test_results:pretty(SizeLabel),
        TestSizes,
        TopPeakMemorySeries,
        {click, NodeName++"/processes/"}
    ).

generate_reductions_peak_graph(SizeLabel, TestSizes, NodeName, NodeResults,N) ->
    TopPeakReductionsSeries =
        get_topN_peak_metric_series(N, reductions, NodeResults),
    performerl_html_test_results:line_graph(
        NodeName++"container4",
        "Peak Reductions Count",
        "top"++integer_to_list(N)++
            " processes by reductions count for each test run",
        "Number of Reductions",
        performerl_html_test_results:pretty(SizeLabel),
        TestSizes,
        TopPeakReductionsSeries,
        {click, NodeName++"/processes/"}
    ).

get_topN_call_time_series(N, Results) ->
    % for each size get the topN names
    Tmp = lists:map(
            fun({_Size, #node_run_data{funs_by_call_time=FunsByCallTime}}) ->
                lists:sublist(FunsByCallTime, 1, N)
            end,
        Results
    ),
    % remove duplicates
    % TODO: should we care about the fact that there can be up to N*|Sizes|
    %       different names? Should we limit them somehow?
    TopNames = lists:usort(lists:flatten(Tmp)),
    % for each one of the TopNames build the series
    lists:map(
        fun(MFA) ->
            {MFA,[case maps:find(MFA, FunsCallTime) of
                error -> null;
                {ok, {_Count, Time, _PPInfo}} ->
                    Time
             end
             || {_, #node_run_data{funs_call_time=FunsCallTime}} <- Results]}
        end,
        TopNames
    ).

get_topN_peak_metric_series(N, Metric, NodeResults) ->
    % for each size get the topN names
    Tmp = lists:map(
        fun({_Size, #node_run_data{procs_by_metrics=ProcsByMetrics}}) ->
            lists:map(
                fun({Name,_V}) -> Name end,
                lists:sublist(proplists:get_value(Metric, ProcsByMetrics),1,N))
        end,
        NodeResults
    ),
    TopNames = lists:usort(lists:flatten(Tmp)),
    % for each one of the TopNames build the series
    lists:map(
        fun(ProcName) ->
            {ProcName,
             [begin
                L = proplists:get_value(Metric, ProcsByMetrics),
                proplists:get_value(ProcName, L, null)
              end || {_, #node_run_data{procs_by_metrics=ProcsByMetrics}} <-
                     NodeResults]
            }
        end,
        TopNames
    ).
