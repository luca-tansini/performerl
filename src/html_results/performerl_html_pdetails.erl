-module(performerl_html_pdetails).

-include_lib("../include/performerl.hrl").

-export([generate/3]).

% for each process name generate its process details page
generate(NodeName, NodeResults, SizeLabel) ->
    AllProcessNames = get_all_proc_names(NodeResults),
    lists:map(
        fun(ProcName) ->
            Page = generate_process_detail_page(NodeName, ProcName,
                                                NodeResults, SizeLabel),
            {ProcName, Page}
        end,
        AllProcessNames
    ).

get_all_proc_names(NodeResults) ->
    lists:usort(lists:foldl(
        fun({_,#node_run_data{procs_name_pid=ProcsMap}}, Acc) ->
            Acc ++ maps:keys(ProcsMap)
        end,
        [],
        NodeResults
    )).

generate_process_detail_page(NodeName, ProcName, NodeResults, SizeLabel) ->
    Title = "Test Results - "++performerl_html_test_results:pretty(ProcName),
    performerl_html_test_results:get_static_header(Title)++
    "<body>\n"++
    get_nav()++
    get_title(NodeName, ProcName)++
    get_fixed_row()++
    get_pid_count_table(ProcName, NodeResults)++
    get_size_select(NodeResults)++
    get_graph_containers_and_used_funs_tab(ProcName, NodeResults)++
    get_graphs(ProcName, NodeResults, SizeLabel)++
    "</script>\n"
	"</body>\n"
    "</html>\n".

get_nav() ->
"<script src=\"../../highcharts/highcharts.js\"></script>
<script src=\"../../highcharts/modules/series-label.js\"></script>
<script src=\"../../highcharts/modules/exporting.js\"></script>
<script src=\"../../highcharts/modules/export-data.js\"></script>
<div id=\"nav\">
    <ul class=\"navul\">
        <li class=\"navli\"><a class=\"navlia\" href=\"../../front_page.html\">
            Front Page
        </a></li>
        <li class=\"navli\"><a class=\"navlia\" href=\"../../functions.html\">
            Functions
        </a></li>
        <li class=\"navli\"><a class=\"navlia\" href=\"../../processes.html\">
            Processes
        </a></li>
    </ul>
</div>\n".

get_title(NodeName, ProcName) ->
"<div style=\"display: flex; align-items: center;"
                         "justify-content: center\">
    <p style= \"font-family:Lucida Grande,Lucida Sans Unicode,Arial,"
                           "Helvetica, sans-serif; font-size: 22px\">
        Detailed test results for process \""++
        performerl_html_test_results:pretty(ProcName)++
        "\" on node: "++
        performerl_html_test_results:pretty(NodeName)++"
    </p>"
"</div>".

% first row with the containers for the overall charts
get_fixed_row() ->
"<div id=\"fixedRow1\" style=\"display: flex\">
    <div class=\"container\" id=\"fixedContainer1\"></div>
    <div class=\"container\" id=\"fixedContainer2\"></div>
</div>
<br><br>".

% second row with the pid count table
get_pid_count_table(ProcName, NodeResults) ->
    NumOfCols = length(NodeResults),
"<div id=\"fixedRow2\" style=\"display: flex; justify-content: center\">
    <table style=\"width:800px;\">
        <tbody>
            <tr>
                <th colspan="++performerl_html_test_results:pretty(NumOfCols)++
                    " style=\"text-align: center;\">
                    Number of PIDs with name \""++
                    performerl_html_test_results:pretty(ProcName)++
                    "\" for each test size
                </th>
            </tr>
            <tr>
            "++lists:flatten(
                [performerl_html_test_results:format(
                    "<td style=\"text-align: right;\">~b</td>", [Size])
                 || {Size,_} <- NodeResults])++"
            </tr>
            <tr>
            "++lists:flatten(lists:map(
                fun({_,#node_run_data{procs_name_pid=ProcsMap}}) ->
                    N = case maps:find(ProcName, ProcsMap) of
                        error -> 0;
                        {ok, Pids} -> length(Pids)
                    end,
                    performerl_html_test_results:format(
                        "<td style=\"text-align: right;\">~b</td>", [N])
                end,
                NodeResults
            ))++"
            </tr>
        </tbody>
    </table>
</div>
<br><br>".

% third row with the size selector for the following charts and tables
get_size_select(NodeResults) ->
"<script>
    function changeRow(selectBox){
        var rowsToHide = document.getElementsByClassName(\"row\");
        for(var i = 0; i < rowsToHide.length; i++){
            rowsToHide[i].style.display = \"none\";
        }
        rowId = selectBox.options[selectBox.selectedIndex].value;
        document.getElementById(rowId).style.display = \"block\";
    };
</script>
<div id=\"fixedRow3\" style=\"display: flex; justify-content: center;"
                             "align-items: center\">
    <p style=\"font-family:Lucida Grande,Lucida Sans Unicode,Arial,"
                          "Helvetica, sans-serif; font-size: 20px\">
        Detailed process metrics for size:
    </p>
    <select name=\"sizeSelect\" id=\"sizeSelect\""
            "onchange=\"changeRow(this)\"
            style=\"
                font-size: 18px;
                font-family: Lucida Grande,Lucida Sans Unicode,Arial,
                                Helvetica, sans-serif;
                background-color: #bfe1f8;
                height: 31px;
                border-color: black;\">\n"++
    lists:flatten([
        performerl_html_test_results:format(
            "        <option value=\"row~b\">~b</option>\n", [Size, Size])
        || {Size, _} <- NodeResults])++
    "</select>
</div><br><br>".

% treat the first size differently to print that one and hide the others
get_graph_containers_and_used_funs_tab(ProcName, [{FirstSize, FirstRes} |
                                                  OtherNodeResults]) ->
    lists:flatten(
        [performerl_html_test_results:format(
"<div class=\"row\" id=\"row~b\" style=\"display:block\">
    <div style=\"display:flex\">
    <div class=\"container\" id=\"container~ba\"></div>
    <div class=\"container\" id=\"container~bb\"></div></div>
    <br>
    <div style=\"display: flex; justify-content: center\">"++
         generate_used_funs_table(ProcName, FirstRes)++"
    </div>
    <br>
</div>", [FirstSize, FirstSize, FirstSize])],

        lists:map(
            fun({Size, Res}) ->
                performerl_html_test_results:format(
"<div class=\"row\" id=\"row~b\" style=\"display:none\">
    <div style=\"display:flex\">
    <div class=\"container\" id=\"container~ba\"></div>
    <div class=\"container\" id=\"container~bb\"></div>
    </div>
    <br>
    <div style=\"display: flex; justify-content: center\">"++
                generate_used_funs_table(ProcName, Res)++"
    </div>
    <br>
</div>",        [Size, Size, Size])
            end,
            OtherNodeResults)).

generate_used_funs_table(ProcName, #node_run_data{procs_used_funs=PUF}) ->
    UsedFuns = lists:sort(fun({_,_,T1}, {_,_,T2}) -> T1 >= T2 end,
                          case maps:find(ProcName, PUF) of
                              {ok, L} -> L;
                              error -> []
                          end),
"<table style=\"width:1000px;\">
    <tbody>
        <tr>
            <th colspan=3 style=\"text-align: center;\">
                Functions Used by Process
            </th>
        </tr>
        <tr>
            <th>Function MFA</th>
            <th>Number of Calls</th>
            <th>Call Time (in microseconds)</th>
        </tr>
        "++lists:flatten([
"        <tr>
            <td>"++make_link(MFA)++"</td>
            <td style=\"text-align: right;\">"++
                performerl_html_test_results:pretty(NumOfCalls)++"</td>
            <td style=\"text-align: right;\">"++
                performerl_html_test_results:pretty(Time)++"</td>
        </tr>" || {MFA,NumOfCalls,Time} <- UsedFuns])++"
    </tbody>
</table>".

make_link(MFA) ->
    "<a href=\"../functions/"++
    performerl_html_test_results:pretty(MFA)++".html\">"++
    performerl_html_test_results:pretty(MFA)++"</href>".

get_graphs(ProcName, NodeResults, SizeLabel) ->
    "<script type=\"text/javascript\">"++
    % first the top graphs copied from the front page
    generate_process_memory_peak_graph(ProcName, NodeResults, SizeLabel)++
    generate_process_reductions_peak_graph(ProcName, NodeResults, SizeLabel)++
    % then the historic graphs for each test size
    generate_memory_history_graphs(ProcName, NodeResults)++
    generate_reductions_history_graphs(ProcName, NodeResults).

generate_process_memory_peak_graph(ProcName, NodeResults, SizeLabel) ->
    PeakMemorySeries = {ProcName, lists:map(
        fun({_, #node_run_data{procs_by_metrics=PBM}}) ->
            proplists:get_value(ProcName,
                proplists:get_value(memory, PBM), null)
        end,
        NodeResults
    )},
    TestSizes = [Size || {Size, _} <- NodeResults],
    performerl_html_test_results:area_graph(
        "fixedContainer1",
        "Peak Total Memory",
        "peak total memory for the process across all test runs",
        "Bytes",
        performerl_html_test_results:pretty(SizeLabel),
        TestSizes,
        [PeakMemorySeries]
    ).

generate_process_reductions_peak_graph(ProcName, NodeResults, SizeLabel) ->
    PeakReductionsSeries = {ProcName, lists:map(
        fun({_, #node_run_data{procs_by_metrics=PBM}}) ->
            proplists:get_value(ProcName,
                proplists:get_value(reductions, PBM), null)
        end,
        NodeResults
    )},
    TestSizes = [Size || {Size, _} <- NodeResults],
    performerl_html_test_results:line_graph(
        "fixedContainer2",
        "Peak Reductions Count",
        "peak reductions count for the process across all test runs",
        "Number of Reductions",
        performerl_html_test_results:pretty(SizeLabel),
        TestSizes,
        [PeakReductionsSeries]
    ).

generate_memory_history_graphs(ProcName, NodeResults) ->
    lists:flatten(lists:map(fun({Size, NodeRunData}) ->
            generate_memory_history_graph(ProcName, Size, NodeRunData)
        end, NodeResults)).

generate_memory_history_graph(ProcName, Size,
                              #node_run_data{procs_name_pid = PNP,
                                             procs_metric_hist = PMH}) ->
    Pids = case maps:find(ProcName, PNP) of
        {ok, P} -> P;
        error -> []
    end,
    Categories = [short_timestamp(Ts) || {Ts, _} <- PMH],
    TmpMap = lists:foldl(
        fun({_Ts, Metrics}, Acc1) ->
            lists:foldl(fun(Pid, Acc2) ->
                    V = proplists:get_value(
                            memory,
                            proplists:get_value(Pid, Metrics, []),
                            null),
                    maps:update_with(Pid, fun(Old) -> Old++[V] end, [V], Acc2)
                end, Acc1, Pids)
        end, maps:new(), PMH),
    MemoryHistorySeriesByPid = maps:to_list(TmpMap),
    performerl_html_test_results:area_graph(
        "container"++performerl_html_test_results:pretty(Size)++"a",
        "Memory History by PID",
        "test size: "++performerl_html_test_results:pretty(Size),
        "Bytes",
        "timestamp",
        Categories,
        MemoryHistorySeriesByPid
    ).

generate_reductions_history_graphs(ProcName, NodeResults) ->
    lists:flatten(lists:map(fun({Size, NodeRunData}) ->
            generate_reductions_history_graph(ProcName, Size, NodeRunData)
        end, NodeResults)).

generate_reductions_history_graph(ProcName, Size,
                              #node_run_data{procs_name_pid = PNP,
                                             procs_metric_hist = PMH}) ->
    Pids = case maps:find(ProcName, PNP) of
        {ok, P} -> P;
        error -> []
    end,
    Categories = [short_timestamp(Ts) || {Ts, _} <- PMH],
    TmpMap = lists:foldl(
        fun({_Ts, Metrics}, Acc1) ->
            lists:foldl(fun(Pid, Acc2) ->
                    V = proplists:get_value(
                            reductions,
                            proplists:get_value(Pid, Metrics, []),
                            null),
                    maps:update_with(Pid, fun(Old) -> Old++[V] end, [V], Acc2)
                end, Acc1, Pids)
        end, maps:new(), PMH),
    ReductionsHistorySeriesByPid = maps:to_list(TmpMap),
    performerl_html_test_results:line_graph(
        "container"++performerl_html_test_results:pretty(Size)++"b",
        "Number of Reductions History by PID",
        "test size: "++performerl_html_test_results:pretty(Size),
        "Number of Reductions",
        "timestamp",
        Categories,
        ReductionsHistorySeriesByPid
    ).

short_timestamp(RawTs) ->
    {_Date, {H,Min,S}} = calendar:gregorian_seconds_to_datetime(RawTs div 1000),
    performerl_html_test_results:format("~2..0b:~2..0b:~2..0b", [H,Min,S]).
