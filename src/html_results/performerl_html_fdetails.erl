-module(performerl_html_fdetails).

-include_lib("../include/performerl.hrl").

-export([generate/3]).

% for each function name generate its function details page
generate(NodeName, NodeResults, SizeLabel) ->
    AllFuncNames = get_all_func_names(NodeResults),
    lists:map(
        fun(FunName) ->
            Page = generate_function_detail_page(NodeName, FunName,
                                                NodeResults, SizeLabel),
            {FunName, Page}
        end,
        AllFuncNames
    ).

get_all_func_names(NodeResults) ->
    lists:usort(lists:foldl(
        fun({_,#node_run_data{funs_by_call_time=Funs}}, Acc) ->
            Acc ++ Funs
        end,
        [],
        NodeResults
    )).

generate_function_detail_page(NodeName, FunName, NodeResults, SizeLabel) ->
    Title = "Test Results - "++performerl_html_test_results:pretty(FunName),
    performerl_html_test_results:get_static_header(Title)++
    "<body>\n"++
    get_nav()++
    get_title(NodeName, FunName)++
    get_fixed_row()++
    get_size_select(NodeResults)++
    get_using_processes_table(FunName, NodeResults)++
    get_graphs(FunName, NodeResults, SizeLabel)++
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

get_title(NodeName, FunName) ->
"<div style=\"display: flex; align-items: center;"
                         "justify-content: center\">
    <p style= \"font-family:Lucida Grande,Lucida Sans Unicode,Arial,"
                           "Helvetica, sans-serif; font-size: 22px\">
        Detailed test results for function \""++
        performerl_html_test_results:pretty(FunName)++
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
        Function details for size:
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
get_using_processes_table(FunName, [{FirstSize,FirstRes} | OtherNodeResults]) ->
    lists:flatten(
        [performerl_html_test_results:format(
"<div class=\"row\" id=\"row~b\" style=\"display:block\">
    <div style=\"display: flex; justify-content: center\">"++
         generate_using_processes_table(FunName, FirstRes)++"
    </div>
    <br>
</div>", [FirstSize])],

        lists:map(
            fun({Size, Res}) ->
                performerl_html_test_results:format(
"<div class=\"row\" id=\"row~b\" style=\"display:none\">
    <div style=\"display: flex; justify-content: center\">"++
                generate_using_processes_table(FunName, Res)++"
    </div>
    <br>
</div>",        [Size])
            end,
            OtherNodeResults)).

generate_using_processes_table(FunName, #node_run_data{funs_call_time=FCT}) ->
    PPInfo = case maps:find(FunName, FCT) of
        {ok, {_,_, PPI}} -> PPI;
        error -> []
    end,
"<table style=\"width:1000px;\">
    <tbody>
        <tr>
            <th colspan=3 style=\"text-align: center;\">
                Processes that used the function
            </th>
        </tr>
        <tr>
            <th>Process Name</th>
            <th>Number of Calls</th>
            <th>Call Time (in microseconds)</th>
        </tr>
        "++lists:flatten([
"        <tr>
            <td>"++make_link(ProcName)++"</td>
            <td style=\"text-align: right;\">"++performerl_html_test_results:pretty(NumOfCalls)++"</td>
            <td style=\"text-align: right;\">"++performerl_html_test_results:pretty(Time)++"</td>
        </tr>" || {ProcName,NumOfCalls,Time} <- PPInfo])++"
    </tbody>
</table>".

make_link(ProcName) ->
    "<a href=\"../processes/"++
    performerl_html_test_results:pretty(ProcName)++".html\">"++
    performerl_html_test_results:pretty(ProcName)++"</href>".

get_graphs(FunName, NodeResults, SizeLabel) ->
    "<script type=\"text/javascript\">"++
    % first the top graphs copied from the front page
    generate_function_call_time_graph(FunName, NodeResults, SizeLabel)++
    generate_function_call_count_graph(FunName, NodeResults, SizeLabel).

generate_function_call_time_graph(FunName, NodeResults, SizeLabel) ->
    CallTimeSeries = {FunName, lists:map(
        fun({_, #node_run_data{funs_call_time=FCT}}) ->
            case maps:find(FunName, FCT) of
                {ok, {_, Time, _}} -> Time;
                error -> null
            end
        end,
        NodeResults
    )},
    TestSizes = [Size || {Size, _} <- NodeResults],
    performerl_html_test_results:area_graph(
        "fixedContainer1",
        "Function Call Time",
        "total function execution times across all test runs",
        "Microseconds",
        performerl_html_test_results:pretty(SizeLabel),
        TestSizes,
        [CallTimeSeries]
    ).

generate_function_call_count_graph(FunName, NodeResults, SizeLabel) ->
    CallCountSeries = {FunName, lists:map(
        fun({_, #node_run_data{funs_call_time=FCT}}) ->
            case maps:find(FunName, FCT) of
                {ok, {Count, _, _}} -> Count;
                error -> null
            end
        end,
        NodeResults
    )},
    TestSizes = [Size || {Size, _} <- NodeResults],
    performerl_html_test_results:line_graph(
        "fixedContainer2",
        "Function Call Count",
        "total number of calls to the functon across all test runs",
        "Number of Calls",
        performerl_html_test_results:pretty(SizeLabel),
        TestSizes,
        [CallCountSeries]
    ).
