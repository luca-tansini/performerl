-module(performerl_html_findex).

-include_lib("../include/performerl.hrl").

-export([generate/1]).

generate(#test_results{results_by_node = Results = [First | Others]}) ->
    TestNodes = [TestNode || {TestNode, _} <- Results],
    {_, Res} = First,
    TestSizes = [Size || {Size, _} <- Res],
    HTML =
    performerl_html_test_results:get_static_header("Functions Index")++
    "<body>\n"++
    get_nav()++
    get_subpage_select(TestNodes, TestSizes)++
    % one hidden subpage for each node
    generate_node_size_subpages(First, {display, "flex"})++
    lists:flatten([generate_node_size_subpages(Result)
                   || Result <- Others])++
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
        <li class=\"navli\"><a class=\"navlia\" href=\"front_page.html\">
            Front Page
        </a></li>
        <li class=\"navli\"><a class=\"navlia navactive\">
            Functions
        </a></li>
        <li class=\"navli\"><a class=\"navlia\" href=\"processes.html\">
            Processes
        </a></li>
    </ul>
</div>\n".

get_subpage_select(TestNodes, TestSizes) ->
"<script>
    function changePage(){
        var divsToHide = document.getElementsByClassName(\"page\");
        for(var i = 0; i < divsToHide.length; i++){
            divsToHide[i].style.display = \"none\";
        }
        var nodeSelect = document.getElementById(\"nodeSelect\");
        var sizeSelect = document.getElementById(\"sizeSelect\");
        node = nodeSelect.options[nodeSelect.selectedIndex].value;
        size = sizeSelect.options[sizeSelect.selectedIndex].value;
        document.getElementById(node+\"-\"+size+\"page\").style.display =
            \"flex\";
    };
</script>
<div style=\"display: flex; align-items: center; justify-content: center\">
    <p style= \"font-family:Lucida Grande,Lucida Sans Unicode,Arial, "
                "Helvetica, sans-serif; font-size: 22px\">
        Functions calls on node:
    </p>
    <select name=\"nodeSelect\" id=\"nodeSelect\""
        "onchange=\"changePage()\" "
        "style=\"
            font-size: 20px;
            font-family: Lucida Grande,Lucida Sans Unicode,Arial,
                            Helvetica, sans-serif;
            background-color: #bfe1f8;
            height: 35px;
            border-color: black;\">\n"++
    lists:flatten(["            <option value=\""++
                   atom_to_list(TestNode)++"\">"++
                   atom_to_list(TestNode)++"</option>\n"
                   || TestNode <- TestNodes])++
"    </select>
</div>
<div style=\"display: flex; align-items: center; justify-content: center\">
    <p style= \"font-family:Lucida Grande,Lucida Sans Unicode,Arial, "
                "Helvetica, sans-serif; font-size: 18px\">
        Sorted by results for test run with size:
    </p>
    <select name=\"sizeSelect\" id=\"sizeSelect\""
        "onchange=\"changePage()\" "
        "style=\"
            font-size: 16px;
            font-family: Lucida Grande,Lucida Sans Unicode,Arial,
                            Helvetica, sans-serif;
            background-color: #bfe1f8;
            height: 30px;
            border-color: black;\">\n"++
    lists:flatten(["            <option value=\""++
                   performerl_html_test_results:pretty(Size)++"\">"++
                   performerl_html_test_results:pretty(Size)++"</option>\n"
                   || Size <- TestSizes])++
"    </select>
</div>\n".

generate_node_size_subpages(Res) ->
    generate_node_size_subpages(Res, {display, "none"}).

% the first one needs to be handled in a special way to only
% show the index for the first run size
generate_node_size_subpages({TestNode,[{FirstSize, FirstRes}|Others]},
                            {display, "flex"}) ->
    NodeName = atom_to_list(TestNode),
    lists:flatten(
"<div class=\"page\" id=\""++NodeName++"-"++
        performerl_html_test_results:pretty(FirstSize)++"page\""
    "style=\"display: flex; justify-content: center\">"++
        generate_index_and_summary_table(TestNode, FirstRes)++
"</div>",
        lists:map(fun({Size, Results}) ->
"<div class=\"page\" id=\""++NodeName++"-"++
            performerl_html_test_results:pretty(Size)++"page\""
    "style=\"display: none; justify-content: center\">"++
            generate_index_and_summary_table(TestNode, Results)++
"</div>"
            end,
            Others)
    );
generate_node_size_subpages({TestNode,NodeResults}, {display, Mode}) ->
    NodeName = atom_to_list(TestNode),
    lists:map(fun({Size, Results}) ->
"<div class=\"page\" id=\""++NodeName++"-"++
        performerl_html_test_results:pretty(Size)++"page\""
    "style=\"display: "++Mode++"; justify-content: center\">"++
        generate_index_and_summary_table(TestNode, Results)++
"</div>"
        end,
        NodeResults).

generate_index_and_summary_table(TestNode,
    #node_run_data{funs_by_call_time = FuncsByCallTime,
                   funs_call_time = FunMap}) ->
"<table style=\"width:1000px;\">
    <tbody>
        <tr>
            <th>MFA</th><th>Call count</th><th>Call time (in microseconds)</th>
        </tr>
        "++lists:flatten(
            lists:map(fun(FuncName) ->
                {Count, Time, _} = maps:get(FuncName, FunMap),
"        <tr>
            <td><a href=\""++make_fdetail_link(TestNode, FuncName)++"\">"++
            performerl_html_test_results:pretty(FuncName)++"</a></td>
            <td style=\"text-align: right;\">"++
                performerl_html_test_results:pretty(Count)++"</td>
            <td style=\"text-align: right;\">"++
                performerl_html_test_results:pretty(Time)++"</td>
        </tr>"
                end,
                FuncsByCallTime)
    )++"
    </tbody>
</table>".

make_fdetail_link(TestNode, FuncName) ->
    performerl_html_test_results:pretty(TestNode)++"/functions/"++
    performerl_html_test_results:pretty(FuncName)++".html".
