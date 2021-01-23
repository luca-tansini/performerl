-module(performerl_html_pindex).

-include_lib("../include/performerl.hrl").

-export([generate/1]).

generate(#test_results{results_by_node = Results = [First | Others]}) ->
    TestNodes = [TestNode || {TestNode, _} <- Results],
    HTML =
    performerl_html_test_results:get_static_header("Process Index")++
    "<body>\n"++
    get_nav()++
    get_subpage_select(TestNodes)++
    % one hidden subpage for each node
    generate_node_subpage(First, {display, "flex"})++
    lists:flatten([generate_node_subpage(Result)
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
        <li class=\"navli\"><a class=\"navlia\" href=\"functions.html\">
            Functions
        </a></li>
        <li class=\"navli\"><a class=\"navlia navactive\">
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
        document.getElementById(pageId).style.display = \"flex\";
    };
</script>
<div style=\"display: flex; align-items: center; justify-content: center\">
    <p style= \"font-family:Lucida Grande,Lucida Sans Unicode,Arial, "
                "Helvetica, sans-serif; font-size: 22px\">
        Processes* discovered on node:
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
</div>
<div style=\"display: flex; align-items: center; justify-content: center\">
    <p style= \"font-family:Lucida Grande,Lucida Sans Unicode,Arial, "
                "Helvetica, sans-serif; font-size: 15px; color: #6f6f6f;"
                "margin-top: 0px\">
        *The name given to the process is its registry name (when available) or"
        " the MFA of the function it was discovered by
    </p>
</div>\n".

generate_node_subpage(Res) -> generate_node_subpage(Res, {display, "none"}).
generate_node_subpage({TestNode,NodeResults}, {display, Mode}) ->
    NodeName = atom_to_list(TestNode),
"<div class=\"page\" id=\""++NodeName++"page\""
    "style=\"display: "++Mode++"; justify-content: center\">"++
    generate_index_table(TestNode, NodeResults)++
"</div>".

generate_index_table(TestNode, NodeResults) ->
    Procs = lists:usort(lists:flatten([
        maps:keys(PNP)
        ||
        {_Size, #node_run_data{procs_name_pid = PNP}} <- NodeResults
    ])),
"<table style=\"width:1000px;\">
    <tbody>
        <tr>
            <th>Process Name</th>
        </tr>
        "++lists:flatten([
"        <tr>
            <td><a href=\""++make_pdetail_link(TestNode, ProcName)++"\">"++
            performerl_html_test_results:pretty(ProcName)++"</a></td>
        </tr>" || ProcName <- Procs])++"
    </tbody>
</table>".

make_pdetail_link(TestNode, ProcName) ->
    performerl_html_test_results:pretty(TestNode)++"/processes/"++
    performerl_html_test_results:pretty(ProcName)++".html".
