-module(performerl_html_test_results).

-include_lib("../include/performerl.hrl").

-export([generate/1, get_static_header/1, format/2, pretty/1,
         generate_series/1, line_graph/7, line_graph/8,
         area_graph/7, area_graph/8, write_results/2]).

-ifdef(TEST).
    -compile([export_all]).
-endif.

-spec generate(
    TestResults :: #test_results{}
) ->
    {ok, Pages:: list()}.

write_results(BaseDir, [Front, PIndex, FIndex, PDetails, FDetails]) ->
    ok = file:write_file(BaseDir++"front_page.html",
                         io_lib:format("~s~n",[Front])),
    ok = file:write_file(BaseDir++"processes.html",
                         io_lib:format("~s~n",[PIndex])),
    ok = file:write_file(BaseDir++"functions.html",
                         io_lib:format("~s~n",[FIndex])),
    % copy highcharts lib
    [] = os:cmd("cp -r "++filename:dirname(code:which(?MODULE))++
           "/../src/vendor/highcharts \""++BaseDir++"/highcharts\""),
    % print processes details
    lists:foreach(fun({NodeName, NodePDetails}) ->
            NodeDir = BaseDir++pretty(NodeName)++"/",
            ok = file:make_dir(NodeDir),
            ok = file:make_dir(NodeDir++"processes/"),
            lists:foreach(fun({ProcName, Page}) ->
                    Name = pretty(ProcName),
                    ok = file:write_file(NodeDir++"processes/"++Name++".html",
                                         io_lib:format("~s~n",[Page]))
                end, NodePDetails)
        end, PDetails),
    % print functions details
    lists:foreach(fun({NodeName, NodeFDetails}) ->
            NodeDir = BaseDir++pretty(NodeName)++"/",
            ok = file:make_dir(NodeDir++"functions/"),
            lists:foreach(fun({FunName, Page}) ->
                    Name = pretty(FunName),
                    ok = file:write_file(NodeDir++"functions/"++Name++".html",
                                         io_lib:format("~s~n",[Page]))
                end, NodeFDetails)
        end, FDetails),
    ok.

generate(TestResults=#test_results{results_by_node=ResultsByNode,
                                   size_label=SizeLabel}) ->
    {ok, Front} = performerl_html_front_page:generate(TestResults),
    {ok, PIndex} = performerl_html_pindex:generate(TestResults),
    {ok, FIndex} = performerl_html_findex:generate(TestResults),
    % for each node generate the process details
    PDetails = 
        [{NodeName,
          performerl_html_pdetails:generate(NodeName, NodeResults, SizeLabel)} ||
         {NodeName, NodeResults} <- ResultsByNode],
    % and the function details
    FDetails =
        [{NodeName,
          performerl_html_fdetails:generate(NodeName, NodeResults, SizeLabel)} ||
         {NodeName, NodeResults} <- ResultsByNode],
    {ok, [Front, PIndex, FIndex, PDetails, FDetails]}.

get_static_header(Title) ->
"<!DOCTYPE HTML>
<html>
<head>
    <meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\">
    <meta name=\"viewport\"content=\"width=device-width,initial-scale=1\">
    <title>"++Title++"</title>
    <style type=\"text/css\">

        .container {
            min-width: 310px;
            max-width: 800px;
            height: 500px;
            width: 100%;
            margin: 0 auto
        }

        .page{
            
        }

        #svg {
            width: 100%
        }

        table {
            font-family: Lucida Grande, Lucida Sans Unicode, "
                         "Arial, Helvetica, sans-serif;
            border-collapse: collapse;
            width: 100%;
        }

        td, th {
            border: 1px solid #cccccc;
            text-align: left;
            padding: 8px;
        }

        tr:nth-child(even) {
            background-color: #bfe1f8;
        }

        .navul {
            list-style-type: none;
            margin: 0;
            padding: 0;
            overflow: hidden;
            background-color: #333;
            font-family: Lucida Grande, Lucida Sans Unicode,Arial,
                            Helvetica, sans-serif;
        }

        .navli {
            float: left;
        }

        .navlia {
            display: block;
            color: white;
            text-align: center;
            padding: 14px 16px;
            text-decoration: none;
        }

        .navlia:hover:not(.navactive) {
            background-color: #111;
        }

        .navactive {
            background-color: #0098ff;
        }
    </style>
</head>\n".

line_graph(Container, Title, SubTitle, YLabel, XLabel, Categories, Series) ->
    line_graph(Container, Title, SubTitle, YLabel,
               XLabel, Categories, Series, none).
line_graph(Container, Title, SubTitle, YLabel, XLabel,
           Categories, Series, ClickLink) ->
"Highcharts.chart('"++Container++"', {
    title: {
        text: '"++Title++"'
    },
    credits: {
        enabled: false
    },
    subtitle: {
        text: '"++SubTitle++"'
    },
    yAxis: {
        title: {
            text: '"++YLabel++"'
        }
    },
    xAxis: {
        title: {
            text: '"++XLabel++"'
        },
        categories: "++format("~p",[Categories])++"
    },
    "++
    case ClickLink of
        none -> "";
        {click, URL} ->"
    plotOptions: {
        series: {
            cursor: 'pointer',
            point: {
                events: {
                    click: function () {
                        location.href = '"++URL++"' +
                            this.series.name + '.html';
                    }
                }
            }
        }
    },
    "
    end
    ++generate_series(Series)++"
});\n".


area_graph(Container, Title, SubTitle, YLabel, XLabel, Categories, Series) ->
    area_graph(Container, Title, SubTitle, YLabel,
               XLabel, Categories, Series, none).
area_graph(Container, Title, SubTitle, YLabel,
           XLabel, Categories, Series, ClickLink) ->
"Highcharts.chart('"++Container++"', {
    chart: {
        type: 'area'
    },
    credits: {
        enabled: false
    },
    plotOptions: {
        area: {
            marker: {
                enabled: true,
                symbol: 'circle',
                radius: 2
            }
        }"++
        case ClickLink of
            none -> "";
            {click, URL} ->",
        series: {
            cursor: 'pointer',
            point: {
                events: {
                    click: function () {
                        location.href = '"++URL++"' +
                            this.series.name + '.html';
                    }
                }
            }
        }"
        end++"
    },
    title: {
        text: '"++Title++"'
    },
    subtitle: {
        text: '"++SubTitle++"'
    },
    yAxis: {
        title: {
            text: '"++YLabel++"'
        }
    },
    xAxis: {
        title: {
            text: '"++XLabel++"'
        },
        categories: "++format("~p",[Categories])++"
    },
    "++generate_series(Series)++"
});\n".

generate_series(ListOfSeries) ->
    "series: [\n"++
    lists:flatten(["    {name: '"++pretty(Name)++"', data: "++format("~p",[Data])++
                   "},\n" || {Name, Data} <- ListOfSeries])
    ++"]".

pretty({{Y,M,D},{H,Min,S}}) ->
    format("~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b", [Y,M,D,H,Min,S]);
pretty({M,F,A}) ->
    Filtered = re:replace(atom_to_list(F), "/", ".", [global, {return, list}]),
    format("~s:~s:~p", [M,Filtered,A]);
pretty(N) when is_integer(N)-> 
    format("~b", [N]);
pretty(P) when is_pid(P) -> 
    format("~p", [P]);
pretty(Name) -> 
    format("~s", [Name]).

format(FmtStr, Args) ->
    lists:flatten(io_lib:format(FmtStr, Args)).
