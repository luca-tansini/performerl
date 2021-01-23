-module(html_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../include/performerl.hrl").

html_test_() ->
{
    timeout, 6000,
fun() ->
    Filename = "test/html_test_sample_results.raw",
    {ok, TestResultsBin} = file:read_file(Filename),
    TestResults = #test_results{results_by_node=RBN=[{_Node, NodeRunData}|_]} = 
        binary_to_term(TestResultsBin),
    % force an extra fake test node (with same data)
    TestRes2 = TestResults#test_results{
        results_by_node = RBN++[{'fake_node@localhost', NodeRunData}]
    },
    {ok, HTML} = performerl_html_test_results:generate(TestRes2)
    ,Timestamp = performerl_lib:get_timestamp(),
    file:make_dir("test/html_test"),
    BaseDir = "test/html_test/html_test@"++
              performerl_lib:format_date(Timestamp)++"/",
    ok = file:make_dir(BaseDir),
    performerl_html_test_results:write_results(BaseDir, HTML)
end
}.
