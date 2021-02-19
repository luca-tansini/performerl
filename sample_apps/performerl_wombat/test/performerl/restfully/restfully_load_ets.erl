-module(restfully_load_ets).

-behaviour(performerl_load_generator).

-export([start_load/2, stop_load/1, get_load_duration/0,
         setup_run/1, teardown_run/1, get_test_sizes/0,
         get_trace_patterns/0, test_setup/0, test_teardown/1,
         get_test_name/0]).

%%====================================================================
%% API functions
%%====================================================================

get_test_name() -> "Restfully ETS".

test_setup() ->
    restfully_load_lib:test_setup(),
    ok = wombat_load_lib:ensure_wombat_started(),
    {ok, []}.

setup_run(Size) ->
    Node = 'restfully@127.0.0.1',
    StartCmd = "cd "++restfully_load_lib:get_restfully_dir()++"; "++
               "PORT=4000 iex --name "++
               atom_to_list(Node)++
               " --cookie "++atom_to_list(erlang:get_cookie())++
               " --erl \"+e "++integer_to_list(Size)++"\""
               " -S mix phoenix.server",
    % detached doesn't work, we need to spawn an extra process
    spawn(fun() -> os:cmd(StartCmd) end),
    ok = restfully_load_lib:try_request(fun() -> 
            restfully_load_lib:get_counters() 
        end, 20, 150),
    Size = rpc:call(Node, erlang, system_info, [ets_limit]),
    {run_started,[Node]}.

get_trace_patterns() ->
    restfully_load_lib:get_trace_patterns().

get_load_duration() ->
    restfully_load_lib:get_load_duration().

get_test_sizes() ->
    {number_of_ets_tables, [ 2053,
                             4106,
                            10265,
                            20530]}.

start_load([Node], _TestSize) ->

    % adds the node to Wombat
    io:format("~nadding node to wombat~n"),
    {node_added,WoNodeId} = wombat_load_lib:add_node_to_wombat(Node,
                                        atom_to_list(erlang:get_cookie())),
    performerl_lib:sleep_progress(4000),

    restfully_load_lib:ets_bomb(),
    {load_started,[{Node,WoNodeId}]}.

stop_load(StopLoadInfo) ->
    restfully_load_lib:ets_defuse(),
    io:format("~ndefusing ets bomb~n"),
    {load_stopped,StopLoadInfo}.

teardown_run(TeardownInfo) ->
    restfully_load_lib:teardown_run(TeardownInfo).

test_teardown(_TestInfo) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
