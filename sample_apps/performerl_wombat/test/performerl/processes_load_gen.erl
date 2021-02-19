-module(processes_load_gen).

-behaviour(performerl_load_generator).

-export([start_load/2, stop_load/1, get_load_duration/0,
         setup_run/1, teardown_run/1, get_test_sizes/0,
         get_trace_patterns/0, test_setup/0, test_teardown/1,
         get_test_name/0, spawn_processes/1]).

%%====================================================================
%% API functions
%%====================================================================

get_test_name() -> "Process Bomb".

test_setup() ->
    restfully_load_lib:test_setup(),
    ok = wombat_load_lib:ensure_wombat_started(),
    {ok, []}.

setup_run(Size) ->
    Node = 'processes@127.0.0.1',
    StartCmd = "erl -detached -name "++atom_to_list(Node)++
               " -setcookie "++atom_to_list(erlang:get_cookie())++
               " +P "++integer_to_list(Size),
    [] = os:cmd(StartCmd),
    ok = try_request(fun() ->
            Size = rpc:call(Node, erlang, system_info, [process_limit])
        end, 20, 500),

    % inject this module in order to have functions
    % to call with RPC during start_load
    % recompiling the module is a workaround because
    % code:get_object_code(?MODULE) return error, for some reason
    Path = proplists:get_value(source, ?MODULE:module_info(compile)),
    {ok,?MODULE,Bin} = compile:file(Path,[binary]),

    ok = performerl_lib:inject_binary(?MODULE, Bin, Node),
    {run_started,[Node]}.

get_trace_patterns() ->
    wombat_load_lib:kernel_patterns().

% 1 minute and 40 seconds, enough to make sure the builtin metrics
% are collected twice
get_load_duration() ->
    100000.

get_test_sizes() ->
    {number_of_processes,
     [round(math:pow(2,I))
      || I <- [16,17,18,19,20]]}.

start_load([Node], Size) ->

    % adds the node to Wombat
    io:format("~nadding node to wombat~n"),
    {node_added,WoNodeId} = wombat_load_lib:add_node_to_wombat(Node,
                                        atom_to_list(erlang:get_cookie())),
    performerl_lib:sleep_progress(4000),

    Pids = rpc:call(Node, ?MODULE, spawn_processes, [Size]),

    {load_started,[{node_info,{Node,WoNodeId}},{bomb_pids,Pids}]}.

stop_load(StopLoadInfo) ->
    {Node, WoNodeId} = proplists:get_value(node_info, StopLoadInfo),
    Pids = proplists:get_value(bomb_pids, StopLoadInfo),
    lists:foreach(fun(Pid) ->
            Pid ! stop
        end, Pids),
    {load_stopped, {Node, WoNodeId}}.

teardown_run({Node,WoNodeId}) ->
    % remove node from Wombat
    io:format("~nremoving node from wombat~n"),
    node_removed = wombat_load_lib:remove_node_from_wombat(WoNodeId),
    performerl_lib:sleep_progress(3000),
    ok = rpc:call(Node, init, stop, []),
    run_ended.

test_teardown(_TestInfo) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

spawn_processes(Size) ->
    Num = (Size * 95) div 100,
    Pids = [spawn(fun() -> receive stop -> ok end end) ||
            _ <- lists:seq(1, Num)].

try_request(_,0,_) -> {error,not_reachable};
try_request(Fun, N, Delay) ->
    try 
        Fun(),
        ok
    catch
        _:_ ->
            timer:sleep(Delay),
            try_request(Fun, N-1, Delay)
    end.
