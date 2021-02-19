-module(restfully_load_message_queue).

-behaviour(performerl_load_generator).

-export([start_load/2, stop_load/1, get_load_duration/0,
         setup_run/1, teardown_run/1, get_test_sizes/0,
         get_trace_patterns/0, test_setup/0, test_teardown/1,
         get_test_name/0]).

%%====================================================================
%% API functions
%%====================================================================

get_test_name() -> "Restfully Message Queue".

test_setup() ->
    restfully_load_lib:test_setup(),
    ok = wombat_load_lib:ensure_wombat_started(),
    {ok, []}.

setup_run(_Size) ->
    restfully_load_lib:setup_run().

get_trace_patterns() ->
    restfully_load_lib:get_trace_patterns().

get_load_duration() ->
    restfully_load_lib:get_load_duration().

get_test_sizes() ->
    {number_of_incr_counter_requests, [  10,
                                        100,
                                        500,
                                       1000]}.

start_load([Node], TestSize) ->

    % adds the node to Wombat
    io:format("~nadding node to wombat~n"),
    {node_added,WoNodeId} = wombat_load_lib:add_node_to_wombat(Node,
                                        atom_to_list(erlang:get_cookie())),
    performerl_lib:sleep_progress(4000),

    % don't care about the specific counter, we just want to make sure
    % at least one exists so that id 1 is valid
    restfully_load_lib:create_counter("test", 0),
    [ begin 
        restfully_load_lib:incr_counter(1),
        timer:sleep(50)
      end || _ <- lists:seq(1,TestSize)],
    {load_started,[{Node,WoNodeId}]}.

stop_load(StopLoadInfo) ->
    % nothing to do here
    {load_stopped,StopLoadInfo}.    

teardown_run(TeardownInfo) ->
    restfully_load_lib:teardown_run(TeardownInfo).

test_teardown(_TestInfo) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
