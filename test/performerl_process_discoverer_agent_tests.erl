-module(performerl_process_discoverer_agent_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

meta_tracing_ms_test() ->
    Pid1 = spawn(fun() -> timer:sleep(100), hello() end),
    _Pid2 = spawn(fun() -> timer:sleep(100), hello() end),
    MS = [{'_', [{'=/=',{self},{const, Pid1}}], []}],
    1 = erlang:trace_pattern({?MODULE, hello, 0}, MS, [meta]),
    receive
        _Msg -> 
            % ?debugFmt("expected: ~p~n", [_Msg])
            ok
    after 200 ->
        throw(nothing_received)
    end,
    receive
        _Msg1 ->
            % ?debugFmt("unexpected: ~p~n", [_Msg1]),
            throw(unexpected)
    after 200 ->
        ok
    end.

meta_tracing_discoverer_mslist_test() ->
    process_discoverer_ms_test(list).

meta_tracing_discoverer_mstree_test() ->
    process_discoverer_ms_test(tree).

split_test() ->
    ?assertEqual({a,[],[]}, performerl_process_discoverer_agent:split([a])),
    ?assertEqual({b,[a],[]}, performerl_process_discoverer_agent:split([a,b])),
    ?assertEqual({b,[a],[c]}, performerl_process_discoverer_agent:split([a,b,c])),
    ?assertEqual({c,[a,b],[d]}, performerl_process_discoverer_agent:split([a,b,c,d])),
    ?assertEqual({c,[a,b],[d,e]}, performerl_process_discoverer_agent:split([a,b,c,d,e])),
    ?assertEqual({d,[a,b,c],[e,f]}, performerl_process_discoverer_agent:split([a,b,c,d,e,f])),
    ?assertEqual({d,[a,b,c],[e,f,g]},
                 performerl_process_discoverer_agent:split([a,b,c,d,e,f,g])).

list_to_tree_test() ->
    ?assertEqual({}, performerl_process_discoverer_agent:list_to_tree([])),
    ?assertEqual({a}, performerl_process_discoverer_agent:list_to_tree([a])),
    ?assertEqual({b,{a},{}}, performerl_process_discoverer_agent:list_to_tree([a,b])),
    ?assertEqual({b,{a},{c}}, performerl_process_discoverer_agent:list_to_tree([a,b,c])),
    ?assertEqual({c,{b,{a},{}},{d}},
                 performerl_process_discoverer_agent:list_to_tree([a,b,c,d])),
    ?assertEqual({c,{b,{a},{}},{e,{d},{}}},
                 performerl_process_discoverer_agent:list_to_tree([a,b,c,d,e])),
    ?assertEqual({d,{b,{a},{c}},{f,{e},{}}},
                 performerl_process_discoverer_agent:list_to_tree([a,b,c,d,e,f])),
    ?assertEqual({d,{b,{a},{c}},{f,{e},{g}}},
                 performerl_process_discoverer_agent:list_to_tree([a,b,c,d,e,f,g])).

list_to_ms_tree_test() ->
    ?assertEqual(false, performerl_process_discoverer_agent:list_to_ms_tree([])),
    ?assertEqual({'==', {self}, {const, a}},
                 performerl_process_discoverer_agent:list_to_ms_tree([a])),
    ?assertEqual(
        {'orelse',
            {'==', {self}, {const, b}},
            {'andalso',
                {'<', {self}, {const, b}},
                {'==', {self}, {const, a}}
            },
            {'andalso', 
                {'>', {self}, {const, b}},
                false
            }
        }, performerl_process_discoverer_agent:list_to_ms_tree([a,b])),
    ?assertEqual(
        {'orelse',
            {'==', {self}, {const, b}},
            {'andalso',
                {'<', {self}, {const, b}},
                {'==', {self}, {const, a}}
            },
            {'andalso',
                {'>', {self}, {const, b}},
                {'==', {self}, {const, c}}
            }
        }, performerl_process_discoverer_agent:list_to_ms_tree([a,b,c])),
    ?assertEqual(
        {'orelse',
            {'==', {self}, {const, c}},
            {'andalso',
                {'<', {self}, {const, c}},
                {'orelse',
                    {'==', {self}, {const, b}},
                    {'andalso',
                        {'<', {self}, {const, b}},
                        {'==', {self}, {const, a}}
                    },
                    {'andalso',
                        {'>', {self}, {const, b}},
                        false
                    }
                }
            },
            {'andalso',
                {'>', {self}, {const, c}},
                {'==', {self}, {const, d}}
            }
        }, performerl_process_discoverer_agent:list_to_ms_tree([a,b,c,d])),
    ?assertEqual(
        {'orelse',
            {'==', {self}, {const, c}},
            {'andalso',
                {'<', {self}, {const, c}},
                {'orelse',
                    {'==', {self}, {const, b}},
                    {'andalso',
                        {'<', {self}, {const, b}},
                        {'==', {self}, {const, a}}
                    },
                    {'andalso',
                        {'>', {self}, {const, b}},
                        false
                    }
                }
            },
            {'andalso',
                {'>', {self}, {const, c}},
                {'orelse',
                    {'==', {self}, {const, e}},
                    {'andalso',
                        {'<', {self}, {const, e}},
                        {'==', {self}, {const, d}}
                    },
                    {'andalso',
                        {'>', {self}, {const, e}},
                        false
                    }
                }
            }
        }, performerl_process_discoverer_agent:list_to_ms_tree([a,b,c,d,e])),
        ?assertEqual(
        {'orelse',
            {'==', {self}, {const, d}},
            {'andalso',
                {'<', {self}, {const, d}},
                {'orelse',
                    {'==', {self}, {const, b}},
                    {'andalso',
                        {'<', {self}, {const, b}},
                        {'==', {self}, {const, a}}
                    },
                    {'andalso',
                        {'>', {self}, {const, b}},
                        {'==', {self}, {const, c}}
                    }
                }
            },
            {'andalso',
                {'>', {self}, {const, d}},
                {'orelse',
                    {'==', {self}, {const, f}},
                    {'andalso',
                        {'<', {self}, {const, f}},
                        {'==', {self}, {const, e}}
                    },
                    {'andalso',
                        {'>', {self}, {const, f}},
                        false
                    }
                }
            }
        }, performerl_process_discoverer_agent:list_to_ms_tree([a,b,c,d,e,f])),
        ?assertEqual(
        {'orelse',
            {'==', {self}, {const, d}},
            {'andalso',
                {'<', {self}, {const, d}},
                {'orelse',
                    {'==', {self}, {const, b}},
                    {'andalso',
                        {'<', {self}, {const, b}},
                        {'==', {self}, {const, a}}
                    },
                    {'andalso',
                        {'>', {self}, {const, b}},
                        {'==', {self}, {const, c}}
                    }
                }
            },
            {'andalso',
                {'>', {self}, {const, d}},
                {'orelse',
                    {'==', {self}, {const, f}},
                    {'andalso',
                        {'<', {self}, {const, f}},
                        {'==', {self}, {const, e}}
                    },
                    {'andalso',
                        {'>', {self}, {const, f}},
                        {'==', {self}, {const, g}}
                    }
                }
            }
        }, performerl_process_discoverer_agent:list_to_ms_tree([a,b,c,d,e,f,g])).

get_discovered_test() ->
    spawn_discoverer([{?MODULE, hello, 0}], tree),
    timer:sleep(100),
    hello(),
    Pid1 = spawn_link(?MODULE, hello_wait, []),
    Pid2 = spawn_link(fun() -> 
        hello_wait()
    end),
    register(dummy,Pid2),
    timer:sleep(100),
    {ok,Discovered} = performerl_process_discoverer_agent:get_discovered_processes(),
    {ok,Active} = performerl_process_discoverer_agent:get_active_processes(),
    
    ?assertEqual(3, length(Discovered), length(Active)),
    TracedFun = {?MODULE, hello, 0},
    ?assert(lists:member({self(),TracedFun}, Discovered)),
    ?assert(lists:member({Pid1, TracedFun}, Discovered)),
    ?assert(lists:member({Pid2, dummy}, Discovered)),
    ?assert(lists:member(self(), Active)),
    ?assert(lists:member(Pid1, Active)),
    ?assert(lists:member(Pid2, Active)),
    Pid1 ! stop,
    Pid2 ! stop,
    timer:sleep(100),
    % make sure it's correct after the processes die
    {ok,Discovered2} = performerl_process_discoverer_agent:get_discovered_processes(),
    {ok,Active2} = performerl_process_discoverer_agent:get_active_processes(),
    ?assertEqual(3, length(Discovered2)),
    ?assertEqual(1, length(Active2)),
    ?assert(lists:member({self(),TracedFun}, Discovered2)),
    ?assert(lists:member({Pid1, TracedFun}, Discovered2)),
    ?assert(lists:member({Pid2, dummy}, Discovered2)),
    ?assert(lists:member(self(), Active2)),
    ?assert(not lists:member(Pid1, Active2)),
    ?assert(not lists:member(Pid2, Active2)),
    stop_discoverer(),
    ok.

still_works_after_emptying() ->
    spawn_discoverer([{?MODULE, hello, 0}], tree),
    timer:sleep(100),
    Pid1 = spawn_link(?MODULE, hello_wait, []),
    Pid2 = spawn_link(?MODULE, hello_wait, []),
    timer:sleep(100),
    {ok,Discovered} = performerl_process_discoverer_agent:get_discovered_processes(),
    ?assertEqual(2, length(Discovered)),
    Pid1 ! stop,
    Pid2 ! stop,
    timer:sleep(100),
    % make sure it's correct after the processes die
    {ok,Discovered2} = performerl_process_discoverer_agent:get_discovered_processes(),
    ?assertEqual(0, length(Discovered2)),
    Pid3 = spawn_link(?MODULE, hello_wait, []),
    Pid4 = spawn_link(?MODULE, hello_wait, []),
    timer:sleep(100),
    {ok,Discovered3} = performerl_process_discoverer_agent:get_discovered_processes(),
    ?assertEqual(2, length(Discovered3)),
    Pid3 ! stop,
    Pid4 ! stop,
    stop_discoverer(),
    ok.

%***********************************************
% internal functions
%***********************************************

process_discoverer_ms_test(MSType) ->
    DiscPid = spawn_discoverer([{?MODULE, hello, 0}], MSType),
    timer:sleep(50),
    % check at first it's empty
    ?assertEqual([], get_active()),
    hello(),
    % then it contains this process
    ?assertEqual([self()], get_active()),

    % spawn another 10 processes and check they're in the active list
    Pids = [spawn(fun() -> hello(), receive stop -> ok end end) ||
            _ <- lists:seq(1,10)],
    erlang:yield(),
    ?assertEqual(11, length(get_active())),

    % stop one and check the active list decreases
    lists:nth(1, Pids) ! stop,
    erlang:yield(),
    ?assertEqual(10, length(get_active())),

    % stop the rest and check both active and discovered are correct
    [Pid ! stop || Pid <- Pids],
    erlang:yield(),
    ?assertEqual([self()], get_active()),
    ?assertEqual(11, length(get_discovered())),

    % spawn some more to build a meaningful MS, then spawn one that makes
    % multiple calls and check the discoverer doesn't get unwanted messages
    Pids2 = [spawn(fun() -> hello(), receive stop -> ok end end) ||
            _ <- lists:seq(1,100)],
    timer:sleep(100),
    ?assertEqual(101, length(get_active())),
    timer:sleep(100),
    MultiPid = spawn(fun F() ->
            hello(),
            receive
                stop -> ok;
                call -> F()
            end
        end),
    erlang:yield(),
    ?assertEqual(102, length(get_active())),
    performerl_process_discoverer_agent:block(),
    erlang:yield(),
    ?assertEqual({messages, []}, erlang:process_info(DiscPid, messages)),
    MultiPid ! call,
    timer:sleep(100),
    ?assertEqual({messages, []}, erlang:process_info(DiscPid, messages)),
    DiscPid ! unblock,
    MultiPid ! stop,
    [Pid ! stop || Pid <- Pids2],
    timer:sleep(100),
    ?assertEqual([self()], get_active()),
    ?assertEqual(112, length(get_discovered())),
    stop_discoverer().

get_active() ->
    {ok, L} = performerl_process_discoverer_agent:get_active_processes(),
    L.

get_discovered() ->
    {ok, L} = performerl_process_discoverer_agent:get_discovered_processes(),
    L.

spawn_discoverer(Patterns, MSType) ->
    {ok, Pid} = performerl_process_discoverer_agent:start_link(
        #{patterns => Patterns, ms_type => MSType}
    ),
    Pid.
stop_discoverer() ->
    ok = performerl_process_discoverer_agent:stop(),
    % small sleep to wait for the discoverer to stop
    timer:sleep(100).

hello() -> hello.

hello_wait() ->
    hello(),
    receive stop -> ok end.
