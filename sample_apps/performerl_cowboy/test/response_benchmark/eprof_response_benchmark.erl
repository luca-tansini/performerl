-module(eprof_response_benchmark).

-include_lib("eunit/include/eunit.hrl").

response_benchmark_test_() ->
{
    timeout,
    6000,
    fun() ->
        ?debugFmt("\nresponse_benchmark_test, this will take a while...\n",[]),
        {ok, _} = net_kernel:start(['eunit@127.0.0.1', longnames]),
        L = lists:map(fun(N) ->
                    {N,response_benchmark(N)}
                end,
                [10,100,1000,10000,100000]),
        ?debugFmt("\n~p\n", [L])
    end
}.

response_benchmark(Size) ->
    % start server and ensure it is started
    StartCmd = "_build/default/rel/performerl_cowboy/bin/performerl_cowboy start",
    [] = os:cmd(StartCmd),
    ?debugFmt("\nRun with size ~p started\n", [Size]),
    ok = try_request(fun() ->
            {ok, {{_,200,_}, _, "Hello World!"}} =
                httpc:request("http://localhost:4242/")
        end, 20, 500),

    % rpc start eprof in cowboy node
    true = erlang:set_cookie('performerl_cowboy@127.0.0.1', performerl),
    % Procs = rpc:call('performerl_cowboy@127.0.0.1', erlang, processes, []),
    % Pids = [convert_pid(P) || P <- Procs],
    profiling =
        rpc:call('performerl_cowboy@127.0.0.1', eprof, start_profiling, [[all]]),

    % time #Size syncronous requests
    % ok = httpc:set_options([{max_keep_alive_length, 0},
    %                         {keep_alive_timeout, 0},
    %                         {max_sessions, 0}]),
    {T,Fs} = lists:foldl(
        fun(N, {Acc, Fs}) ->
            % timer:sleep(1),
            T1 = erlang:system_time(microsecond),
            F = case httpc:request("http://localhost:4242/") of
                {ok, _} -> 0;
                _ -> 1
            end,
            T2 = erlang:system_time(microsecond),
            {Acc + T2-T1, Fs+F}
        end,
        {0,0},
        lists:seq(1,Size)),

    profiling_stopped =
        rpc:call('performerl_cowboy@127.0.0.1', eprof, stop_profiling, []),

    Cmd = "_build/default/rel/performerl_cowboy/bin/performerl_cowboy stop",
    "ok\n" = os:cmd(Cmd),
    {T, Fs}.

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

convert_pid(Pid) ->
    try
        S = lists:flatten(io_lib:format("~p",[Pid])),
        R = re:replace(S, "<\\d*\\.", "<0.", [{return,list}]),
        erlang:list_to_pid(R)
    catch
        _:_ ->
            io:format(lists:flatten(
                io_lib:format("pid: ~p could not be converted!~n", [Pid]))),
            Pid
    end.
