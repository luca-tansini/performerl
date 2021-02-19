-module(response_benchmark_load_gen).

-behaviour(performerl_load_generator).

-export([start_load/2, stop_load/1, get_load_duration/0,
         setup_run/1, teardown_run/1, get_test_sizes/0,
         get_trace_patterns/0, test_setup/0, test_teardown/1,
         get_test_name/0]).

%%====================================================================
%% API functions
%%====================================================================

get_test_name() -> "Response Benchmark".

test_setup() ->
    {ok, []}.

setup_run(_Size) ->
    Node = 'performerl_cowboy@127.0.0.1',
    StartCmd = "_build/default/rel/performerl_cowboy/bin/performerl_cowboy start",
    [] = os:cmd(StartCmd),
    ok = try_request(fun() ->
            {ok, {{_,200,_}, _, "Hello World!"}} =
                httpc:request("http://localhost:4242/")
        end, 20, 500),
    {run_started,[Node]}.

get_trace_patterns() ->
    Mods = [cowboy_app,cowboy_bstr,cowboy_children,cowboy_clear,
            cowboy_clock,cowboy_compress_h,cowboy_constraints,
            cowboy,cowboy_handler,cowboy_http2,cowboy_http,
            cowboy_loop,cowboy_metrics_h,cowboy_middleware,cowboy_req,
            cowboy_rest,cowboy_router,cowboy_static,cowboy_stream,
            cowboy_stream_h,cowboy_sub_protocol,cowboy_sup,cowboy_tls,
            cowboy_tracer_h,cowboy_websocket, hello_handler,
            performerl_cowboy_app, performerl_cowboy_sup],
    [{Mod,'_','_'} || Mod <- Mods].

get_load_duration() ->
    30000.

get_test_sizes() ->
    {number_of_requests, [10, 100, 1000]}.

start_load([Node], Size) ->
    % time #Size syncronous requests
    % ok = httpc:set_options([{max_keep_alive_length, 0},
    %                         {keep_alive_timeout, 0},
    %                         {max_sessions, 0}]),
    {T,Fs} = lists:foldl(
        fun(N, {Acc, Fs}) ->
            T1 = erlang:system_time(microsecond),
            F = case httpc:request("http://localhost:4242/") of
                {ok, _} -> 0;
                _ -> 1
            end,
            T2 = erlang:system_time(microsecond),
            {Acc + T2-T1, Fs + F}
        end,
        {0,0},
        lists:seq(1,Size)),
    io:format("\nTotal response time with size ~p: ~p, (with ~p errors)\n", [Size, T, Fs]),
    {load_started,[Node]}.

stop_load([Node]) ->
    {load_stopped, []}.

teardown_run(_) ->
    Cmd = "_build/default/rel/performerl_cowboy/bin/performerl_cowboy stop",
    "ok\n" = os:cmd(Cmd),
    run_ended.

test_teardown(_) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

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
