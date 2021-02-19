-module(request_burst_load_gen).

-behaviour(performerl_load_generator).

-export([start_load/2, stop_load/1, get_load_duration/0,
         setup_run/1, teardown_run/1, get_test_sizes/0,
         get_trace_patterns/0, test_setup/0, test_teardown/1,
         get_test_name/0]).

%%====================================================================
%% API functions
%%====================================================================

get_test_name() -> "Request Burst".

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
            performerl_cowboy_app, performerl_cowboy_sup, cow_base64url,
            cow_cookie, cow_date, cow_hpack, cow_http2, cow_http2_machine,
            cow_http, cow_http_hd, cow_http_te, cow_iolists, cow_mimetypes,
            cow_multipart, cow_qs, cow_spdy, cow_sse, cow_uri, cow_ws,
            ranch_acceptor, ranch_acceptors_sup, ranch_app, ranch_conns_sup,
            ranch_crc32c, ranch, ranch_listener_sup, ranch_protocol,
            ranch_proxy_header, ranch_server, ranch_ssl, ranch_sup,
            ranch_tcp, ranch_transport],
    [{Mod,'_','_'} || Mod <- Mods].

get_load_duration() ->
    60000.

get_test_sizes() ->
    {number_of_requests, [10, 100, 1000, 10000]}.

start_load([Node], Size) ->
    %%**********DEBUG***********%
%     rpc:call(Node,erlang,trace_pattern,
%              [{performerl_process_discoverer_agent,'_','_'},
%               true, [call_time]]),
%     {call_time, []} =
%         rpc:call(Node, erlang, trace_info,
%                  [{performerl_process_discoverer_agent,stop,0}, call_time]),
%     rpc:call(Node,erlang,trace_pattern,
%              [{erlang,trace_pattern,3},
%               true, [call_time]]),
%    {call_time, []} =
%        rpc:call(Node, erlang, trace_info,
%                 [{erlang,trace_pattern,3}, call_time]),
    %%**********DEBUG***********%
    ok = httpc:set_options([{max_keep_alive_length, 0},
                            {keep_alive_timeout, 0}]),
    lists:foreach(fun(_) ->
            {ok, {{_,200,_}, _, "Hello World!"}} =
                httpc:request("http://localhost:4242/")
        end, lists:seq(1,Size)),
    {load_started,[Node]}.

stop_load([_Node]) ->
    %%**********DEBUG***********%
    % count_patterns(Node),
    % M = performerl_process_discoverer_agent,
    % rpc:call(Node, erlang, trace_pattern,
    %          [{erlang,trace_pattern,3}, pause, [call_time]]),
    % TP = rpc:call(Node, erlang, trace_info,
    %         [{erlang,trace_pattern,3}, call_time]),
    % io:format("erlang:trace_pattern/3\n~p\n", [TP]),
    % rpc:call(Node, erlang, trace_pattern, [{M,'_','_'}, pause, [call_time]]),
    % lists:foreach(fun({F,A}) ->
    %         Info = rpc:call(Node, erlang, trace_info, [{M,F,A}, call_time]),
    %         io:format("~p:~p/~p\n~p\n", [M,F,A,Info])
    %     end, performerl_process_discoverer_agent:module_info(functions)),
    %%**********DEBUG***********%
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

% count_patterns(Node)->
%     Mods = [cowboy_app,cowboy_bstr,cowboy_children,cowboy_clear,
%             cowboy_clock,cowboy_compress_h,cowboy_constraints,
%             cowboy,cowboy_handler,cowboy_http2,cowboy_http,
%             cowboy_loop,cowboy_metrics_h,cowboy_middleware,cowboy_req,
%             cowboy_rest,cowboy_router,cowboy_static,cowboy_stream,
%             cowboy_stream_h,cowboy_sub_protocol,cowboy_sup,cowboy_tls,
%             cowboy_tracer_h,cowboy_websocket, hello_handler,
%             performerl_cowboy_app, performerl_cowboy_sup],
%     L = [length(rpc:call(Node, Mod, module_info, [functions])) || Mod <- Mods],
%     Tot = lists:foldl(fun(E,Acc) -> Acc + E end, 0, L),
%     io:format("in totale erano ~p pattern~n",[Tot]).

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
