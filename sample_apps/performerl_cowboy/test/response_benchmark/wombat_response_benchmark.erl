-module(wombat_response_benchmark).

-include_lib("eunit/include/eunit.hrl").

response_benchmark_test_() ->
{
    timeout,
    6000,
    fun() ->
        ?debugFmt("\nresponse_benchmark_test, this will take a while...\n",[]),
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

    % add node to wombat
    ok = ensure_wombat_started(),
    {node_added, NodeId} =
        add_node_to_wombat('performerl_cowboy@127.0.0.1', "performerl"),
    timer:sleep(3000),

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

    % remove node from wombat
    node_removed = remove_node_from_wombat(NodeId),
    timer:sleep(2000),

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

%*******************************************************************************
% wombat lib
%*******************************************************************************

-define(WOMBAT_DIR, "/home/tanso/Desktop/wombat").

ensure_wombat_started() ->
    case is_wombat_up_with_right_credentials() of
        ok -> ok;
        {error, bad_admin_password} ->
            % for now we can only fail, we could update the password though
            {error, bad_admin_password};
        {error, api_not_working} ->
            {error, api_not_working};
        {error, not_started} ->
            StartCmd = ?WOMBAT_DIR ++ "/scripts/wombat start_script start",
            os:cmd(StartCmd),
            case wait_until(fun() ->
                    ok == is_wombat_up_with_right_credentials()
                end, 15, 1000) of
                true -> ok;
                {error, maxretries} -> {error, couldnt_start_wombat}
            end
    end.

add_node_to_wombat(Node, Cookie) ->

    %prepare tracing for wombat (basically remove it so that wombat can use it)
    rpc:call(Node, erlang, trace, [processes, false, [call]]),

    Name = atom_to_list(Node),
    inets:start(),
    Body = "{\"name\":\""++Name++"\", \"cookie\":\""++Cookie++"\"}",
    Request = {
        "http://localhost:8080/api/topo/action/add-node",
        [{"Authorization", "Basic " ++ base64:encode_to_string("admin:password")}],
        "application/json",
        Body
    },
    case httpc:request(post,Request,[],[]) of
        {ok,Result} ->
            case Result of
                {{_,200,_},_,Json} ->
                    EJson = jsx:decode(list_to_binary(Json)),
                    Id = proplists:get_value(<<"id">>,lists:flatten(EJson)),
                    {node_added,Id};
                _ -> node_not_added
            end;
        {error,Reason} ->
            io:format("something went wrong trying to add node to Wombat, reason: ~p~n",[Reason]),
            node_not_added
    end.

remove_node_from_wombat(Id) ->
    inets:start(),
    Request = {
        "http://localhost:8080/api/topo/node/"++binary_to_list(Id),
        [{"Authorization", "Basic " ++ base64:encode_to_string("admin:password")}],
        "application/json",
        []
    },
    case httpc:request(delete,Request,[],[]) of
        {ok,Result} ->
            case Result of
                {{_,204,_},_,_} -> node_removed;
                _ -> node_not_removed
            end;
        {error,Reason} ->
            io:format("something went wrong trying to remove node from Wombat, reason: ~p~n",[Reason]),
            node_not_removed
    end.

wait_until(_Fun, 0, _Sleep) -> {error, maxretries};
wait_until(Fun, Retries, Sleep) ->
    case Fun() of
        true -> true;
        false ->
            timer:sleep(Sleep),
            wait_until(Fun, Retries-1, Sleep)
    end.

is_wombat_up_with_right_credentials() ->
    PingCmd = ?WOMBAT_DIR ++ "/scripts/wombat start_script ping",
    Ping = os:cmd(PingCmd),
    case Ping of
        "pong\n" ->
            case try_simple_API_request() of
                ok -> ok;
                {error, 401} -> {error, bad_admin_password};
                error -> {error, api_not_working}
            end;
        _ -> {error, not_started}
    end.

try_simple_API_request() ->
    inets:start(),
    Request = {
        "http://localhost:8080/api/users/username",
        [{"Authorization",
          "Basic " ++ base64:encode_to_string("admin:password")}]
    },
    case httpc:request(get,Request,[],[]) of
        {ok,Result} ->
            case Result of
                {{_,200,_},_,_} -> ok;
                {{_,401,_},_,_} -> {error, 401};
                _ -> error
            end;
        {error, _} ->
            error
    end.
