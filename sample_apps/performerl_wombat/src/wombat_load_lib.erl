-module(wombat_load_lib).

-compile([export_all]).

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
                _ -> 
                    io:format("something went wrong trying to add node to Wombat, reason: ~p~n",[Result]),
                    node_not_added
            end;
        {error,Reason} ->
            io:format("something went wrong trying to add node to Wombat, reason: ~p~n",[Reason]),
            node_not_added
    end.

% removal of a node from wombat must be done after profiling is finished
% because when wombat purges the plugins they disappear from tracing counters
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

% if no websocket subscribes to these requests within 60 seconds they time out
start_service_request(NodeId,ServiceName) ->
    inets:start(),
    ServiceId = get_service_id_for_node(NodeId,ServiceName),
    Body = get_params_for_service(list_to_atom(ServiceName),ServiceId),
    Request = {
            "http://localhost:8080/api/request/service_request/",
            [{"Authorization", "Basic " ++ base64:encode_to_string("admin:password")}],
            "application/json",
            Body
    },
    {ok,{{_,200,"OK"},_,Json}} = httpc:request(post,Request,[],[]),
    EJson = jsx:decode(list_to_binary(Json)),
    proplists:get_value(<<"requestId">>,EJson).

get_params_for_service('Process Manager',ServiceId) ->
    "{\"id\":\""++ServiceId++"\", \"arguments\": {\"OrderBy\":\"Reductions\", \"Interval\": \"10\", \"Limit\": \"50\", \"OrderDirection\": \"Descending\", \"Page\": \"1\", \"FilterText\": \"\"}}";
get_params_for_service('Table Visualizer',ServiceId) ->
    "{\"id\":\""++ServiceId++"\", \"arguments\": {\"OrderBy\":\"Size (words)\", \"Interval\": \"10\", \"OrderDirection\": \"Descending\", \"Page\": \"1\", \"FilterText\": \"\"}}";
get_params_for_service(_,_) ->
    throw(unsupporter_service_request).

ws_subscribe_to_request(RequestId) ->
    application:ensure_all_started(gun),
    {ok,ConnPid} = gun:open("localhost",8080),
    gun:await_up(ConnPid),
    gun:ws_upgrade(ConnPid,"/api/core/ws"),
    receive
        {gun_upgrade, ConnPid, _StreamRef, [<<"websocket">>], _Headers} ->
            ok;
        {gun_response, ConnPid, _, _, Status, Headers} ->
            exit({ws_upgrade_failed, Status, Headers});
        {gun_error, ConnPid, _StreamRef, Reason} ->
            exit({ws_upgrade_failed, Reason})
    end,
    Salt = "fewfwfs",
    Pass = md5("admin"++Salt),
    gun:ws_send(ConnPid, {text,"auth=admin;websocket_pass="++Pass}),
    gun:ws_send(ConnPid, {text,"requests.stream.subscribe:"++RequestId}),
    receive
        {gun_ws, ConnPid, _, _} -> 
            {ok,ConnPid}
        after
            20000 ->
                exit(timeout)
    end.

ws_unsubscribe_from_request(ConnPid,RequestId) ->
    gun:ws_send(ConnPid, {text,"requests.stream.unsubscribe:"++RequestId}).


get_service_id_for_node(NodeId,ServiceName) ->
    {ok,Services} = get_all_explorer_services_for_node(NodeId),
    Service = lists:filter(
    fun(S) ->
        L = proplists:get_value(<<"label">>,lists:flatten(S)),
        ServiceName == binary_to_list(L)
    end,
    Services),
    binary_to_list(proplists:get_value(<<"serviceId">>,lists:flatten(Service))).

get_all_explorer_services_for_node(NodeId) ->
    inets:start(),
    Request = {
        "http://localhost:8080/api/service/service-info/explorer/node/"++
        binary_to_list(NodeId),
        [{"Authorization", "Basic " ++ base64:encode_to_string("admin:password")}]
    },
    case httpc:request(get,Request,[],[]) of
        {ok,Result} ->
            case Result of
                {{_,200,_},_,Json} ->
                    EJson = jsx:decode(list_to_binary(Json)),
                    {ok,proplists:get_value(<<"service_infos">>,EJson)};
                _ ->
                    bad_request
            end;
        {error,Reason} ->
            io:format("something went wrong while trying to contact Wombat, reason: ~p~n",[Reason]),
            bad_request
    end.

kernel_patterns() ->
    Mods = ['wombat_plugin_app', 'wombat_plugin_conn_sup',
    'wombat_plugin_controller',
    'wombat_plugin_deprecated', 'wombat_plugin_file_relay',
    'wombat_plugin_logger',
    'wombat_plugin_monitor_plugins', 'wombat_plugin_os_darwin_backend',
    'wombat_plugin_os_linux_backend', 'wombat_plugin_os_service',
    'wombat_plugin_os_win_backend', 'wombat_plugin_os',
    'wombat_plugin_proc_sup', 'wombat_plugin_relay',
    'wombat_plugin_service_sup', 'wombat_plugin_services',
    'wombat_plugin_tick_monitor', 'wombat_plugin_tracer',
    'wombat_plugin_urlcode',
    'wombat_types', 'wombat_plugin_alarm', 'wombat_plugin_application',
    'wombat_plugin_builtin_metrics',
    'wombat_plugin_code_tracer', 'wombat_plugin_config',
    'wombat_plugin_cowboy_common', 'wombat_plugin_cowboy',
    'wombat_plugin_disk_info', 'wombat_plugin_ejabberd',
    'wombat_plugin_elogger_handler', 'wombat_plugin_error_logger',
    'wombat_plugin_example', 'wombat_plugin_executors',
    'wombat_plugin_exometer', 'wombat_plugin_explorers',
    'wombat_plugin_fix_mnesia_netsplit', 'wombat_plugin_folsom',
    'wombat_plugin_lager_handler', 'wombat_plugin_lager',
    'wombat_plugin_mnesia_metrics', 'wombat_plugin_mnesia',
    'wombat_plugin_mongooseim_hosts', 'wombat_plugin_mongooseim_metrics',
    'wombat_plugin_mongooseim_users',
    'wombat_plugin_node_info', 'wombat_plugin_observer_backend',
    'wombat_plugin_observer_ets', 'wombat_plugin_observer_processes',
    'wombat_plugin_poolboy', 'wombat_plugin_process_monitor',
    'wombat_plugin_profiler_backend', 'wombat_plugin_profiler',
    'wombat_plugin_rabbitmq_alarms', 'wombat_plugin_rabbitmq_channels',
    'wombat_plugin_rabbitmq_connections', 'wombat_plugin_rabbitmq_exchanges',
    'wombat_plugin_rabbitmq_federation_upstreams',
    'wombat_plugin_rabbitmq_federation', 'wombat_plugin_rabbitmq_handler',
    'wombat_plugin_rabbitmq_metrics', 'wombat_plugin_rabbitmq_overview_stats',
    'wombat_plugin_rabbitmq_overview_util', 'wombat_plugin_rabbitmq_overview',
    'wombat_plugin_rabbitmq_queues',
    'wombat_plugin_rabbitmq', 'wombat_plugin_riak_core_metrics',
    'wombat_plugin_riak_core', 'wombat_plugin_riak_kv_metrics',
    'wombat_plugin_riak_kv', 'wombat_plugin_riak_repl_metrics',
    'wombat_plugin_riak_repl', 'wombat_plugin_sasl_handler',
    'wombat_plugin_sasl', 'wombat_plugin_shell_killer',
    'wombat_plugin_sys_info', 'wombat_plugin_telemetry_handler',
    'wombat_plugin_telemetry', 'wombat_plugin_user_command',
    'wombat_plugin_yaws', 'wombat_plugin_yokozuna_metrics',
    'wombat_plugin_yokozuna'],
    % not very significant
    % LibMods = [wombat_plugin_core_utils,
    %  wombat_plugin_monitor_plugins_utils,wombat_plugin_utils,
    %  wombat_plugin_builtin_metrics_lib,
    %  wombat_plugin_netsplit_lib,wombat_plugin_rabbitmq_utils],
    [{Mod,'_','_'} || Mod <- Mods].

md5(String) ->
    MD5 = erlang:md5(String),
    lists:flatten([ to_hex(I) || I <- binary_to_list(MD5)]).

to_hex(Int) ->
    binary_to_list(iolist_to_binary(io_lib:format("~2.16.0b",[Int]))).

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
