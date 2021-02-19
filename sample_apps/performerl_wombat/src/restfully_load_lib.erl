-module(restfully_load_lib).

-export([get_trace_patterns/0,setup_run/0, teardown_run/1, get_restfully_dir/0]).
-export([get_counters/0,create_counter/2,get_counter/1,next_counter/1,
         incr_counter/1,http_millis/1,delay_millis/1,ets_bomb/0,
         ets_defuse/0,process_bomb/0,process_defuse/0,atom_bomb/0,
         get_load_duration/0,try_request/3, test_setup/0]).

-define(RESTFULLY_FOLDER, "/home/tanso/Desktop/restfully").

%%====================================================================
%% API functions
%%====================================================================

get_restfully_dir() -> ?RESTFULLY_FOLDER.

test_setup() ->
    inets:start(),
    os:cmd("brew services start postgresql"),
    timer:sleep(1000).

setup_run() ->
    Node = 'restfully@127.0.0.1',
    StartCmd = "cd "++?RESTFULLY_FOLDER++"; "++
               "PORT=4000 iex --name "++
               atom_to_list(Node)++
               " --cookie "++atom_to_list(erlang:get_cookie())++
               " -S mix phoenix.server",
    % detached doesn't work, we need to spawn an extra process
    spawn(fun() -> os:cmd(StartCmd) end),
    ok = try_request(fun() -> get_counters() end, 20, 150),
    {run_started,[Node]}.

% return a list of traced patterns
get_trace_patterns() ->
    Mods = [wombat_plugin_ecto,wombat_plugin_elixir_logger,
    wombat_plugin_elixir_shell,wombat_plugin_eval_elixir_expr,
    wombat_plugin_phoenix,wombat_plugin_alarm,
    wombat_plugin_builtin_metrics,wombat_plugin_config,
    wombat_plugin_cowboy,wombat_plugin_error_logger,
    wombat_plugin_executors,wombat_plugin_exometer,
    wombat_plugin_explorers,wombat_plugin_lager,
    wombat_plugin_node_info,wombat_plugin_observer_ets,
    wombat_plugin_observer_processes,wombat_plugin_poolboy,
    wombat_plugin_process_monitor,wombat_plugin_profiler,
    wombat_plugin_shell_killer,wombat_plugin_user_command],
    wombat_load_lib:kernel_patterns() ++ [{Mod,'_','_'} || Mod <- Mods].

% 1 minute and 40 seconds, enough to make sure we collect the metrics twice
get_load_duration() ->
    100000.

teardown_run([{Node,WoNodeId}]) ->

    % removes the node from Wombat
    io:format("~nremoving node from wombat~n"),
    node_removed = wombat_load_lib:remove_node_from_wombat(WoNodeId),
    performerl_lib:sleep_progress(3000),

    ok = rpc:call(Node, init, stop, []),
    run_ended.

get_counters() ->
    {ok,{{_,200,_},_,_}} = httpc:request(get,
        {"http://localhost:4000/counters",[]},[],[]).

create_counter(Name,InitCount) ->
    {ok,{{_,201,_},_,_}} = httpc:request(post,
        {"http://localhost:4000/counters",[{}],"application/json",
        "{\"counter\": {\"name\": \""++Name++"\", \"count\": "++integer_to_list(InitCount)++"}}"},[],[]).

get_counter(Id) ->
    {ok,{{_,200,_},_,_}} = httpc:request(get,
        {"http://localhost:4000/counters/"++integer_to_list(Id),[]},[],[]).

next_counter(Id) ->
    {ok,{{_,200,_},_,_}} = httpc:request(get,
        {"http://localhost:4000/counters/"++integer_to_list(Id)++"/next",[]},[],[]).

incr_counter(Id) ->
    {ok,{{_,200,_},_,_}} = httpc:request(get,
        {"http://localhost:4000/counters/"++integer_to_list(Id)++"/incr",[]},[],[]).

http_millis(Delay) ->
    {ok,{{_,200,_},_,_}} = httpc:request(put,
        {"http://localhost:4000/delay/http_millis/"++integer_to_list(Delay),[]},[],[]).

delay_millis(Delay) ->
    {ok,{{_,200,_},_,_}} = httpc:request(put,
        {"http://localhost:4000/delay/delay_millis/"++integer_to_list(Delay),[]},[],[]).

ets_bomb() ->
    {ok,{{_,200,_},_,_}} = httpc:request(get,
        {"http://localhost:4000/ets/bomb",[]},[],[]).

ets_defuse() ->
    {ok,{{_,200,_},_,_}} = httpc:request(get,
        {"http://localhost:4000/ets/defuse",[]},[],[]).

process_bomb() ->
    {ok,{{_,200,_},_,_}} = httpc:request(get,
        {"http://localhost:4000/process/bomb",[]},[],[]).

process_defuse() ->
    {ok,{{_,200,_},_,_}} = httpc:request(get,
        {"http://localhost:4000/process/defuse",[]},[],[]).

atom_bomb() ->
    {ok,{{_,200,_},_,_}} = httpc:request(get,
        {"http://localhost:4000/atom/bomb",[]},[],[]).

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
