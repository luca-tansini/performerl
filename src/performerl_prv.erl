-module(performerl_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, performerl).
-define(DEPS, [app_discovery, compile]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 performerl <load_gen_module>"}, % How to use the plugin
            {opts, [
                {custom_agents, $C, "custom_agents", string,
                 "comma separated list of custom agents erlang modules"}
            ]},                   % list of options understood by the plugin
            {short_desc, "run PerformErl performance tests"},
            {desc, "PerformErl - a performance testing framework for Erlang"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(task, Args) of
        undefined -> io:format("usage: rebar3 performerl <path/to/load_gen>~n");
        LoadGenPath ->
            CustomAgents = proplists:get_value(custom_agents, Args),
            do_test(LoadGenPath, CustomAgents)
    end,
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

do_test(LoadGenPath, CustomAgents) ->
    Node = 'performerl@127.0.0.1',
    net_kernel:start([Node, longnames]),
    Node = node(),
    erlang:set_cookie(Node, performerl),
    performerl = erlang:get_cookie(),
    % TODO: add the option to disable tracing impact benchmark
    performerl:test(LoadGenPath, #{benchmark_comp => true,
                                   custom_agents => CustomAgents}).