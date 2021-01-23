-module(performerl_process_discoverer_agent).

-include_lib("../include/performerl.hrl").

-behaviour(gen_server).

% API exports
-export([start_link/1,
         stop/0,
         get_active_processes/0,
         get_discovered_processes/0,
         get_results_and_stop/0]).

-export([create_pid_ms/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-ifdef(TEST).
    -compile([export_all]).
-endif.

-define(DEFAULT_MS_TYPE, tree).
-define(META_FLAG, {meta, performerl_custom_meta_tracer, self()}).

-record(state, {
    patterns = [],
    ms_type = tree,
    discovered_procs = maps:new(),  % map with discovered Pid => name
    active_procs = gb_sets:new(),   % gb_set of active pids to use in the MS
    max_active = 0                  % count of the highest number of
                                    % concurrently active processes
}).

%*******************************************************************************
% API functions
%*******************************************************************************

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

stop() ->
    gen_server:stop(?MODULE).

get_results_and_stop() ->
    {ok, Res} = gen_server:call(?MODULE, get_results, infinity),
    gen_server:stop(?MODULE),
    {ok, Res}.

get_discovered_processes() ->
    gen_server:call(?MODULE, get_discovered, infinity).

get_active_processes() -> gen_server:call(?MODULE, get_active, infinity).

-ifdef(TEST).
    block() -> gen_server:cast(?MODULE, block).
-endif.

%*******************************************************************************
% gen_server callbacks
%*******************************************************************************

init(Config) ->
    Patterns = maps:get(patterns, Config),
    MSType = maps:get(ms_type, Config, ?DEFAULT_MS_TYPE),
    % try to set meta-tracing, but won't work for modules not yet loaded
    [erlang:trace_pattern(Pattern, true, [?META_FLAG])
     || Pattern <- Patterns],
    {ok, #state{patterns=Patterns, ms_type = MSType}}.

handle_cast(block, State) ->
    receive
        unblock -> ok
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(get_discovered, _From, State=#state{discovered_procs = D}) ->
    {reply, {ok,maps:to_list(D)}, State};
handle_call(get_active, _From, State=#state{active_procs = A}) ->
    {reply, {ok,gb_sets:to_list(A)}, State};
handle_call(get_max_active, _From, State=#state{max_active = Max}) ->
    {reply, {ok, Max}, State};
handle_call(get_results, _From, State=#state{discovered_procs = D,
                                             max_active = Max}) ->
    {reply, {ok,{maps:to_list(D), Max}}, State}.

% handle a custom meta-trace message
% TODO: we're not getting anything from the tracer agent anymore, because
%       the only time it calls traced modules is when it stops, and now we're
%       stopped first, but for the future it could be useful to drop traces
%       coming from that process
handle_info({custom_trace, Pid, MFA},
             State=#state{patterns=Patterns, ms_type = MSType,
                          active_procs = Active, max_active = Max,
                          discovered_procs = Discovered}) ->
    case pid_already_discovered(Pid, Discovered) of
        false ->
            Name = get_process_name(Pid, MFA),
            % if the name is undefined it means that the process died before we
            % could even handle the fact that it was spawned. Likely a worker.
            case Name of
                {inactive, _} ->
                    {noreply, State#state{
                        discovered_procs=maps:put(Pid, Name, Discovered)}};
                _ ->
                    NewActive = gb_sets:insert(Pid, Active),
                    NewMax = case gb_sets:size(NewActive) of
                        N when N > Max -> N;
                        _ -> Max
                    end,
                    MS = create_pid_ms(gb_sets:to_list(NewActive), MSType),
                    [erlang:trace_pattern(Pattern, MS, [?META_FLAG]) || 
                     Pattern <- Patterns],
                    erlang:monitor(process, Pid),
                    {noreply,
                     State#state{
                        max_active = NewMax,
                        active_procs = NewActive,
                        discovered_procs=maps:put(Pid, Name, Discovered)
                    }}
            end;
        _ ->
            % We should just ignore these, as they were probably
            % sent before the new match spec was set.
            {noreply, State}
    end;

% handle a module_loaded message from the tracer
handle_info({patterns_loaded, LoadedPatterns},
            State=#state{ms_type=Type, active_procs=Active}) ->
    MS = create_pid_ms(gb_sets:to_list(Active), Type),
    [erlang:trace_pattern(MFA, MS, [?META_FLAG]) || MFA <- LoadedPatterns],
    {noreply, State};

% handle a monitored process dying
% just remove it from the active processes. No need to force the update of the
% MS, since it will be updated anyway with the next discovered process, and
% the cost of the MS is logarithmic so having a few more processes won't hurt
handle_info({'DOWN', _Ref, _, DeadPid, _},
            State=#state{active_procs = Active}) ->
    {noreply, State#state{active_procs=gb_sets:delete(DeadPid, Active)}};

handle_info(Info, State) ->
    io:format("got unexpected message: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, #state{patterns=Patterns}) ->
    [erlang:trace_pattern(Pattern, false, [?META_FLAG]) || Pattern <- Patterns],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%*******************************************************************************
% match spec building functions
%*******************************************************************************

% TODO: nobody, except for tests, is using the list MS anymore,
%       should we just remove it?
create_pid_ms(_, true) -> true;
create_pid_ms(_, false) -> false;
create_pid_ms(L, list) ->
    create_pid_ms_list(L);
create_pid_ms(L, tree) ->
    create_pid_ms_tree(L).

create_pid_ms_tree(L) ->
    % the tree_to_ms tries to find the pid in the tree, so we need to negate it
    [{'_', [{'not', list_to_ms_tree(L)}], []}].

list_to_ms_tree(L) ->
    Tree = list_to_tree(L),
    tree_to_ms(Tree).

tree_to_ms({}) ->
    false;
tree_to_ms({X}) ->
    {'==', {self}, {const, X}};
tree_to_ms({X,L,R}) ->
    {'orelse',
        {'==', {self}, {const, X}},
        {'andalso',
            {'<', {self}, {const, X}},
            tree_to_ms(L)
        },
        {'andalso',
            {'>', {self}, {const, X}},
            tree_to_ms(R)
        }
    }.

list_to_tree([]) -> {};
list_to_tree(L) ->
    Sorted = lists:usort(L),
    sorted_list_to_tree(Sorted).

sorted_list_to_tree([]) -> {};
sorted_list_to_tree([X]) -> {X};
sorted_list_to_tree(L) ->
    {X,Left,Right} = split(L),
    {X,sorted_list_to_tree(Left),sorted_list_to_tree(Right)}.

split(L) ->
    N = floor(length(L)/2) + 1,
    split(L,N,[]).
split([H|TL],1,Left) -> {H,lists:reverse(Left),TL};
split([H|TL],N,Left) -> split(TL, N-1, [H|Left]).

create_pid_ms_list([]) -> [];
create_pid_ms_list(L) when is_list(L) ->
    Andalso = ['andalso' | [create_pid_match_condition(Pid) || Pid <- L]],
    [{'_', [list_to_tuple(Andalso)], []}].

create_pid_match_condition(Pid) ->
    {'=/=', {self}, {const, Pid}}.

%*******************************************************************************
% internal functions
%*******************************************************************************

get_process_name(Pid, DiscoveryMFA) ->
    case process_info(Pid, registered_name) of
        % if the process is registered under a name return that one
        {registered_name, Name} -> Name;
        [] ->
            % otherwise return the name of the function
            % that triggered his discovery
            DiscoveryMFA;
        % this means that the process is dead before we could even get its name
        undefined -> {inactive, DiscoveryMFA}
    end.

pid_already_discovered(Pid, Discovered) ->
    maps:is_key(Pid, Discovered).
