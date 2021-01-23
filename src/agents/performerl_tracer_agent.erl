-module(performerl_tracer_agent).

-include_lib("../include/performerl.hrl").

% This gen_server is injectced into the test nodes and takes care of all the
% tracing needed by the profiling processes.

% - This module uses meta-tracing for the most part, but it will need to set itself as an actual tracer for all processes in the beginning, because the call_time tracing, which is done by this same process, needs a tracer process to work. But still, this means that we don't actually need to enforce that we are the tracer, we set it in the beginning and if someone steals it from us it should still be fine (I wrote tests to make sure that when the tracer process changes, call_time counters are not restarted). What happens then if the new tracer dies? We have to immediately reset ourserlves as tracers, so that we don't miss call_time traced calls. In order to do this we have to meta-trace the tracing BIFs erlang:trace/3 and erlang:trace_pattern/3 and when we let someone steal the tracing from us we have to monitor them and store somewhere which set of processes they are going to trace, so that if and when they die, we can immediately replace them as tracers.

% - CODE LOADING: PerofrmErl profiling infrastructure, of which this module is a core part of, is injected into the test nodes and started before the workload is started. This means that if the load requires injecting modules that include some of the tracing patterns we won't find them as soon as the profiler starts, and so we need to trace the erlang:load_module function in order to try and set the tracing flags again as soon as the interested modules have been loaded.

% - The only reserved tracing patterns that we need are:
%   - meta-tracing of erlang:load_module function (with return_from flag)
%   - meta-tracing of tracing BIFs
%   if anyone tries to set themselves as meta-tracer for any of these patterns, %   we cannot allow it and we will abort the test. This is highly unlikely.
% - What we need to check for with the tracing BIFs is:
%   RELATED TO META TRACING:
%       - nobody should steal meta-tracing of the reserved patterns from us
%   RELATED TO CALL_TIME:
%       - nobody should do anything that causes our call_time counters to be
%         lost or reset, if they do, we abort
%       - keep track of who steals the tracing and for which processes, so that %         we can set it back if they die

% this module can actually take care of the call_time tracing itself, we don't need as many functionalities as eprof.

% what happens if the user requests any of the RESERVED PATTERNS to be traced? We cannot let the discoverer take over the meta_tracing, but we could forward him the messages. Is it worth the complexity though? It would probably be cleaner to just filter them out.

% the trace_pattern calls made from the process_discoverer should of course be ignored

-behaviour(gen_server).

% API exports
-export([start/1,
         get_results_and_stop/0]).

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
-define(RESERVED_PATTERNS, [{erlang,trace,3},
                            {erlang,trace_pattern,3},
                            {erlang,load_module,2}]).

-record(state, {
    patterns = [],      % contains the (filtered) user specified patterns
    tracers = [],       % contains info about the tracers that stole the tracing
                        % from us, tracers entries are {Tracer,Target,MonRef}
    discoverer_pid      % the pid of the process_discoverer process
}).

%*******************************************************************************
% API functions
%*******************************************************************************

start(Config) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Config, []).

get_results_and_stop() ->
    {ok, Res} = gen_server:call(?MODULE, get_results, infinity),
    gen_server:stop(?MODULE),
    {ok, Res}.

%*******************************************************************************
% gen_server callbacks
%*******************************************************************************

init(Config) ->
    Patterns = maps:get(patterns, Config),
    Filtered = filter_reserved_patterns(Patterns),
    NewConfig = Config#{patterns => Filtered},
    ok = meta_trace_reserved_patterns(),
    ok = set_process_trace(processes, true),
    ok = set_call_time_tracing(Filtered, true),
    % start process_discoverer and store pid, not expected to change
    {ok, Pid} = performerl_process_discoverer_agent:start_link(NewConfig),
    % start metrics collector
    performerl_metrics_collector_agent:start(),
    {ok, #state{patterns=Filtered, discoverer_pid=Pid}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(get_results, _From, State=#state{patterns=Patterns}) ->
    % pause all call_time counters
    set_call_time_tracing(Patterns, pause),
    Res = get_call_time_results(Patterns),
    {reply, {ok, Res}, State};

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

%*******************************************************************************
% handle_info
%*******************************************************************************

% loaded_module
handle_info(
        {trace_ts, _, return_from, {erlang, load_module, 2}, {module, Mod}, _},
        State=#state{patterns=Patterns, discoverer_pid=DiscPid}) ->
    % set call_time flag for all user defined patterns in the loaded module
    LoadedPatterns = lists:foldl(fun({M,F,A}, Acc) ->
                case M == Mod of
                    true ->
                        ok = set_call_time_tracing([{M,F,A}], true),
                        [{M,F,A}|Acc];
                    false -> 
                        Acc
                end
            end,
            [],
            Patterns),
    case LoadedPatterns of
        [] -> ok;
        _ ->
            % notify the process_discoverer
            send(DiscPid, {patterns_loaded, LoadedPatterns})
    end,
    {noreply, State};

% ignore calls, we only want return_from
handle_info({trace_ts, _, call, {erlang, load_module, _}, _},State) ->
    {noreply, State};

handle_info({trace_ts, Pid, call, {erlang, trace, Args}, _},
            State=#state{tracers=Tracers}) ->
    NewTracers = handle_trace_call(Pid,Args,Tracers),
    {noreply, State#state{tracers=NewTracers}};

% ignore trace message generated by the process_discoverer
handle_info({trace_ts, DiscPid, call, {erlang, trace_pattern, _Args}, _},
            State=#state{discoverer_pid=DiscPid}) ->
    {noreply,State};
handle_info({trace_ts, _Pid, call, {erlang, trace_pattern, Args}, _},
            State=#state{patterns=Patterns}) ->
    handle_trace_pattern_call(Args, Patterns),
    {noreply, State};

handle_info({'DOWN', _Ref, _, DeadTracer, _}, State=#state{tracers=Tracers}) ->
    % one of the tracers died, if it's still in the Tracers list we have to
    % remove it and take back tracing for the Targets it was tracing
    case is_known_tracer(DeadTracer, Tracers) of
        {true, Targets} ->
            [ok = set_process_trace(Target, true) || Target <- Targets],
            NewTracers = lists:filter(fun({Tracer,_,_}) -> 
                                        Tracer =/= DeadTracer
                                      end, Tracers),
            {noreply, State#state{tracers=NewTracers}};
        false ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    io:format("got unexpected message: ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, #state{patterns=_Patterns}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%*******************************************************************************
% internal functions
%*******************************************************************************
% if the flag is false, someone removed something. 
% We check the Tracers list to see if the Target was in there:
% - if it is then it means it's someone we know cleaning up after they finished
%   so we can remove them from the Tracers list, remove the monitor and set
%   ourselves as tracers again
% - otherwise it means they're preparing to trace (like Wombat does),
%   in this case we can assume they called trace with true right away.
handle_trace_call(_Caller, [Target,false,_Opts], Tracers) ->
    case is_known_target(Target, Tracers) of
        {true, {Tracer,Target,MonRef}} ->
            % remove the monitor and reset tracing for the targets
            ok = set_process_trace(Target, true),
            erlang:demonitor(MonRef),
            lists:delete({Tracer,Target,MonRef}, Tracers);
        false ->
            % we assume they're just preparing like wombat or
            % we already got the monitor down message
            Tracers
    end;

% we assume the call was successful, otherwise the test is failing anyway
% we allow someone else to take the tracing, as long as they set the call flag
handle_trace_call(Caller, [Target,true,Opts], Tracers) ->
    case has_call_flag(Opts) of
        false ->
            error_logger:error_report(
                io_lib:format("someone stole tracing without "++
                              "setting the call flag:~n"++
                              "Caller: ~p, Target: ~p, Opts: ~p",
                              [Caller, Target, Opts])),
            % abort
            throw(bad_trace);
        true ->
            Tracer = get_tracer(Caller, Opts),
            MonRef = erlang:monitor(process,Tracer),
            [{Tracer,Target,MonRef}|Tracers]
    end.

is_known_target(Target, [X={_,Target,_}|_]) -> {true,X};
is_known_target(Target, [_|Tracers]) -> is_known_target(Target, Tracers);
is_known_target(_, []) -> false.

is_known_tracer(Tracer, Tracers) -> is_known_tracer(Tracer, Tracers, []).
is_known_tracer(_, [], []) -> false;
is_known_tracer(_, [], Targets) -> {true, Targets};
is_known_tracer(Tracer, [{Tracer,Target,_}|TL], Targets) ->
    is_known_tracer(Tracer, TL, [Target | Targets]);
is_known_tracer(Tracer, [_|TL], Targets) ->
    is_known_tracer(Tracer, TL, Targets).

% the tracer could be different from the caller
get_tracer(Caller, []) -> Caller;
get_tracer(_Caller, [{tracer,Pid}|_]) -> Pid;
get_tracer(_Caller, [{tracer,Pid,_}|_]) -> Pid;
get_tracer(Caller, [_|TL]) -> get_tracer(Caller,TL).

has_call_flag([]) -> false;
has_call_flag([call|_]) -> true;
has_call_flag([_|TL]) -> has_call_flag(TL).

% bad things that can happen are:
% - someone steals/disables meta tracing of reserved or user patterns --> abort
% - someone resets/disables call_time for one of the user patterns,
%   which will cause the counter to be restarted --> abort
handle_trace_pattern_call([{erlang,trace_pattern,3}, _Flag, [call_time]], _) ->
    io:format("DEBUGGING...\n"),
    ok;
handle_trace_pattern_call([Pattern, Flag, Opts], UserPatterns) ->
    IsBadPattern = is_bad_pattern(Pattern, UserPatterns++?RESERVED_PATTERNS),
    HasBadOpt = has_bad_opts(Opts),
    case IsBadPattern andalso HasBadOpt of
        true ->
            error_logger:error_report(
                io_lib:format("someone made a call to trace_pattern "++
                              "we cannot work with:~n"++
                              "Pattern: ~p, Flag: ~p, Opts: ~p",
                              [Pattern, Flag, Opts])),
            % abort
            throw(bad_trace_pattern);
        false -> ok
    end.

has_bad_opts([]) -> false;
has_bad_opts([O|Opts]) ->
    case O of
        call_time -> true;
        global -> true;
        meta -> true;
        {meta, _} -> true;
        {meta, _, _} -> true;
        _ -> has_bad_opts(Opts)
    end.

is_bad_pattern(on_load,_) -> true;
is_bad_pattern(_, []) -> false;
is_bad_pattern(Pattern, [Rp|Reserved]) ->
    case patterns_overlap(Pattern, Rp) of
        true -> true;
        false -> is_bad_pattern(Pattern, Reserved)
    end.

patterns_overlap(P1, P2) ->
    compare_patterns(P1, P2) orelse compare_patterns(P2,P1).

% this is not symmetrical! Only the first argument can contain the matchall atom
compare_patterns({'_',_,_}, _) -> true;
compare_patterns({Mod,'_',_}, {Mod,_,_}) -> true;
compare_patterns({Mod,Fun,'_'}, {Mod,Fun,_}) -> true;
compare_patterns({Mod,Fun,A}, {Mod,Fun,A})-> true;
compare_patterns(_,_) -> false.

set_call_time_tracing(Patterns, MS) ->
    Ret = try
        erlang:system_flag(multi_scheduling, block),
	    [erlang:trace_pattern(P, MS, [call_time]) || P <- Patterns],
        ok
    catch
	    _:Reason -> {error, Reason}
    end,
    erlang:system_flag(multi_scheduling, unblock),
    Ret.

% it would be best if they all had return_from, but the problem is that for some
% of this patterns we need to do something based on the arguments
meta_trace_reserved_patterns() ->
    Ret = try
        erlang:system_flag(multi_scheduling, block),
        % functions for module loading
        erlang:trace_pattern({erlang,load_module,2},
                             [{'_',[],[{return_trace}]}], [meta]),
        % functions for target system tracing needs
        erlang:trace_pattern({erlang,trace,3}, true, [meta]),
        erlang:trace_pattern({erlang,trace_pattern,3}, true, [meta]),
        ok
    catch
        _:Reason -> {error,Reason}
    end,
    erlang:system_flag(multi_scheduling, unblock),
    Ret.

% we should only use the 'processes' flag during the init and teardown
set_process_trace(Target, Flag) ->
    Ret = try
        erlang:system_flag(multi_scheduling, block),
	    erlang:trace(Target, Flag, [call]),
        ok
    catch
	    _:Reason -> {error, Reason}
    end,
    erlang:system_flag(multi_scheduling, unblock),
    Ret.

filter_reserved_patterns(Patterns) ->
    Filtered = lists:filter(
        fun(P) ->
            not lists:any(
                fun(Rp) -> compare_patterns(P,Rp) end,
                ?RESERVED_PATTERNS)
        end,
        Patterns),
    Len = length(Patterns),
    case length(Filtered) of
        0 ->
            error_logger:error_report(
                io_lib:format("no valid user pattern was specified, "++
                              "terminating...", [])),
            % abort
            throw(no_valid_user_pattern);
        Len ->
            Filtered;
        _ ->
            % some user patterns were removed
            Filtered
    end.

get_call_time_results(Patterns) ->
    get_call_time_results(Patterns, []).

get_call_time_results([], Res) -> Res;
% we assume the user doesn't specify overlapping patterns
get_call_time_results([P|Patterns], Res) ->
    R = get_call_time_result(P),
    get_call_time_results(Patterns, R++Res).

get_call_time_result({M,F,A}) ->
    case code:is_loaded(M) of
        false -> [];
        _ -> get_call_time_result2({M,F,A})
    end.
get_call_time_result2({M,'_','_'}) ->
    [begin
        PerProcSeconds = convert_to_seconds(PerProc),
        {Count,Tot} = get_totals(PerProcSeconds),
        {{M,F,A},Count,Tot,PerProcSeconds}
     end ||
     {F,A} <- M:module_info(functions),
     {_, PerProc} <- [erlang:trace_info({M,F,A}, call_time)], PerProc=/=[]];
get_call_time_result2({M,F,'_'}) ->
    [begin
        PerProcSeconds = convert_to_seconds(PerProc),
        {Count,Tot} = get_totals(PerProcSeconds),
        {{M,F,A},Count,Tot,PerProcSeconds}
     end ||
     {F1,A} <- M:module_info(functions), F == F1,
     {_, PerProc} <- [erlang:trace_info({M,F1,A}, call_time)], PerProc=/=[]];
get_call_time_result2({M,F,A}) ->
    {call_time, PerProc} = erlang:trace_info({M,F,A}, call_time),
    case PerProc of
        [] -> [];
        _ ->
        PerProcSeconds = convert_to_seconds(PerProc),
        {Count,Tot} = get_totals(PerProcSeconds),
        [{{M,F,A},Count,Tot,PerProcSeconds}]
    end.

get_totals(L) ->
    get_totals(L,0,0).
get_totals([], Count, Time) -> {Count, Time};
get_totals([{_,C,Time}|TL], Count, Tot) ->
    get_totals(TL, Count+C, Tot+Time).

convert_to_seconds(PerProc) ->
    lists:map(fun({Pid, Count, S, Us}) ->
            {Pid, Count, S*1000000+Us}
        end, PerProc).

send(Pid, Msg) when is_pid(Pid) -> Pid ! Msg;
send(Name, Msg) ->
    case whereis(Name) of
        undefined -> ignored;
        Pid -> Pid ! Msg
    end.