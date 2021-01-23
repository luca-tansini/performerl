-module(performerl_metrics_collector_agent).

-include_lib("../include/performerl.hrl").

-behaviour(gen_server).

% API exports
-export([start/0,
         stop/0,
         get_metrics_and_stop/0]).

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

-define(COLLECT_METRICS_INTERVAL, 5000).
-record(state, {
    metrics = []
}).

%*******************************************************************************
% API functions
%*******************************************************************************

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

-spec get_metrics_and_stop() ->
    {ok, list({
        timestamp(),
        list({pid(), list({metric_name(), value()})})
    })}.
get_metrics_and_stop() ->
    {ok, Metrics} = gen_server:call(?MODULE, get_metrics, infinity),
    gen_server:stop(?MODULE),
    {ok, lists:reverse(Metrics)}.

%*******************************************************************************
% gen_server callbacks
%*******************************************************************************

init(_) ->
    erlang:send_after(?COLLECT_METRICS_INTERVAL, self(), collect_metrics),
    {ok, #state{}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(get_metrics, _From, State=#state{metrics=Metrics}) ->
    {reply, {ok, Metrics}, State}.

handle_info(collect_metrics, State) ->
    T1 = erlang:system_time(millisecond),
    % TODO: this polling is not ideal, because the discoverer can be flooded
    % and thus taking too long to reply to our polling
    % call process_discoverer for list of active processes
    {ok, Pids} = performerl_process_discoverer_agent:get_active_processes(),
    % collect metrics from active processes and put them the state
    Metrics = collect_metrics(Pids),
    OldMetrics = State#state.metrics,
    NewState = State#state{metrics = [{T1,Metrics}|OldMetrics]},
    T2 = erlang:system_time(millisecond),
    Int = case ?COLLECT_METRICS_INTERVAL - (T2-T1) of
        I when I < 500 ->
            % force a small interval
            500;
        I -> I
    end,
    erlang:send_after(Int, self(), collect_metrics),
    {noreply, NewState};

handle_info(Info, State) ->
    io:format("got unexpected message: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%*******************************************************************************
% internal functions
%*******************************************************************************

collect_metrics(Pids) -> collect_metrics(Pids, []).

collect_metrics([], Metrics) -> Metrics;
collect_metrics([Pid|Pids], Metrics) ->
    Mem = erlang:process_info(Pid, memory),
    Reds = erlang:process_info(Pid, reductions),
    case (Mem == undefined) or (Reds == undefined) of
        true -> collect_metrics(Pids, Metrics);
        false -> collect_metrics(Pids, [{Pid, [Mem, Reds]}|Metrics])
    end.
