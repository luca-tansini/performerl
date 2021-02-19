-module(invariant_checker_custom_agent).

-behaviour(gen_server).
-behaviour(performerl_custom_agent).

% custom PerformErl agent exports
-export([get_config/3, start/1,
         get_results_and_stop/0,
         process_results/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {
    history = [] :: list(),
    % invariants must be in the form
    % {Name,MFA,Operator,DefaultValue}
    % where operator is one of {'==','>=','=<'}
    invariants :: list(),
    check_interval :: integer
}).

%*******************************************************************************
% API functions
%*******************************************************************************

start(Config) ->
    InitState = #state{
        check_interval = maps:get(check_interval, Config),
        invariants = maps:get(invariants, Config)
    },
    gen_server:start({local, ?MODULE}, ?MODULE, InitState, []).

get_results_and_stop() ->
    {ok, ResHist} = gen_server:call(?MODULE, get_results, infinity),
    gen_server:stop(?MODULE),
    {ok, lists:reverse(ResHist)}.

get_config(TestNode, LoadModule, TestSize) ->
    % this custom agent gets its config parameters
    % from the LoadModule written by the user
    #{ check_interval => round(LoadModule:get_load_duration()/100),
       invariants => LoadModule:get_invariants(TestNode, TestSize)
    }.

process_results(TestNode, ResHist) ->
    {TotalChecks, InvResList} = process_results0(ResHist),
    io:format("Invariants were tested ~p times on test node:~p~n",
              [TotalChecks, TestNode]),
    lists:foreach(
        fun({InvName,V}) ->
            io:format("  -invariant ~p was violated ~p times~n", [InvName, V])
        end,
        InvResList
    ).

%*******************************************************************************
% gen_server callbacks
%*******************************************************************************

init(InitState=#state{check_interval = Interval}) ->
    erlang:send_after(Interval, self(), check_invariants),
    {ok, InitState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(get_results, _From, State=#state{history=ResHist}) ->
    {reply, {ok, ResHist}, State}.

handle_info(check_invariants, State=#state{history=ResHist,
                                           check_interval=Int,
                                           invariants=InvList}) ->
    Res = check_invariants(InvList),
    erlang:send_after(Int, self(), check_invariants),
    {noreply, State#state{history=[Res|ResHist]}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%*******************************************************************************
% internal functions
%*******************************************************************************

check_invariants(InvList) ->
    lists:map(
        fun({Name, MFA, Op, DefValue}) ->
            {Name, check_invariant(MFA, Op, DefValue)}
        end,
        InvList
    ).

check_invariant({M,F,Args}, Op, DefValue) ->
    Value = erlang:apply(M,F,Args),
    case test(Value, Op, DefValue) of
        true  -> ok;
        false -> {violated, Value}
    end.

test(A, '==', B) -> A == B;
test(A, '=<', B) -> A =< B;
test(A, '>=', B) -> A >= B.

% function that takes the history and counts
% the total number of invariant checks and
% how many times each invariant failed
process_results0(ResHist) ->
    {TotalChecks, InvResMap} = lists:foldl(
        fun(InvResList, {N, Map}) ->
            NewMap = lists:foldl(
                fun({InvName, Res}, Map1) ->
                    case Res =/= ok of
                        true ->
                            maps:update_with(InvName,
                                             fun(M) -> M+1 end, 1, Map1);
                        false ->
                            Map1
                    end
                end,
                Map,
                InvResList
            ),
            {N+1, NewMap}
        end,
        {0, maps:new()},
        ResHist
    ),
    {TotalChecks, maps:to_list(InvResMap)}.
