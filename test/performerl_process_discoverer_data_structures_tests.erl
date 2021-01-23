-module(performerl_process_discoverer_data_structures_tests).

-include_lib("eunit/include/eunit.hrl").

-define(N_REPS, 1000000).
-define(MAX_SIZE, 20).

%******************************************************************************%
% throughout this experiment the PIDs are approximated by integers. In terms of
% memory it should be the same (1 word each) but for speed (comparison,
% insertion, lookup) there's a slowdown factor of around 1.4 when using PIDs
% instead of integers. The whole thing could be replaced with list_to_pid
%*******************************************************************************

% dict_mem_test_() ->
%     {
%         timeout,
%         6000,
%         fun() ->
%             R = [ dict_mem_benchmark(round(math:pow(2,Size))) ||
%               Size <- lists:seq(10,?MAX_SIZE)],
%             ?debugFmt("~ndict mem test results:~n~p~n",[R])
%         end
%     }.

% dict_speed_test_() ->
%     {
%         timeout,
%         6000,
%         fun() ->
%             R = [ dict_speed_benchmark(round(math:pow(2,Size))) ||
%               Size <- lists:seq(10,?MAX_SIZE)],
%             ?debugFmt("~ndict speed test results:~n~p~n",[R])
%         end
%     }.

gb_sets_mem_test_() ->
    {
        timeout,
        6000,
        fun() ->
            R = [ gb_sets_mem_benchmark(round(math:pow(2,Size))) ||
              Size <- lists:seq(10,?MAX_SIZE)],
            ?debugFmt("~ngb_sets mem test results:~n~p~n",[R])
        end
    }.

gb_sets_speed_test_() ->
    {
        timeout,
        6000,
        fun() ->
            R = [ gb_sets_speed_benchmark(round(math:pow(2,Size))) ||
              Size <- lists:seq(10,?MAX_SIZE)],
            ?debugFmt("~ngb_sets speed test results:~n~p~n",[R]),
            print_chart_series(R)
        end
    }.

maps_mem_test_() ->
    {
        timeout,
        6000,
        fun() ->
            R = [ maps_mem_benchmark(round(math:pow(2,Size))) ||
              Size <- lists:seq(10,?MAX_SIZE)],
            ?debugFmt("~nmaps mem test results:~n~p~n",[R])
        end
    }.

maps_speed_test_() ->
    {
        timeout,
        6000,
        fun() ->
            R = [ maps_speed_benchmark(round(math:pow(2,Size))) ||
              Size <- lists:seq(10,?MAX_SIZE)],
            ?debugFmt("~nmaps speed test results:~n~p~n",[R]),
            print_chart_series(R)
        end
    }.

%*******************************************************************************
% Internal Functions
%*******************************************************************************

% dict_mem_benchmark(Size) ->
%     D = build_dict(Size),
%     Words = erts_debug:flat_size(D),
%     Bytes = Words * erlang:system_info(wordsize),
%     {Size, {bytes, Bytes},
%            {words_per_entry, Words / Size}}.

% dict_speed_benchmark(Size) ->
%     % build the dictionary with Size entries
%     Dict = build_dict(Size),
%     % lookup speed bench: take the average time for Size lookup operations
%     % (each with a different key)
%     LookupTime = lookup(Dict, fun dict:is_key/2, Size),
%     % insertion and deletion must be done at the same time
%     % (to avoid the structure growing)
%     {DeleteTime, InsertTime} =
%         delete_insert(Dict,
%                       fun dict:erase/2,
%                       fun(K, D) ->
%                             dict:store(K, {inactive,{module, func, K}}, D) end,
%                       Size),
%     {Size, [{lookup, LookupTime},
%             {delete, DeleteTime},
%             {insert, InsertTime}]}.

gb_sets_speed_benchmark(Size) ->
    % build the gb_set with Size entries
    GBSet = build_gb_set(Size),
    % lookup speed bench: take the average time for Size lookup operations
    % (each with a different key)
    LookupTime = lookup(GBSet, fun gb_sets:is_member/2, Size),
    % insertion and deletion must be done at the same time
    % (to avoid the structure growing too much)
    {DeleteTime, InsertTime} =
        delete_insert(GBSet,
                      fun gb_sets:delete/2,
                      fun(K, S) ->
                            gb_sets:insert(K, S) end,
                      Size),
    {Size, [{lookup, LookupTime},
            {delete, DeleteTime},
            {insert, InsertTime}]}.

gb_sets_mem_benchmark(Size) ->
    S = lists:foldl(
        fun(N, Acc) -> gb_sets:insert(N, Acc) end,
        gb_sets:new(),
        lists:seq(1, Size)
    ),
    Words = erts_debug:flat_size(S),
    Bytes = Words * erlang:system_info(wordsize),
    {Size, {bytes, Bytes},
           {words_per_entry, Words / Size}}.

maps_mem_benchmark(Size) ->
    M = build_map(Size),
    Words = erts_debug:flat_size(M),
    Bytes = Words * erlang:system_info(wordsize),
    {Size, {bytes, Bytes},
           {words_per_entry, Words / Size}}.

maps_speed_benchmark(Size) ->
    % build the Map with Size entries
    Map = build_map(Size),
    % lookup speed bench: take the average time for Size lookup operations
    % (each with a different key)
    LookupTime = lookup(Map, fun maps:is_key/2, Size),
    % insertion and deletion must be done at the same time
    % (to avoid the structure growing too much)
    {DeleteTime, InsertTime} =
        delete_insert(Map,
                      fun maps:remove/2,
                      fun(K, M) ->
                            maps:put(K, {inactive,{module, func, K}}, M) end,
                      Size),
    {Size, [{lookup, LookupTime},
            {delete, DeleteTime},
            {insert, InsertTime}]}.

% build_dict(Size) ->
%     Pids = gen_keys(Size),
%     lists:foldl(
%         fun(Pid, Acc) -> 
%             dict:store(Pid, {inactive, {module, func, Pid}}, Acc)
%         end,
%         dict:new(),
%         Pids
%     ).

build_gb_set(Size) ->
    Pids = gen_keys(Size),
    lists:foldl(
        fun(Pid, Acc) -> 
            gb_sets:insert(Pid, Acc)
        end,
        gb_sets:new(),
        Pids
    ).

build_map(Size) ->
    Pids = gen_keys(Size),
    lists:foldl(
        fun(Pid, Acc) -> 
            maps:put(Pid, {inactive, {module, func, Pid}}, Acc)
        end,
        maps:new(),
        Pids
    ).

lookup(Struct, LookupFun, Size) ->
    Keys = get_n_keys(?N_REPS, Size),
    Total = lists:foldl(
        fun(K, Acc) ->
            {T, true} = timer:tc(LookupFun, [K, Struct]),
            Acc + T
        end,
        0,
        Keys
    ),
    Total / ?N_REPS.

delete_insert(Struct, DeleteFun, InsertFun, Size) ->
    Keys = get_n_keys(?N_REPS, Size),
    {TotalD, TotalI} = lists:foldl(
        fun(K, {AccD, AccI}) ->
            {TD, S1} = timer:tc(DeleteFun, [K, Struct]),
            {TI, _}  = timer:tc(InsertFun, [K, S1]),
            {AccD + TD, AccI+TI}
        end,
        {0, 0},
        Keys
    ),
    {TotalD/?N_REPS, TotalI/?N_REPS}.

% generates N keys out of a space ranging from [1,Size]
% randomly sorted within each [1,Size] period
% (repeats them if needed)
get_n_keys(N, Size) ->
    get_n_keys(N, Size, 0, []).
get_n_keys(N, _Size, Count, Acc) when Count >= N  ->
    lists:sublist(Acc, N);
get_n_keys(N, Size, Count, Acc) ->
    get_n_keys(N, Size, Count+Size, gen_keys(Size)++Acc).

gen_keys(Size) ->
    %% The trick is to generate all numbers as usual, but match them
    %% with a random value in a tuple of the form {Random, Number}.
    %% The idea is to then sort the list generated that way; done in
    %% this manner, we know all values will be unique and the randomness
    %% will be done by the sorting.
    Random = lists:sort([{rand:uniform(Size), X} || X <- lists:seq(1, Size)]),
    [X || {_, X} <- Random].

%*******************************************************************************
% Functions to print the results series in a HighCharts JS fashion
%*******************************************************************************

print_chart_series(L) ->
    print_chart_series(L, [], maps:new()).
print_chart_series([], Categories, Map) ->
    ?debugFmt("~ncategories: ~p~n", [Categories]),
    [ ?debugFmt("~n{name: \'~p\',\n data: ~p}~n",
                [K, maps:get(K, Map)]) ||
      K <- maps:keys(Map)];
print_chart_series([{Cat, Data} | TL], Categories, Map) ->
    NewMap = lists:foldl(
        fun({K, V}, D) ->
            maps:update_with(K,fun(L) -> L++[V] end, [V], D)
        end,
        Map,
        Data
    ),
    print_chart_series(TL, Categories++[Cat], NewMap).