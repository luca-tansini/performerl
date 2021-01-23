-module(performerl_load_generator).

-include_lib("../include/performerl.hrl").

-type trace_pattern() :: {module()|'_',atom()|'_',non_neg_integer()|'_'}.
-type run_info() :: term().
-type test_info() :: term().

-callback get_test_name() -> string().

% weird naming but 'setup_test' caused problems with eunit
-callback test_setup() -> {ok, test_info()}.

-callback setup_run(Size :: test_size()) -> {run_started, [node()]}.

-callback start_load(TestNodes :: [node()], Size :: test_size()) -> 
                {load_started, run_info()} | {already_started, pid()}.

-callback get_load_duration() -> pos_integer().

% get the sizes for the runs and a label suggesting what the size refers to
-callback get_test_sizes() -> {atom(), [test_size()]}.

-callback stop_load(RunInfo :: run_info()) ->
                {load_stopped, run_info()} | {error, not_started}.

-callback teardown_run(RunInfo :: run_info()) -> run_ended.

-callback test_teardown(TestInfo :: test_info()) -> ok.

% trace patterns can contain the matchall atom (i.e.:  '_')
% only in the function or arity fields, and only if they don't
% clash with the profiler's RESERVED_PATTERNS
-callback get_trace_patterns() -> [trace_pattern()].
