-module(performerl_custom_agent).

-include_lib("../include/performerl.hrl").

%*******************************************************************************
% functions called locally in the PerformErl module
%*******************************************************************************
-callback get_config(TestNode :: node(),
                     LoadModule :: module(),
                     TestSize :: test_size()) -> map().

-callback process_results(TestNode :: node(), Results :: term()) ->
    ProcessedResults :: term().

%*******************************************************************************
% functions called remotely in the test nodes
%*******************************************************************************
-callback start(Config :: map()) ->
    {ok, Pid :: pid()} | ignore | {error, Error :: term()}.

-callback get_results_and_stop() -> {ok, Results :: term()}.
