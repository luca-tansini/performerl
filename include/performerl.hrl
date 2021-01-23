%*******************************************************************************
% Type Declarations
%*******************************************************************************

-type sorted_list(Type) :: list(Type).
-type count() :: non_neg_integer().
% time is always in microseconds
-type time() :: non_neg_integer().
-type proc_name() :: atom() | mfa().
-type raw_per_proc_info() :: list({pid(), count(), time()}).
-type per_proc_info() :: sorted_list({proc_name(), count(), time()}).
-type metric_name() :: atom().
-type value() :: term().
-type timestamp() :: non_neg_integer().

-type test_size() :: non_neg_integer().

-type raw_results() ::
    list({
        % for each test_size
        test_size(),
        list({
            % for each node
            NodeName :: node(),
            {
                CallTime :: list({mfa(), count(), time(), raw_per_proc_info()}),
                Discovered :: list({pid(), proc_name() |
                                           {inactive, proc_name()}
                                }),
                MaxActive  :: non_neg_integer(),
                MetricsHist :: list({timestamp(),
                                     list({pid(),
                                           list({metric_name(), value()})
                                         })
                                   })
            }
        })
    }).

-type results_by_node() ::
    list({
        NodeName :: node(),
        list({
            TestSize :: test_size(),
            node_run_data()
        })
    }).

-record(node_run_data,{
% functions fields
    % map with data for each MFA
    funs_call_time      :: maps:map(mfa(), {count(), time(), per_proc_info()}),
    % list of function names sorted by total call_time
    funs_by_call_time   :: sorted_list(mfa()),
    
% processes fields
    % map with the mapping of proc_names into PIDs
    procs_name_pid     :: maps:map(proc_name(), [pid()]),
    % for each name, list the call_time per process info
    procs_used_funs      :: maps:map(proc_name(),
                                      list({mfa(), count(), time()})),
    % proplist that for each metric name holds a list of the
    % process names, sorted by that metric's peak value
    procs_by_metrics     :: list({metric_name(),
                                  sorted_list({proc_name(),
                                               value()
                                             })
                                }),
    % for each timestamp, for each process, holds a list of all the metrics 
    procs_metric_hist    :: list({timestamp(),
                                  list({pid(),
                                        list({metric_name(), value()})
                                      })
                                }),
    % highest number of concurrently active processes in the run
    max_active :: non_neg_integer()
}).

-type node_run_data() :: #node_run_data{}.

-record(test_results, {
    test_name        :: string(),
    timestamp        :: calendar:datetime(),
    test_sizes       :: [pos_integer()],
    size_label       :: atom(),
    results_by_node  :: results_by_node()
}).
