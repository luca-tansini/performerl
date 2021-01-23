-module(performerl_lib).

-export([format_date/1,
         get_timestamp/0,
         sleep_progress/1,
         inject_module/2,
         rpc/2]).

-define(WIDTH, 60).

get_timestamp() ->
    calendar:local_time().

format_date(Timestamp) ->
    case Timestamp of
        {{Y,M,D},{H,Mm,S}} ->
            io_lib:format("~2..0B-~2..0B-~4..0B.~2..0B:~2..0B:~2..0B",[D,M,Y,H,Mm,S])
    end.

sleep_progress(Millis) ->
    io:format("[>"++lists:duplicate(?WIDTH-1,$\s)++"]"),
    sleep_progress(ceil(Millis div ?WIDTH),1).

sleep_progress(Millis,?WIDTH) -> 
    timer:sleep(Millis),
    io:format(lists:flatten(lists:duplicate(?WIDTH+2,"\e[D"))),
    io:format("["
        ++lists:duplicate(?WIDTH,$=)
        ++"]~n");
sleep_progress(Millis,N) ->
    timer:sleep(Millis),
    io:format(lists:flatten(lists:duplicate(?WIDTH+2,"\e[D"))),
    io:format("["
        ++lists:duplicate(N,$=)
        ++">"
        ++lists:duplicate(?WIDTH-1-N,$\s)
        ++"]"),
    sleep_progress(Millis,N+1).

rpc(Node, CodeStr) ->
    {ok, Tokens, _} = erl_scan:string(CodeStr),
    {ok, Forms} = erl_parse:parse_exprs(Tokens),
    {value, Res, _} = rpc:call(Node, erl_eval, exprs, [Forms,[]]),
    Res.

inject_module(Module, Node) ->
    {Module, ModBin, _} = code:get_object_code(Module),
    case rpc:call(Node, code, load_binary, 
             [Module, atom_to_list(Module)++".erl", ModBin]) of
        {module, Module} -> ok;
        {error, Reason} -> {error, Reason}
    end.
