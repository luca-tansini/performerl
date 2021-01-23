-module(code_loading_test).

-include_lib("eunit/include/eunit.hrl").

% the ways to load code are at least:
% - erlang:load_module/2
% - code:load_abs/1
% - code:load_binary/3
% - code:load_file/1
% but do the second ones always call erlang:load_module inside? Let's find out!
% Yes! Seems like the process of loading a module always has to go through the erlang:load_module call, just in the case of code:load_*** functions, it's done by the code sever, and not by the calling process.

trace_load_module_test() ->
    Self = self(),
    Bin = create_dummy_mod(binary),
    Tracer = spawn_tracer(),
    timer:sleep(10),
    erlang:load_module(dummy_module, Bin),
    hello = dummy_module:hello(),
    ok = receive
        {trace_ts, Self, call, {erlang,load_module, _}, _} -> ok
    after 1000 ->
        ?assert(got_nothing)
    end,
    Tracer ! stop,
    delete_dummy_mod().

code_load_binary_test() ->
    Bin = create_dummy_mod(binary),
    Tracer = spawn_tracer(),
    timer:sleep(10),
    {module, dummy_module} = code:load_binary(dummy_module, "", Bin),
    hello = dummy_module:hello(),
    CodeServerPid = whereis(code_server),
    ok = receive
        {trace_ts, CodeServerPid, call, {erlang,load_module, _}, _} -> ok
    after 1000 ->
        ?assert(got_nothing)
    end,
    Tracer ! stop,
    delete_dummy_mod().

code_load_file_test() ->
    create_dummy_mod(),
    Tracer = spawn_tracer(),
    timer:sleep(10),
    {module, dummy_module} = code:load_file(dummy_module),
    hello = dummy_module:hello(),
    CodeServerPid = whereis(code_server),
    ok = receive
        {trace_ts, CodeServerPid, call, {erlang,load_module, _}, _} -> ok
    after 1000 ->
        ?assert(got_nothing)
    end,
    Tracer ! stop,
    delete_dummy_mod().

code_load_abs_test() ->
    create_dummy_mod(),
    Tracer = spawn_tracer(),
    timer:sleep(10),
    {module, dummy_module} = code:load_abs("dummy_module"),
    hello = dummy_module:hello(),
    CodeServerPid = whereis(code_server),
    ok = receive
        {trace_ts, CodeServerPid, call, {erlang,load_module, _}, _} -> ok
    after 1000 ->
        ?assert(got_nothing)
    end,
    Tracer ! stop,
    delete_dummy_mod().

%*******************************************************************************
% internal functions
%*******************************************************************************

create_dummy_mod() ->
    {ok, Fd} = file:open("dummy_module.erl", [write]),
    ok = io:format(Fd,"-module(dummy_module).~n"++
                      "-export([hello/0]).~n"++
                      "hello() -> hello.~n", []),
    ok = file:close(Fd),
    {ok, dummy_module} = compile:file("dummy_module.erl").
create_dummy_mod(binary) ->
    {ok, Fd} = file:open("dummy_module.erl", [write]),
    ok = io:format(Fd,"-module(dummy_module).~n"++
                      "-export([hello/0]).~n"++
                      "hello() -> hello.~n", []),
    ok = file:close(Fd),
    {ok, dummy_module, Bin} = compile:file("dummy_module.erl", [binary]),
    Bin.

delete_dummy_mod() ->
    code:purge(dummy_module),
    code:delete(dummy_module),
    code:purge(dummy_module),
    file:delete("dummy_module.beam"),
    ok = file:delete("dummy_module.erl").

spawn_tracer() ->
    Self = self(),
    spawn_link(fun()->
        erlang:trace_pattern({erlang, load_module, 2}, true, [meta]),
        tracer_loop(Self)
    end).

tracer_loop(Self) ->
    receive
        stop -> ok;
        Msg -> 
            Self ! Msg,
            tracer_loop(Self)
    end.