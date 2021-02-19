performerl_cowboy
=====

Sample app to display PerformErl functionality.
A dummy cowboy web server application.

Build
-----

    $ rebar3 release

Run PerformErl tests
-----

    $ rebar3 performerl test/performerl/<loag_generator_module.erl>

Run a test with custom agents:

    $ rebar3 performerl test/performerl/<loag_generator_module.erl> -C <custom_agent1_module.erl>,<custom_agent2_module.erl>