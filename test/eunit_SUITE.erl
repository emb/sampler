%% A simple wrapper suite to run Eunit tests for this application.
-module(eunit_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(APP, sampler).

all() ->
    [run_eunit].

run_eunit() ->
    [].

run_eunit(_Config) ->
    LibDir = code:lib_dir(?APP),
    Modules = [ list_to_atom(filename:basename(F, ".beam"))
                || F <- filelib:wildcard(filename:join([LibDir, "ebin", "*.beam"])) ],
    ok = eunit:test(Modules, [verbose, {report, {eunit_surefire, [{dir, "."}]}}]).
