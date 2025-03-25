-module(tram_test).
-include_lib("eunit/include/eunit.hrl").

tram_test_() ->
    {
        setup,
        fun setup/0,
        fun cleanup/1,
        fun test_sequence/1
    }.

setup() ->
    {ok, Pid} = tram:start_link(),
    Pid.

cleanup(_Pid) ->
    ok.

test_sequence(_Pid) ->
    ?assertEqual(ok, tram:open_doors()),
    timer:sleep(100),

    ?assertEqual(ok, tram:close_doors()),
    timer:sleep(100),

    ?assertEqual(ok, tram:start()),
    timer:sleep(100),

    ?assertEqual(ok, tram:stop()),
    timer:sleep(100),

    ?assertEqual(ok, tram:start()),
    timer:sleep(100),
    ?assertEqual(ok, tram:open_doors()),
    timer:sleep(100),
    tram:stop().
