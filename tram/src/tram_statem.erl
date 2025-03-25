-module(tram_statem).
-behaviour(gen_statem).

%% API
-export([start_link/0, start/0, stop/0, open_doors/0, close_doors/0]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([stopped/3, moving/3, doors_open/3]).

-define(SERVER, ?MODULE).

%%% API Functions

start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_statem:cast(?SERVER, start).

stop() ->
    gen_statem:cast(?SERVER, stop).

open_doors() ->
    gen_statem:cast(?SERVER, open_doors).

close_doors() ->
    gen_statem:cast(?SERVER, close_doors).

%%% gen_statem Callbacks

init([]) ->
    io:format("Tram initialized~n"),
    {ok, stopped, #{}}.

callback_mode() ->
    handle_event_function.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%% States

stopped(cast, start, Data) ->
    io:format("Tram starting to move~n"),
    {next_state, moving, Data};

stopped(cast, open_doors, Data) ->
    io:format("Opening doors~n"),
    {next_state, doors_open, Data};

stopped(Type, Event, Data) ->
    io:format("Event ~p not valid in stopped state~n", [{Type, Event}]),
    {keep_state, Data}.

moving(cast, stop, Data) ->
    io:format("Tram stopping~n"),
    {next_state, stopped, Data};

moving(Type, Event, Data) ->
    io:format("Event ~p not valid while moving~n", [{Type, Event}]),
    {keep_state, Data}.

doors_open(cast, close_doors, Data) ->
    io:format("Closing doors~n"),
    {next_state, stopped, Data};

doors_open(Type, Event, Data) ->
    io:format("Event ~p not valid with doors open~n", [{Type, Event}]),
    {keep_state, Data}.


-include_lib("eunit/include/eunit.hrl").

    tram_test_() ->
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            fun test_sequence/1
        }.
    
    setup() ->
        {ok, Pid} = tram_statem:start_link(),
        Pid.
    
    cleanup(_Pid) ->
        ok.
    
    test_sequence(_Pid) ->
        ?assertEqual(ok, tram_statem:open_doors()),
        timer:sleep(100),
    
        ?assertEqual(ok, tram_statem:close_doors()),
        timer:sleep(100),
    
        ?assertEqual(ok, tram_statem:start()),
        timer:sleep(100),
    
        ?assertEqual(ok, tram_statem:stop()),
        timer:sleep(100),
    
        ?assertEqual(ok, tram_statem:start()),
        timer:sleep(100),
        ?assertEqual(ok, tram_statem:open_doors()),
        timer:sleep(100),
        tram:stop().
    