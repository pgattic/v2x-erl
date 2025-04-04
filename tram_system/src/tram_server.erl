%% @doc
%% A simple GenServer implementation for a tram system with position tracking.
%% Converted from gen_statem to gen_server.
%%
%% The tram has three main operational states: `stopped`, `moving`, and `doors_open`.
%% Clients can issue commands to transition between states and manage tram position.
%%
%% == Metadata ==
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – simple state transitions and position tracking using maps
%% @since 2025-04-01

%% @module tram_server
%% GenServer implementation for simulating a tram system's movement and door states.

-module(tram_server).

%% @behaviour gen_server
%% Specifies that this module implements the gen_server behaviour callbacks.

-behaviour(gen_server).

%% @exported API functions
%% Public functions available to clients for controlling the tram's state and position.
-export([
    start_link/0,    % Starts the server
    start/0,         % Starts moving the tram
    stop/0,          % Stops the tram
    open_doors/0,    % Opens the doors (when stopped)
    close_doors/0,   % Closes the doors (when open)
    set_position/1,  % Sets the current position {X, Y}
    get_position/0   % Retrieves the current position
]).

%% @exported callbacks
%% Required callbacks for gen_server behaviour.
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% @define SERVER
%% Macro for referring to this module’s name (used in local registration).
-define(SERVER, ?MODULE).

%%% API Functions

%% @doc
%% Starts the GenServer and registers it locally under the module name.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – direct call to gen_server:start_link/4
%% @since 2025-04-01
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc
%% Sends a `start` cast to transition the tram from `stopped` to `moving`.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – fire-and-forget message passing
%% @since 2025-04-01
start() ->
    gen_server:cast(?SERVER, start).

%% @doc
%% Sends a `stop` cast to transition the tram from `moving` to `stopped`.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – fire-and-forget message passing
%% @since 2025-04-01
stop() ->
    gen_server:cast(?SERVER, stop).

%% @doc
%% Opens the tram doors if it is in the `stopped` state.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – state-guarded command
%% @since 2025-04-01
open_doors() ->
    gen_server:cast(?SERVER, open_doors).

%% @doc
%% Closes the tram doors if they are currently open.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – state-guarded command
%% @since 2025-04-01
close_doors() ->
    gen_server:cast(?SERVER, close_doors).

%% @doc
%% Sets the tram's position to the given `{X, Y}` coordinate.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – map update with simple validation
%% @since 2025-04-01
set_position({X, Y}) when is_integer(X), is_integer(Y) ->
    gen_server:cast(?SERVER, {set_position, {X, Y}}).

%% @doc
%% Retrieves the current tram position.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – map lookup via gen_server call
%% @since 2025-04-01
get_position() ->
    gen_server:call(?SERVER, get_position).

%%% gen_server Callbacks

%% @doc
%% Initializes the GenServer with default state: `stopped` and position `{0, 0}`.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – initializes map-based state
%% @since 2025-04-01
init([]) ->
    io:format("Tram initialized~n"),
    {ok, #{state => stopped, position => {0, 0}}}.

%% @doc
%% Handles a synchronous request to get the tram's current position.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – pattern matching with map lookup
%% @since 2025-04-01
handle_call(get_position, _From, Data) ->
    {reply, maps:get(position, Data), Data};

%% @doc
%% Handles unrecognized synchronous requests with an error response.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – default pattern match
%% @since 2025-04-01
handle_call(_, _From, Data) ->
    {reply, error, Data}.

%% @doc
%% Updates the tram's position based on the provided `{X, Y}` tuple.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – simple map update with logging
%% @since 2025-04-01
handle_cast({set_position, {X, Y}}, Data) ->
    io:format("Position updated to (~p, ~p)~n", [X, Y]),
    {noreply, Data#{position => {X, Y}}};

%% @doc
%% Handles the `start` command when tram is in `stopped` state – transitions to `moving`.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – pattern matching with state transition
%% @since 2025-04-01
handle_cast(start, #{state := stopped} = Data) ->
    io:format("Tram starting to move~n"),
    {noreply, Data#{state => moving}};

%% @doc
%% Handles the `start` command when tram is in `doors_open` state – blocks action.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – guard with fallback handling
%% @since 2025-04-01
handle_cast(start, #{state := doors_open} = Data) ->
    io:format("Cannot start while doors are open, closing doors first~n"),
    {noreply, Data};

%% @doc
%% Handles the `start` command in invalid states – logs an error message.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – fallback clause with state introspection
%% @since 2025-04-01
handle_cast(start, Data) ->
    io:format("Start not valid in state ~p~n", [maps:get(state, Data)]),
    {noreply, Data};

%% @doc
%% Handles the `stop` command when tram is `moving` – transitions to `stopped`.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – state-based transition with logging
%% @since 2025-04-01
handle_cast(stop, #{state := moving} = Data) ->
    io:format("Tram stopping~n"),
    {noreply, Data#{state => stopped}};

%% @doc
%% Handles invalid `stop` commands in states where stopping isn't allowed.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – fallback clause with logging
%% @since 2025-04-01
handle_cast(stop, Data) ->
    io:format("Stop not valid in state ~p~n", [maps:get(state, Data)]),
    {noreply, Data};

%% @doc
%% Handles `open_doors` command when tram is `stopped` – transitions to `doors_open`.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – guarded state change
%% @since 2025-04-01
handle_cast(open_doors, #{state := stopped} = Data) ->
    io:format("Opening doors~n"),
    {noreply, Data#{state => doors_open}};

%% @doc
%% Handles invalid `open_doors` requests – logs an error and retains current state.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – fallback guard with state introspection
%% @since 2025-04-01
handle_cast(open_doors, Data) ->
    io:format("Open doors not valid in state ~p~n", [maps:get(state, Data)]),
    {noreply, Data};

%% @doc
%% Handles `close_doors` command when doors are open – transitions to `stopped`.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – conditional state transition
%% @since 2025-04-01
handle_cast(close_doors, #{state := doors_open} = Data) ->
    io:format("Closing doors~n"),
    {noreply, Data#{state => stopped}};

%% @doc
%% Handles invalid `close_doors` requests – retains state and logs error.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – fallback clause with guard
%% @since 2025-04-01
handle_cast(close_doors, Data) ->
    io:format("Close doors not valid in state ~p~n", [maps:get(state, Data)]),
    {noreply, Data}.

%% @doc
%% Handles unexpected or irrelevant messages by ignoring them.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – default ignore pattern
%% @since 2025-04-01
handle_info(_Info, Data) ->
    {noreply, Data}.

%% @doc
%% Handles server termination; no specific cleanup logic is performed.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – no-op termination
%% @since 2025-04-01
terminate(_Reason, _Data) ->
    ok.

%% @doc
%% Allows for future upgrade handling; currently just passes state forward unchanged.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – no transformation logic
%% @since 2025-04-01
code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

%%% Tests

%% @doc
%% EUnit tests for the `tram_server` module.
%%
%% This test suite verifies:
%% - Initial position defaults
%% - Setting and getting position
%% - Transitioning between states (start, stop)
%% - Opening and closing doors
%%
%% The test uses `setup` to start a fresh server instance before each run.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – direct black-box testing of exposed API
%% @since 2025-04-01
-include_lib("eunit/include/eunit.hrl").

tram_server_test_() ->
    {setup,
     %% Setup function: starts the server
     fun() ->
         {ok, Pid} = tram_server:start_link(),
         Pid
     end,

     %% Cleanup function: no teardown needed
     fun(_) ->
         ok
     end,

     %% Test cases
     fun(_) ->
         [
             %% Test initial position
             ?_assertEqual({0, 0}, tram_server:get_position()),

             %% Test position update
             ?_assertEqual(ok, tram_server:set_position({10, 5})),
             ?_assertEqual({10, 5}, tram_server:get_position()),

             %% Test start from stopped
             ?_assertEqual(ok, tram_server:start()),

             %% Test stop from moving
             ?_assertEqual(ok, tram_server:stop()),

             %% Test door operations
             ?_assertEqual(ok, tram_server:open_doors()),
             ?_assertEqual(ok, tram_server:close_doors())
         ]
     end
    }.
