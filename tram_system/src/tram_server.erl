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

-module(tram_server).
-behaviour(gen_server).

%% API
-export([start_link/0, start/0, stop/0, open_doors/0, close_doors/0, set_position/1, get_position/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

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
%% Handles synchronous call to retrieve the current tram position or returns `error` for unknown calls.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – pattern matching on simple request
%% @since 2025-04-01
handle_call(get_position, _From, Data) ->
    {reply, maps:get(position, Data), Data};
handle_call(_, _From, Data) ->
    {reply, error, Data}.

%% @doc
%% Handles asynchronous cast messages for controlling tram behavior and position updates.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Medium – guards, state evaluation, and pattern matching
%% @since 2025-04-01
handle_cast({set_position, {X, Y}}, Data) ->
    io:format("Position updated to (~p, ~p)~n", [X, Y]),
    {noreply, Data#{position => {X, Y}}};

handle_cast(start, #{state := stopped} = Data) ->
    io:format("Tram starting to move~n"),
    {noreply, Data#{state => moving}};
handle_cast(start, #{state := doors_open} = Data) ->
    io:format("Cannot start while doors are open, closing doors first~n"),
    {noreply, Data};
handle_cast(start, Data) ->
    io:format("Start not valid in state ~p~n", [maps:get(state, Data)]),
    {noreply, Data};

handle_cast(stop, #{state := moving} = Data) ->
    io:format("Tram stopping~n"),
    {noreply, Data#{state => stopped}};
handle_cast(stop, Data) ->
    io:format("Stop not valid in state ~p~n", [maps:get(state, Data)]),
    {noreply, Data};

handle_cast(open_doors, #{state := stopped} = Data) ->
    io:format("Opening doors~n"),
    {noreply, Data#{state => doors_open}};
handle_cast(open_doors, Data) ->
    io:format("Open doors not valid in state ~p~n", [maps:get(state, Data)]),
    {noreply, Data};

handle_cast(close_doors, #{state := doors_open} = Data) ->
    io:format("Closing doors~n"),
    {noreply, Data#{state => stopped}};
handle_cast(close_doors, Data) ->
    io:format("Close doors not valid in state ~p~n", [maps:get(state, Data)]),
    {noreply, Data}.

%% @doc
%% Handles unexpected messages sent to the server.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – fallback clause
%% @since 2025-04-01
handle_info(_Info, Data) ->
    {noreply, Data}.

%% @doc
%% Handles cleanup when the GenServer terminates. No specific shutdown logic.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – no-op termination
%% @since 2025-04-01
terminate(_Reason, _Data) ->
    ok.

%% @doc
%% Handles upgrade logic when changing server version. Simply preserves state.
%%
%% @author Jared Keh
%% @version 1.1
%% @complexity Low – identity transformation
%% @since 2025-04-01
code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

%%% Tests
-include_lib("eunit/include/eunit.hrl").

tram_server_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = tram_server:start_link(),
         Pid
     end,
     fun(_) ->
         ok
     end,
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
