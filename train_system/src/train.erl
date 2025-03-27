%%% ==================================================================
%%% @doc
%%%
%%% TRAIN module - GenServer implementation for a train process.
%%%
%%% This module represents a train as an OTP GenServer, managing:
%%% - Train state (status, speed, direction, passengers, destination)
%%% - Operations like starting, stopping, accelerating, and decelerating
%%% - Passenger boarding and departures
%%% - Dynamic process management through PIDs
%%%
%%% The train interacts with external processes via asynchronous casts
%%% and synchronous calls.
%%%
%%% @author Boston Williams
%%% @version 1.1
%%% @since 2025-03-20
%%% @end
%%% ==================================================================

-module(train).
-behaviour(gen_server).

%% API Functions
-export([
    start/1, stop/1, accelerate/2, decelerate/2, 
    set_destination/2, board_passengers/2, depart_passengers/2, 
    get_state/1, start_link/1,
    report_position/1, see/2
]).

%% gen_server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% ==================================================================
%%%                            API FUNCTIONS
%%% ==================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% Starts a train process and links it to the calling process.
%%% @param Name The name of the train.
%%% @returns {ok, Pid} on success.
%%% @author Boston Williams
%%% @version 1.1
%%% @since 2025-03-20
%%% @complexity O(1)
%%% @end
%%%-------------------------------------------------------------------
start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

%%%-------------------------------------------------------------------
%%% @doc
%%% Starts the train (changes status to 'moving').
%%% @param Pid The process ID of the train.
%%% @returns ok.
%%% @complexity O(1)
%%% @end
%%%-------------------------------------------------------------------
start(Pid) -> 
    gen_server:cast(Pid, start).

%%%-------------------------------------------------------------------
%%% @doc
%%% Stops the train (changes status to 'stopped' and speed to 0).
%%% @param Pid The process ID of the train.
%%% @returns ok.
%%% @complexity O(1)
%%% @end
%%%-------------------------------------------------------------------
stop(Pid) -> 
    gen_server:cast(Pid, stop).

%%%-------------------------------------------------------------------
%%% @doc
%%% Increases the train’s speed.
%%% @param Pid The process ID of the train.
%%% @param SpeedIncrease The amount to increase speed.
%%% @returns ok.
%%% @complexity O(1)
%%% @end
%%%-------------------------------------------------------------------
accelerate(Pid, SpeedIncrease) -> 
    gen_server:cast(Pid, {accelerate, SpeedIncrease}).

%%%-------------------------------------------------------------------
%%% @doc
%%% Decreases the train’s speed, ensuring it does not go below zero.
%%% @param Pid The process ID of the train.
%%% @param SpeedDecrease The amount to decrease speed.
%%% @returns ok.
%%% @complexity O(1)
%%% @end
%%%-------------------------------------------------------------------
decelerate(Pid, SpeedDecrease) -> 
    gen_server:cast(Pid, {decelerate, SpeedDecrease}).

%%%-------------------------------------------------------------------
%%% @doc
%%% Sets the destination for the train.
%%% @param Pid The process ID of the train.
%%% @param Destination The new destination.
%%% @returns ok.
%%% @complexity O(1)
%%% @end
%%%-------------------------------------------------------------------
set_destination(Pid, Destination) -> 
    gen_server:cast(Pid, {set_destination, Destination}).

%%%-------------------------------------------------------------------
%%% @doc
%%% Boards passengers onto the train.
%%% @param Pid The process ID of the train.
%%% @param NumPassengers The number of passengers boarding.
%%% @returns ok.
%%% @complexity O(1)
%%% @end
%%%-------------------------------------------------------------------
board_passengers(Pid, NumPassengers) -> 
    gen_server:cast(Pid, {board_passengers, NumPassengers}).

%%%-------------------------------------------------------------------
%%% @doc
%%% Allows passengers to depart from the train.
%%% @param Pid The process ID of the train.
%%% @param NumPassengers The number of passengers departing.
%%% @returns ok.
%%% @complexity O(1)
%%% @end
%%%-------------------------------------------------------------------
depart_passengers(Pid, NumPassengers) -> 
    gen_server:cast(Pid, {depart_passengers, NumPassengers}).

%%%-------------------------------------------------------------------
%%% @doc
%%% Retrieves the current state of the train.
%%% @param Pid The process ID of the train.
%%% @returns The train's state as a map.
%%% @complexity O(1)
%%% @end
%%%-------------------------------------------------------------------
get_state(Pid) -> 
    gen_server:call(Pid, get_state).

%%%-------------------------------------------------------------------
%%% @doc
%%% Reports the train's current position to the reality system.
%%% @param Position The current position.
%%% @returns ok.
%%%-------------------------------------------------------------------
report_position(Position) ->
    Result = rpc:cast('v2xmage@164.92.104.30', reality, report_position, [train, self(), Position]),
    io:format("Remote report_position result: ~p~n", [Result]),
    Result.

%%%-------------------------------------------------------------------
%%% @doc
%%% Queries visible objects around a given position.
%%% @param Position The center point.
%%% @param Radius The radius to see.
%%% @returns List of visible entities.
%%%-------------------------------------------------------------------
see(Position, Radius) ->
    Result = rpc:call('v2xmage@164.92.104.30', reality, see, [Position, Radius]),
    io:format("Remote see result: ~p~n", [Result]),
    Result.


%%% ==================================================================
%%%                        GEN_SERVER CALLBACKS
%%% ==================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% Initializes the train state.
%%% @param Name The name of the train.
%%% @returns {ok, State}.
%%% @complexity O(1)
%%% @end
%%%-------------------------------------------------------------------
init([Name]) ->
    State = #{
        name => Name,
        status => stopped,
        speed => 0,
        direction => undefined,
        passengers => 0,
        destination => none,
        max_speed => 200
    },
    {ok, State}.

%%%-------------------------------------------------------------------
%%% @doc
%%% Handles synchronous calls (e.g., retrieving state).
%%% @param Request The request.
%%% @param From The caller.
%%% @param State The current state.
%%% @returns {reply, Response, NewState}.
%%% @complexity O(1)
%%% @end
%%%-------------------------------------------------------------------
handle_call(get_state, _From, State) ->    
    {reply, State, State}.

%%%-------------------------------------------------------------------
%%% @doc
%%% Handles asynchronous casts (e.g., state changes).
%%% @param Msg The message.
%%% @param State The current state.
%%% @returns {noreply, NewState}.
%%% @complexity O(1)
%%% @end
%%%-------------------------------------------------------------------
handle_cast(stop, #{status := moving} = State) ->
    {noreply, State#{status => stopped, speed => 0}};

handle_cast({accelerate, SpeedIncrease}, #{status := moving, speed := Speed} = State) ->
    MaxSpeed = maps:get(max_speed, State),
    {noreply, State#{speed => min(Speed + SpeedIncrease, MaxSpeed)}};

handle_cast({decelerate, SpeedDecrease}, #{status := moving, speed := Speed} = State) ->
    {noreply, State#{speed => max(Speed - SpeedDecrease, 0)}};

handle_cast({set_destination, Destination}, State) ->
    {noreply, State#{destination => Destination}};

handle_cast({board_passengers, NumPassengers}, State) ->
    {noreply, State#{passengers => maps:get(passengers, State) + NumPassengers}};

handle_cast({depart_passengers, NumPassengers}, State) ->
    NewPassengers = max(maps:get(passengers, State) - NumPassengers, 0),
    {noreply, State#{passengers => NewPassengers}};

handle_cast(start, #{status := stopped} = State) ->
    NewState = State#{status => moving},
    io:format("~p is now moving.~n", [maps:get(name, State)]),
    Position = maps:get(position, State, {0,0}),
    report_position(Position),
    {noreply, NewState};

handle_cast(_, State) -> {noreply, State}.

%%%-------------------------------------------------------------------
%%% @doc
%%% Handles unexpected messages.
%%% @param Msg The message.
%%% @param State The current state.
%%% @returns {noreply, NewState}.
%%% @complexity O(1)
%%% @end
%%%-------------------------------------------------------------------
handle_info(_Msg, State) -> {noreply, State}.

%%%-------------------------------------------------------------------
%%% @doc
%%% Handles process termination.
%%% @param Reason The termination reason.
%%% @param State The last known state.
%%% @returns ok.
%%% @complexity O(1)
%%% @end
%%%-------------------------------------------------------------------
terminate(_Reason, _State) -> ok.

%%%-------------------------------------------------------------------
%%% @doc
%%% Handles code upgrades.
%%% @param OldVsn The previous version.
%%% @param State The current state.
%%% @param Extra Extra data.
%%% @returns {ok, NewState}.
%%% @complexity O(1)
%%% @end
%%%-------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%% ==================================================================
%%%                          UNIT TESTS
%%% ==================================================================
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

train_test_() ->
    {setup,
        fun setup/0,
        fun teardown/1,
        [
            fun start_test/0,
            fun stop_test/0,
            fun accelerate_test/0,
            fun decelerate_test/0,
            fun set_destination_test/0,
            fun board_passengers_test/0,
            fun depart_passengers_test/0,
            fun get_state_test/0
        ]}.

%%% Setup and Teardown
setup() ->
    {ok, Pid} = train:start_link(test_train),
    Pid.

teardown(Pid) ->
    gen_server:stop(Pid).

%%% Start Test
start_test() ->
    Pid = setup(),
    train:start(Pid),
    timer:sleep(50),
    State = train:get_state(Pid),
    ?assertEqual(moving, maps:get(status, State)),
    teardown(Pid).

%%% Stop Test
stop_test() ->
    Pid = setup(),
    train:start(Pid),
    train:stop(Pid),
    timer:sleep(50),
    State = train:get_state(Pid),
    ?assertEqual(stopped, maps:get(status, State)),
    ?assertEqual(0, maps:get(speed, State)),
    teardown(Pid).

%%% Accelerate Test
accelerate_test() ->
    Pid = setup(),
    train:start(Pid),
    train:accelerate(Pid, 50),
    timer:sleep(50),
    State = train:get_state(Pid),
    ?assertEqual(50, maps:get(speed, State)),

    %% Exceed max speed
    train:accelerate(Pid, 200),
    timer:sleep(50),
    State2 = train:get_state(Pid),
    ?assertEqual(200, maps:get(speed, State2)), %% Max speed cap
    teardown(Pid).

%%% Decelerate Test
decelerate_test() ->
    Pid = setup(),
    train:start(Pid),
    train:accelerate(Pid, 60),
    train:decelerate(Pid, 30),
    timer:sleep(50),
    State = train:get_state(Pid),
    ?assertEqual(30, maps:get(speed, State)),

    %% Decelerate below zero
    train:decelerate(Pid, 100),
    timer:sleep(50),
    State2 = train:get_state(Pid),
    ?assertEqual(0, maps:get(speed, State2)),
    teardown(Pid).

%%% Set Destination Test
set_destination_test() ->
    Pid = setup(),
    train:set_destination(Pid, "Paris"),
    timer:sleep(50),
    State = train:get_state(Pid),
    ?assertEqual("Paris", maps:get(destination, State)),
    teardown(Pid).

%%% Board Passengers Test
board_passengers_test() ->
    Pid = setup(),
    train:board_passengers(Pid, 5),
    timer:sleep(50),
    State = train:get_state(Pid),
    ?assertEqual(5, maps:get(passengers, State)),
    teardown(Pid).

%%% Depart Passengers Test
depart_passengers_test() ->
    Pid = setup(),
    train:board_passengers(Pid, 5),
    train:depart_passengers(Pid, 3),
    timer:sleep(50),
    State = train:get_state(Pid),
    ?assertEqual(2, maps:get(passengers, State)),

    %% Depart more than available
    train:depart_passengers(Pid, 10),
    timer:sleep(50),
    State2 = train:get_state(Pid),
    ?assertEqual(0, maps:get(passengers, State2)),
    teardown(Pid).

%%% Get State Test
get_state_test() ->
    Pid = setup(),
    State = train:get_state(Pid),
    ?assertMatch(#{name := _, status := stopped, speed := 0, max_speed := 200}, State),
    teardown(Pid).

-endif.