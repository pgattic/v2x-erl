-module(traffic_light).
-behaviour(gen_statem).

%% API Functions
-export([start_link/0, stop/1]).

%% gen_statem Callbacks
-export([callback_mode/0, init/1, terminate/3, handle_event/4, code_change/3]).

%% Type Definitions
-type state() :: red | yellow | green.
-type event() :: timeout | stop.

%% State Machine Mode
callback_mode() ->
    state_functions.

%% Start the Traffic Light State Machine
start_link() ->
    gen_statem:start_link(?MODULE, [], []).

%% Stop the State Machine
stop(Pid) ->
    gen_statem:cast(Pid, stop).

%% Initialization
init([]) ->
    io:format("Starting Traffic Light in RED state...~n"),
    {ok, red, 5000}. %% Start in red state, timeout after 5 seconds.

%% State Handlers
red(timeout, _From, _State) ->
    io:format("State: RED -> Switching to GREEN~n"),
    {next_state, green, 10000}; %% Transition to green after 10 seconds.

green(timeout, _From, _State) ->
    io:format("State: GREEN -> Switching to YELLOW~n"),
    {next_state, yellow, 2000}; %% Transition to yellow after 2 seconds.

yellow(timeout, _From, _State) ->
    io:format("State: YELLOW -> Switching to RED~n"),
    {next_state, red, 5000}.

%% Handle unexpected events (or special commands like stop)
handle_event(stop, _From, State, _Data) ->
    io:format("Received stop command. Stopping from state: ~p~n", [State]),
    {stop, normal, State};
handle_event(Event, _From, State, _Data) ->
    io:format("Unhandled event: ~p in state: ~p~n", [Event, State]),
    {keep_state, State}. %% Ignore unrecognized events.

%% Other Callbacks
terminate(_Reason, _State, _Data) ->
    io:format("Traffic Light terminated.~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
