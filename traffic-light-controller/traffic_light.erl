-module(traffic_light).
-behaviour(gen_statem).

%% API
-export([start_link/0, change_light/1, trigger_emergency/1, stop_emergency/1, stop/1]).

%% gen_statem Callbacks
-export([init/1, callback_mode/0, handle_event/4]).


% States: red, green, yellow, emergency


%% API Functions
start_link() ->
  gen_statem:start_link(?MODULE, red, []).

change_light(Pid) ->
  gen_statem:cast(Pid, next).

trigger_emergency(Pid) ->
  gen_statem:cast(Pid, trigger_emergency).

stop_emergency(Pid) ->
  gen_statem:cast(Pid, stop_emergency).

stop(Pid) ->
  gen_statem:cast(Pid, stop).

%% Callback Mode
callback_mode() ->
  handle_event_function.

%% Initialization
init(InitialState) ->
  {ok, InitialState, []}.

%% Event Handling
handle_event(cast, next, red, _Data) ->
  io:format("Switching to green~n"),
  {next_state, green, undefined, {state_timeout, 5000, timeout}};
handle_event(state_timeout, timeout, red, _Data) ->
  io:format("Red light timeout, switching to green automatically~n"),
  {next_state, green, undefined, {state_timeout, 5000, timeout}};

handle_event(cast, next, green, _Data) ->
  io:format("Switching to yellow~n"),
  {next_state, yellow, undefined, {state_timeout, 3000, timeout}};
handle_event(state_timeout, timeout, green, _Data) ->
  io:format("Green light timeout, switching to yellow automatically~n"),
  {next_state, yellow, undefined, {state_timeout, 3000, timeout}};

handle_event(cast, next, yellow, _Data) ->
  io:format("Switching to red~n"),
  {next_state, red, undefined, {state_timeout, 2000, timeout}};
handle_event(state_timeout, timeout, yellow, _Data) ->
  io:format("Yellow light timeout, switching to red automatically~n"),
  {next_state, red, undefined, {state_timeout, 2000, timeout}};

handle_event(cast, trigger_emergency, _State, _Data) ->
  io:format("Emergency Mode Activated!~n"),
  {next_state, emergency, undefined};
handle_event(cast, stop_emergency, emergency, _Data) ->
  io:format("Emergency Mode Deactivated!~n"),
  io:format("Switching to red~n"),
  {next_state, red, undefined, {state_timeout, 2000, timeout}};

handle_event(cast, stop, _State, _Data) ->
  io:format("Received stop command. Stopping Traffic Light...~n"),
  {stop, normal, _State}. %% Gracefully stop the state machine.


