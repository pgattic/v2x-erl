-module(traffic_light).
-behaviour(gen_statem).

%% API
-export([start_link/0, change_light/0, trigger_emergency/0, stop_emergency/0, stop/0]).

%% gen_statem Callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/3]).


% States: red, green, yellow, emergency


%% API Functions
start_link() ->
  gen_statem:start_link({local, traffic_light}, ?MODULE, red, []).

change_light() ->
  gen_statem:cast(traffic_light, next).

trigger_emergency() ->
  gen_statem:cast(traffic_light, trigger_emergency).

stop_emergency() ->
  gen_statem:cast(traffic_light, stop_emergency).

stop() ->
  gen_statem:cast(traffic_light, stop).

%% Callback Mode
callback_mode() ->
  handle_event_function.

%% Initialization
init(InitialState) ->
  io:format("Starting Traffic Light in RED state...~n"),
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
  io:format("Stopping Traffic Light~n"),
  {stop, normal, _State}.

%% Other Callbacks
terminate(_Reason, _State, _Data) ->
  io:format("Traffic Light terminated.~n"),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

