-module(vehicle_actor).
-behaviour(gen_server).

%% API
-export([start_link/2, update_position/2, request_traffic_info/1]).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2]).

%% State Definition
-record(state, {id, position, speed, aggregator_pid}).

%% Start the Vehicle Actor
start_link(Id, AggregatorPid) ->
    gen_server:start_link({local, Id}, ?MODULE, {Id, AggregatorPid}, []).

%% Public API to update position
update_position(Pid, Position) ->
    gen_server:cast(Pid, {update_position, Position}).

%% Public API to request traffic info
request_traffic_info(Pid) ->
    gen_server:call(Pid, request_traffic_info).

%% Callbacks
init({Id, AggregatorPid}) ->
    %% Initialize vehicle state
    State = #state{id = Id, position = {0, 0}, speed = 0, aggregator_pid = AggregatorPid},
    %% Schedule periodic updates
    timer:send_interval(5000, send_traffic_update),
    {ok, State}.

handle_info(send_traffic_update, State) ->
    %% Periodically send traffic updates to the aggregator
    TrafficData = {State#state.position, State#state.speed},
    gen_server:cast(State#state.aggregator_pid, {traffic_update, State#state.id, TrafficData}),
    {noreply, State}.

handle_cast({update_position, {Position, Speed}}, State) ->
    %% Update the vehicle's position and speed
    NewState = State#state{position = Position, speed = Speed},
    io:format("~p: Position updated to ~p with speed ~p~n", [State#state.id, Position, Speed]),
    {noreply, NewState}.

handle_call(request_traffic_info, _From, State) ->
    %% Request traffic info from the aggregator
    case gen_server:call(State#state.aggregator_pid, {get_info, State#state.position}) of
        {ok, TrafficData} ->
            io:format("~p: Received traffic info: ~p~n", [State#state.id, TrafficData]),
            {reply, TrafficData, State};
        {error, Reason} ->
            io:format("~p: Failed to get traffic info: ~p~n", [State#state.id, Reason]),
            {reply, {error, Reason}, State}
    end.

terminate(_Reason, State) ->
    io:format("~p: Terminating vehicle actor~n", [State#state.id]),
    ok.

