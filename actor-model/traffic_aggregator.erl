-module(traffic_aggregator).
-behaviour(gen_server).

%% API
-export([start_link/0, get_traffic_info/1]).
-export([init/1, handle_cast/2, handle_call/3]).

%% State Definition
-record(state, {traffic_data}).

%% Start the Traffic Aggregator Actor
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Public API to get traffic info
get_traffic_info(Position) ->
    gen_server:call(?MODULE, {get_info, Position}).

%% Callbacks
init([]) ->
    %% Initialize with empty traffic data
    {ok, #state{traffic_data = #{}}}.

handle_cast({traffic_update, VehicleId, {Position, Speed}}, State) ->
    %% Update traffic data with the vehicle's report
    NewTrafficData = maps:put(VehicleId, {Position, Speed}, State#state.traffic_data),
    {noreply, State#state{traffic_data = NewTrafficData}}.

handle_call({get_info, Position}, _From, State) ->
    %% Simplified traffic info lookup
    NearbyVehicles = lists:filter(
        fun({_VehicleId, {VehPos, _VehSpeed}}) ->
            %% Example proximity check (simple distance calculation)
            distance(Position, VehPos) < 5
        end,
        maps:to_list(State#state.traffic_data)
    ),
    {reply, {ok, NearbyVehicles}, State}.

distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

