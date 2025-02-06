-module(monitor).
-behavior(gen_server).

-export([start_link/0, start_link/1, update_position/2, get_position/1, get_history/1, delete_history/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

-type server_state() :: #{
  area => string(),
  vehicles => #{string() => [car_state()]}
}.
-type car_state() :: {calendar:datetime(), position()}.
-type position() :: {number(), number()}.

%%----------------------------------------------------------------------
%%  API
%%----------------------------------------------------------------------

-spec start_link(Area :: string()) -> gen_server:start_ret().
start_link(Area) ->
  %% We use a unique registered name for each area monitor.
  %% For instance, "North" -> monitor_North
  NameAtom = list_to_atom("monitor_" ++ Area),
  gen_server:start_link({local, NameAtom}, ?MODULE, [Area], []).

%% Second start_link/0 for the case of a single global monitor
-spec start_link() -> gen_server:start_ret().
start_link() ->
  io:format("WARNING: This function is meant to only be used for testing purposes and should not be used alone in production!~nPlease consult README.md for more information.~n"),
  %% Just registers under the module name "monitor" the way you originally did
  gen_server:start_link({local, ?MODULE}, ?MODULE, ["Global"], []).

-spec update_position(string(), position()) -> ok.
update_position(VehicleName, Pos) ->
  %% We cast to any running instance that has been registered with gen_server:start_link/3 above.
  %% If you want to target a specific area, you would call update_position on that area’s registered name.
  gen_server:cast(?MODULE, {update_position, VehicleName, Pos}).

-spec get_position(string()) -> {ok, car_state()} | {error, no_data}.
get_position(VehicleName) ->
  gen_server:call(?MODULE, {get_position, VehicleName}).

-spec get_history(string()) -> {ok, [car_state()]} | {error, no_data}.
get_history(VehicleName) ->
  gen_server:call(?MODULE, {get_history, VehicleName}).

-spec delete_history(string()) -> ok.
delete_history(VehicleName) ->
  gen_server:cast(?MODULE, {delete_history, VehicleName}).

%%----------------------------------------------------------------------
%%  gen_server callbacks
%%----------------------------------------------------------------------

-spec init([Area :: string()]) -> {ok, server_state()}.
init([Area]) ->
  %% Store the area in the state, plus an empty vehicles map
  State = #{
            area => Area,
            vehicles => #{}
           },
  {ok, State}.

handle_cast({update_position, VehicleName, Position}, State = #{vehicles := Vehicles}) ->
  Entry = {calendar:local_time(), Position},
  UpdatedVehicles =
  case maps:find(VehicleName, Vehicles) of
    {ok, History} -> Vehicles#{VehicleName => [Entry | History]};
    error         -> Vehicles#{VehicleName => [Entry]}
  end,
  {noreply, State#{vehicles := UpdatedVehicles}};

handle_cast({delete_history, VehicleName}, State = #{vehicles := Vehicles}) ->
  UpdatedVehicles = maps:remove(VehicleName, Vehicles),
  {noreply, State#{vehicles := UpdatedVehicles}}.

handle_call({get_position, VehicleName}, _From, State = #{vehicles := Vehicles}) ->
  case maps:find(VehicleName, Vehicles) of
    {ok, [Latest|_]} -> {reply, {ok, Latest}, State};
    error            -> {reply, {error, no_data}, State}
  end;

handle_call({get_history, VehicleName}, _From, State = #{vehicles := Vehicles}) ->
  case maps:find(VehicleName, Vehicles) of
    {ok, History} -> {reply, {ok, History}, State};
    error         -> {reply, {error, no_data}, State}
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

