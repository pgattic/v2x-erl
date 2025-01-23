-module(monitor).
 
-behavior(gen_server).
 
-export([start_link/0, update_position/2, get_position/1, get_history/1, remove_vehicle/1]).
 
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).


%% Data format returned from `calendar:local_time()`: `{{year, month, day}, {hour, minute, second}}`
-type timestamp() :: {{int, int, int}, {int, int, int}}.

-type car_state() :: {timestamp(), {float, float}}.
 
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
 
init(_) ->
    {ok, #{}}.
 
%API Functions
-spec update_position(string(), {float, float}) -> ok.
update_position(Key, Value) ->
    gen_server:cast(?MODULE, {update_position, Key, {calendar:local_time(), Value}}).
 
-spec get_position(string()) -> {ok, car_state()} | {error, no_data}.
get_position(Key) ->
    gen_server:call(?MODULE, {get_position, Key}).

-spec get_history(string()) -> {ok, [car_state()]} | {error, no_data}.
get_history(Key) ->
    gen_server:call(?MODULE, {get_all_data, Key}).
 
-spec remove_vehicle(string()) -> ok.
remove_vehicle(Key) ->
    gen_server:cast(?MODULE, {remove_vehicle, Key}).
 
 
%Handle Call
 
%|{remove_vehicle, string()}|get_position, }, Arg2) -> return_type().
%-spec handle_cast({update_position, string(), {int, int}}) -> {noreply, #{string(), [car_state()]}}.
handle_cast({update_position, Name, Position}, State) ->
    case maps:find(Name, State) of
        {ok, PositionList} -> {noreply, maps:update(Name, [Position|PositionList], State)};
        _ -> {noreply, maps:put(Name, [Position], State)}
    end;
 
handle_cast({remove_vehicle, Key}, State) ->
    NewState = maps:remove(Key, State),
    {noreply, NewState}.
 
handle_call({get_position, Key}, _From, State) ->
    case maps:find(Key, State) of
        {ok, [Value|_]} -> {reply, {ok, Value}, State};
        _ -> {reply, {error, no_data}, State}
    end;
 
handle_call({get_all_data, Key}, _From, State) ->
    case maps:find(Key, State) of
        {ok, Value} -> {reply, {ok, Value}, State};
        _ -> {reply, {error, no_data}, State}
    end.
 
terminate(_, _) ->
    ok.
 
code_change(_, State, _) ->
    {ok, State}.
