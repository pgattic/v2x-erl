-module(monitor).
-behavior(gen_server).
 
-export([start_link/0, update_position/2, get_position/1, get_history/1, delete_history/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

-type position() :: {number, number}.
-type car_state() :: {calendar:datetime(), position()}.
-type server_state() :: #{string() => [car_state()]}.
 
%% API (user-facing) functions

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
 
-spec update_position(string(), position()) -> ok.
update_position(Name, Value) ->
    gen_server:cast(?MODULE, {update_position, Name, Value}).
 
-spec get_position(string()) -> {ok, car_state()} | {error, no_data}.
get_position(Name) ->
    gen_server:call(?MODULE, {get_position, Name}).

-spec get_history(string()) -> {ok, [car_state()]} | {error, no_data}.
get_history(Name) ->
    gen_server:call(?MODULE, {get_history, Name}).
 
-spec delete_history(string()) -> ok.
delete_history(Name) ->
    gen_server:cast(?MODULE, {delete_history, Name}).
 
 
%% gen_server hooks

-spec init(_) -> {ok, server_state()}.
init(_) ->
    {ok, #{}}.
 
-spec handle_cast({update_position, string(), position()} | {remove_vehicle, string()}, server_state()) -> {noreply, server_state()}.
handle_cast({update_position, Name, Position}, State) ->
    Entry = {calendar:local_time(), Position},
    case maps:find(Name, State) of
        {ok, History} -> {noreply, maps:update(Name, [Entry|History], State)};
        _ -> {noreply, maps:put(Name, [Entry], State)}
    end;
 
handle_cast({delete_history, Name}, State) ->
    NewState = maps:remove(Name, State),
    {noreply, NewState}.
 
-spec handle_call({get_position, string()} | {get_history, string()}, _, server_state()) -> {reply, {ok, car_state() | [car_state()]} | {error, no_data}, server_state()}.
handle_call({get_position, Name}, _From, State) ->
    case maps:find(Name, State) of
        {ok, [Value|_]} -> {reply, {ok, Value}, State};
        _ -> {reply, {error, no_data}, State}
    end;
 
handle_call({get_history, Name}, _From, State) ->
    case maps:find(Name, State) of
        {ok, Value} -> {reply, {ok, Value}, State};
        _ -> {reply, {error, no_data}, State}
    end.
 
-spec terminate(_, _) -> ok.
terminate(_, _) ->
    ok.
 
-spec code_change(_, server_state(), _) -> {ok, server_state()}.
code_change(_, State, _) ->
    {ok, State}.

