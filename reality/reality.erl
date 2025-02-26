-module(reality).
-behaviour(gen_server).

%%% ================================================================================================
%%%
%%% REALITY module
%%%
%%% In a real-life v2x system, there is an aspect of communication that is not explicitly managed in
%%% software. That is, communication through the environment. This includes seeing, radio signaling,
%%% and normal force.
%%%
%%% Since our v2x system is not deployed on actual hardware, that type of communication must still
%%% be handled in software. Thus, the purpose of this process is to emulate those methods of
%%% communication. When something sees, it sees through reality.
%%%
%%% ================================================================================================

%% API
-export([start_link/0, report_position/3, get_state/0, see/2]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% Starts the gen_server and registers it as 'reality'
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Sends a report_position containing the entity type, its pid, and its position.
report_position(Type, Pid, Pos) ->
  gen_server:cast(?MODULE, {report_position, Type, Pid, Pos}).

%% Retrieves the current state (a map of Pid to {Type, Pos})
get_state() ->
  gen_server:call(?MODULE, get_state).

%% The "see" API receives a position and a radius, and returns all entities within that radius.
%% Assumes positions are in 2D: {X, Y}
see({X, Y} = Pos, Radius) ->
  gen_server:call(?MODULE, {see, Pos, Radius}).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

init([]) ->
  %% Initialize with an empty map: #{Pid => {Type, Pos}}
  {ok, #{}}.

%% Handle incoming report_positions by updating the map with the latest data.
handle_cast({report_position, Type, Pid, Pos}, State) ->
  io:format("Received report_position from ~p: ~p, ~p~n", [Type, Pid, Pos]),
  NewState = State#{Pid => {Type, Pos}},
  {noreply, NewState};
handle_cast(_Msg, State) ->
  {noreply, State}.

%% Handle synchronous calls.
handle_call(get_state, _From, State) ->
  {reply, State, State};

%% For "see" requests, filter entities by checking if their squared distance is less than or equal to Radius*Radius.
handle_call({see, Pos, Radius}, _From, State) ->
  Entities = maps:fold(
               fun(Pid, {Type, EntityPos}, Acc) ->
                   case distance(EntityPos, Pos) =< Radius of
                     true -> [{Pid, Type, EntityPos} | Acc];
                     false -> Acc
                   end
               end, [], State),
  {reply, Entities, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Computes the Euclidean distance between two 2D positions.
distance({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

