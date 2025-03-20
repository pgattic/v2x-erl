%%% ==================================================================
%% @doc
%%
%% REALITY module
%%
%% In a real-life v2x system, there is an aspect of communication that is not explicitly managed in software. That is, communication through the environment. This includes seeing, radio signaling, and normal force.
%%
%% Since our v2x system is not deployed on actual hardware, that type of communication must still be handled in software. Thus, the purpose of this process is to emulate those methods of communication. When something sees, it sees through reality.
%%
%% @end
%%% ==================================================================

-module(reality).
-behaviour(gen_server).

%% API
-export([start_link/0, report_position/3, see/2]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, terminate/2, code_change/3]).

-type server_state() :: #{
  pid() => {atom(), position()}
}.
-type position() :: {number(), number()}.

%%%===================================================================
%%% API Functions
%%%===================================================================

%% Starts the gen_server and registers it as 'reality'
-spec start_link() -> gen_server:start_ret().
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Sends a report_position containing the entity type, its pid, and
%% its position.
%% This function should be invoked as often as possible for any entity
%% whose position is relevant to the simulation.
-spec report_position(atom(), pid(), position()) -> ok.
report_position(Type, Pid, Pos) ->
  gen_server:cast(?MODULE, {report_position, Type, Pid, Pos}).

%% The "see" API receives a position and a radius, and returns all
%% entities within that radius.
%% Assumes positions are in 2D: {X, Y}
-spec see(position(), number()) -> [{atom(), position()}] | term().
see(Pos, Radius) ->
  gen_server:call(?MODULE, {see, Pos, Radius}).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

%% Initialize with an empty map: #{Pid => {Type, Pos}}
init([]) ->
  {ok, #{}}.

%% Handle incoming report_positions by updating the map with the latest data.
-spec handle_cast({report_position, atom(), pid(), position()}, server_state()) -> {noreply, server_state()}.
handle_cast({report_position, Type, Pid, Pos}, State) ->
  io:format("Received 'report_position' from ~p: ~p, ~p~n", [Type, Pid, Pos]),
  NewState = State#{Pid => {Type, Pos}},
  {noreply, NewState};
handle_cast(_Msg, State) ->
  {noreply, State}.

%% For "see" requests, filter entities by checking if their squared
%% distance is less than or equal to Radius*Radius.
%-spec handle_call({see, position(), number()}, pid(), server_state()) -> {reply, [{pid(), atom(), position()}] | ok, server_state()}.
handle_call({see, Pos, Radius}, _From, State) ->
  io:format("Received 'see' from position ~p with radius ~p~n", [Pos, Radius]),
  Entities = see_helper(State, Pos, Radius),
  {reply, Entities, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec terminate(string(), server_state()) -> ok.
terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Computes the Euclidean distance between two 2D positions.
-spec distance(position(), position()) -> number().
distance({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

-spec see_helper(server_state(), position(), number()) -> [{pid(), atom(), position()}].
see_helper(State, Pos, Radius) ->
  maps:fold(
    fun(Pid, {Type, EntityPos}, Acc) ->
      case distance(EntityPos, Pos) =< Radius of
        true -> [{Pid, Type, EntityPos} | Acc];
        false -> Acc
      end
    end, [], State).


%%%===================================================================
%%% Unit Tests
%%%===================================================================

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

distance_test_() ->
  [
    ?_assertEqual(distance({0, 0}, {4, 3}), 5.0),
    ?_assertEqual(distance({0, 0}, {1, 1}), math:sqrt(2))
  ].

see_test_() ->
  State = #{
    my_car_1 => {car, {45, 37}},
    my_car_2 => {car, {26, 43}},
    guy_1 => {person, {69, 420}},
    guy_2 => {person, {27, 44}}
  },

  [
    ?_assertEqual(see_helper(State, {48, 37}, 5), [{my_car_1, car, {45, 37}}]),
    ?_assertEqual(see_helper(State, {48, 37}, 1), []),
    ?_assertEqual(lists:member({my_car_2, car, {26, 43}}, see_helper(State, {26, 44}, 3)), true),
    ?_assertEqual(lists:member({guy_2, person, {27, 44}}, see_helper(State, {26, 44}, 3)), true)
  ].

-endif.

