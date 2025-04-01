%%%-------------------------------------------------------------------
%%% @doc
%%% A `gen_server`-based module representing a pedestrian crossing system.
%%% The process transitions through `waiting`, `crossing_with_countdown`, 
%%% and `done` states based on crossing requests and timers.
%%%
%%% It supports both a default singleton pedestrian and named instances, 
%%% and includes built-in unit tests using EUnit.
%%%
%%% @author Elena Makin
%%% @version 1.1
%%% @since 2025-03-11
%%% @complexity Medium
%%%-------------------------------------------------------------------
-module(pedestrian).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, cross_request/0, get_state/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    status  %% waiting | crossing_with_countdown | done
}).

%%%===================================================================
%%% API
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% Starts the default singleton pedestrian process,
%%% registered locally under the module name.
%%%
%%% @spec start_link() -> {ok, pid()} | {error, any()}
%%%-------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%-------------------------------------------------------------------
%%% @doc
%%% Starts a named pedestrian process.
%%% Allows multiple independent pedestrian processes to exist.
%%%
%%% @spec start_link(Name :: atom()) -> {ok, pid()} | {error, any()}
%%%-------------------------------------------------------------------
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%%%-------------------------------------------------------------------
%%% @doc
%%% Sends a request to begin crossing.
%%% Only applicable for the default pedestrian process.
%%%
%%% @spec cross_request() -> ok
%%%-------------------------------------------------------------------
cross_request() ->
    gen_server:cast(?MODULE, cross).

%%%-------------------------------------------------------------------
%%% @doc
%%% Retrieves the current status of the pedestrian process.
%%% Returns one of: `waiting`, `crossing_with_countdown`, or `done`.
%%%
%%% @spec get_state() -> waiting | crossing_with_countdown | done
%%%-------------------------------------------------------------------
get_state() ->
    gen_server:call(?MODULE, get_state).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Initializes the pedestrian process in `waiting` state.
%%%
%%% @spec init([]) -> {ok, State}
%%%-------------------------------------------------------------------
init([]) ->
    % io:format("Pedestrian process started~n"),
    {ok, #state{status = waiting}}.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Handles synchronous `get_state` calls and default catch-all responses.
%%%
%%% @spec handle_call(Request, From, State) -> {reply, any(), State}
%%%-------------------------------------------------------------------
handle_call(get_state, _From, State) ->
    {reply, State#state.status, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Handles asynchronous `cross` requests and manages state transitions.
%%%
%%% @spec handle_cast(cross, State) -> {noreply, State}
%%%-------------------------------------------------------------------
handle_cast(cross, State = #state{status = waiting}) ->
    % io:format("Pedestrian is starting to cross with countdown~n"),
    send_countdown(5),
    {noreply, State#state{status = crossing_with_countdown}};
handle_cast(cross, State = #state{status = crossing_with_countdown}) ->
    % io:format("Pedestrian is already crossing~n"),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Handles internal messages for countdown, completion, and reset.
%%%
%%% @spec handle_info(Msg :: any(), State) -> {noreply, State}
%%%-------------------------------------------------------------------
handle_info({countdown, 0}, State) ->
    % io:format("Countdown: 0~n"),
    self() ! done_crossing,
    {noreply, State};
handle_info({countdown, N}, State) when N > 0 ->
    % io:format("Countdown: ~p~n", [N]),
    send_countdown(N - 1),
    {noreply, State};
handle_info(done_crossing, State) ->
    % io:format("Pedestrian has finished crossing~n"),
    timer:send_after(3000, self(), reset),
    {noreply, State#state{status = done}};
handle_info(reset, _State) ->
    % io:format("Pedestrian reset to waiting~n"),
    {noreply, #state{status = waiting}};
handle_info(_Info, State) ->
    {noreply, State}.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Callback executed on process termination.
%%%
%%% @spec terminate(Reason :: any(), State) -> ok
%%%-------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Callback for code upgrades.
%%%
%%% @spec code_change(OldVsn, State, Extra) -> {ok, State}
%%%-------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Utility function to send a countdown message with 1-second delay.
%%%
%%% @spec send_countdown(N :: non_neg_integer()) -> reference()
%%%-------------------------------------------------------------------
send_countdown(N) ->
    timer:send_after(1000, self(), {countdown, N}).

%%%===================================================================
%%% EUnit Tests (Structured with setup/teardown)
%%%===================================================================
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

pedestrian_test_() ->
    {setup,
        fun setup/0,
        fun teardown/1,
        [
            fun default_start_test/0,
            fun named_start_test/0,
            fun initial_state_test/0,
            fun crossing_transition_test/0,
            fun countdown_to_done_test/0,
            fun auto_reset_test/0
        ]}.

%% Helper: start only if not already started
setup() ->
    case whereis(pedestrian) of
        undefined ->
            {ok, Pid} = pedestrian:start_link(),
            Pid;
        Pid when is_pid(Pid) ->
            Pid
    end.

teardown(_Pid) ->
    catch gen_server:stop(pedestrian),
    ok.

default_start_test() ->
    catch gen_server:stop(pedestrian),
    {ok, Pid} = pedestrian:start_link(),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid).

named_start_test() ->
    {ok, Pid} = pedestrian:start_link(my_named_ped),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid).

initial_state_test() ->
    _Pid = setup(),
    ?assertEqual(waiting, pedestrian:get_state()).

crossing_transition_test() ->
    _Pid = setup(),
    pedestrian:cross_request(),
    timer:sleep(100),
    ?assertEqual(crossing_with_countdown, pedestrian:get_state()).

countdown_to_done_test() ->
    _Pid = setup(),
    pedestrian:cross_request(),

    %% Simulate full countdown
    pedestrian ! {countdown, 5},
    pedestrian ! {countdown, 4},
    pedestrian ! {countdown, 3},
    pedestrian ! {countdown, 2},
    pedestrian ! {countdown, 1},
    pedestrian ! {countdown, 0},

    %% Then what handle_info({countdown, 0}, ...) triggers
    pedestrian ! done_crossing,

    timer:sleep(50), %% Let messages be processed
    ?assertEqual(done, pedestrian:get_state()).
    

auto_reset_test() ->
    _Pid = setup(),
    pedestrian:cross_request(),

    %% Simulate the full countdown manually
    pedestrian ! {countdown, 5},
    pedestrian ! {countdown, 4},
    pedestrian ! {countdown, 3},
    pedestrian ! {countdown, 2},
    pedestrian ! {countdown, 1},
    pedestrian ! {countdown, 0},

    %% Countdown 0 triggers: done_crossing
    pedestrian ! done_crossing,
    timer:sleep(50),
    ?assertEqual(done, pedestrian:get_state()),

    %% Simulate delayed reset (normally comes 3s later)
    pedestrian ! reset,
    timer:sleep(50),
    ?assertEqual(waiting, pedestrian:get_state()).
    

-endif.
