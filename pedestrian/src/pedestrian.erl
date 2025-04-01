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
    io:format("Pedestrian process started~n"),
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
    io:format("Pedestrian is starting to cross with countdown~n"),
    send_countdown(5),
    {noreply, State#state{status = crossing_with_countdown}};
handle_cast(cross, State = #state{status = crossing_with_countdown}) ->
    io:format("Pedestrian is already crossing~n"),
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
    io:format("Countdown: 0~n"),
    self() ! done_crossing,
    {noreply, State};
handle_info({countdown, N}, State) when N > 0 ->
    io:format("Countdown: ~p~n", [N]),
    send_countdown(N - 1),
    {noreply, State};
handle_info(done_crossing, State) ->
    io:format("Pedestrian has finished crossing~n"),
    timer:send_after(3000, self(), reset),
    {noreply, State#state{status = done}};
handle_info(reset, _State) ->
    io:format("Pedestrian reset to waiting~n"),
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
%%% Unit Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% Unit test for default pedestrian startup.
%%%-------------------------------------------------------------------
default_start_test_() ->
    [
        {"starts default pedestrian process", fun() ->
            {ok, Pid} = start_link(),
            true = is_process_alive(Pid)
        end}
    ].

%%%-------------------------------------------------------------------
%%% @doc
%%% Unit test for named pedestrian startup.
%%%-------------------------------------------------------------------
named_start_test_() ->
    [
        {"starts named pedestrian process", fun() ->
            {ok, Pid} = start_link(my_pedestrian),
            true = is_process_alive(Pid)
        end}
    ].

%%%-------------------------------------------------------------------
%%% @doc
%%% Unit test for initial state of pedestrian.
%%%-------------------------------------------------------------------
initial_state_test_() ->
    [
        {"initial state is waiting", fun() ->
            start_link(),
            ?assertEqual(waiting, get_state())
        end}
    ].

%%%-------------------------------------------------------------------
%%% @doc
%%% Unit test for crossing request state transition.
%%%-------------------------------------------------------------------
crossing_transition_test_() ->
    [
        {"cross_request changes state to crossing_with_countdown", fun() ->
            start_link(),
            cross_request(),
            timer:sleep(50),
            ?assertEqual(crossing_with_countdown, get_state())
        end}
    ].

%%%-------------------------------------------------------------------
%%% @doc
%%% Unit test for countdown reaching zero transitioning to done.
%%%-------------------------------------------------------------------
countdown_to_done_test_() ->
    [
        {"countdown 0 triggers transition to done", fun() ->
            start_link(),
            cross_request(),
            self() ! {countdown, 0},
            timer:sleep(50),
            ?assertEqual(done, get_state())
        end}
    ].

%%%-------------------------------------------------------------------
%%% @doc
%%% Unit test for automatic reset from done to waiting.
%%%-------------------------------------------------------------------
auto_reset_test_() ->
    [
        {"done state automatically resets to waiting", fun() ->
            start_link(),
            cross_request(),
            self() ! {countdown, 0},
            timer:sleep(50),
            self() ! reset,
            timer:sleep(50),
            ?assertEqual(waiting, get_state())
        end}
    ].

-endif.
