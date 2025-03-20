%%% ==================================================================
%%% @doc
%%%
%%% train_system_sup module - Supervisor for train processes.
%%%
%%% This module acts as an OTP supervisor for dynamically spawning and
%%% managing train processes. It provides:
%%% - Dynamic train process supervision with `start_train/1`
%%% - Controlled termination with `stop_train/1`
%%% - Retrieval of supervised train processes via `get_children/0`
%%%
%%% The supervisor follows a `one_for_one` strategy, meaning if a train
%%% process crashes, only that process will be restarted.
%%%
%%% @author Boston Williams
%%% @version 1.2
%%% @since 2025-03-20
%%% @end
%%% ==================================================================

-module(train_system_sup).
-behaviour(supervisor).

%% API Functions
-export([start_link/0]).

%% Supervisor Callbacks
-export([init/1]).

%%--------------------------------------------------------------------
%% API Functions
%%--------------------------------------------------------------------

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%% ==================================================================
%%%                      SUPERVISOR CALLBACKS
%%% ==================================================================

%% @doc Initializes the train supervisor.
%% @returns {ok, {one_for_one, MaxRestarts, MaxTime}, [ChildSpec]}.
%% @complexity O(1)
init([]) ->
    TrainSpec = #{
        id => train, %% Unique child id
        start => {train, start_link, []}, %% Module, function and arguments to start the child
        restart => permanent, %% Restart strategy: always restart
        shutdown => 5000, %% Shutdown timeout in milliseconds
        type => worker, %% Process type
        modules => [train] %% Modules implemented by the child process
    },
    {ok, {one_for_one, 5, 10}, [TrainSpec]}.  %% Include child spec here!

