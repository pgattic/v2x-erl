%%%-------------------------------------------------------------------
%%% @doc
%%% Supervisor module for managing the `pedestrian` gen_server process.
%%% This supervisor uses a `one_for_one` strategy and ensures that the
%%% pedestrian process is restarted if it terminates unexpectedly.
%%%
%%% @author Elena Makin
%%% @version 1.0
%%% @since 2025-03-20
%%%-------------------------------------------------------------------
-module(pedestrian_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%%-------------------------------------------------------------------
%%% @doc
%%% Starts the pedestrian supervisor and links it to the calling process.
%%% Registers the supervisor locally under the module name.
%%%
%%% @spec start_link() -> {ok, pid()} | {error, any()}
%%%-------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%-------------------------------------------------------------------
%%% @doc
%%% Initializes the supervisor with a single `pedestrian` child process.
%%% Uses a `one_for_one` strategy with maximum 1 restart per 5 seconds.
%%%
%%% @spec init(Args :: list()) ->
%%%           {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} |
%%%           ignore | {error, any()}
%%%-------------------------------------------------------------------
init([]) ->
    %% Define the child spec for the pedestrian gen_server
    PedestrianChild = {
        pedestrian,               %% ID
        {pedestrian, start_link, []}, %% Start function
        permanent,                %% Restart strategy
        5000,                     %% Shutdown timeout (ms)
        worker,                   %% Process type
        [pedestrian]              %% Modules
    },

    %% Return supervision strategy and children
    {ok, {{one_for_one, 1, 5}, [PedestrianChild]}}.
