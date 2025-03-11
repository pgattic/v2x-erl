%%%-------------------------------------------------------------------
%% @doc Reality top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(reality_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%--------------------------------------------------------------------
%% API Functions
%%--------------------------------------------------------------------

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------
%% Supervisor Initialization
%%--------------------------------------------------------------------

init([]) ->
  %% Define the child specification for the reality gen_server.
  ChildSpecs = [
    {
      reality,  %% Unique child id
      {reality, start_link, []},  %% Module, function and arguments to start the child
      permanent,  %% Restart strategy: always restart
      5000,       %% Shutdown timeout in milliseconds
      worker,     %% Process type
      [reality]   %% Modules implemented by the child process
    }
  ],
  %% Use one_for_one strategy: if a child terminates, only that child is restarted.
  {ok, {{one_for_one, 5, 10}, ChildSpecs}}.

