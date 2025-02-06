-module(v2x_monitor_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    %% Start this supervisor under a local registered name as well:
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% We choose a simple one_for_one strategy: if a monitor crashes, only that monitor is restarted.
    SupFlags = #{
        strategy => one_for_one,
        intensity => 3,
        period => 5
    },

    %% Define multiple children, each with a different "area" argument to monitor:start_link/1.
    ChildSpecs = [
        #{
           id => north_monitor,
           start => {monitor, start_link, ["North"]},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [monitor]
        },
        #{
           id => south_monitor,
           start => {monitor, start_link, ["South"]},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [monitor]
        },
        #{
           id => east_monitor,
           start => {monitor, start_link, ["East"]},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [monitor]
        }
        %% ... Add as many areas as you need
    ],

    %% Return the supervisor flags + the list of child specs
    {ok, {SupFlags, ChildSpecs}}.

