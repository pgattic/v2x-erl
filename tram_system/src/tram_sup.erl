%%%-------------------------------------------------------------------
%% @doc tram top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tram_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    TramServer = {tram_server,
                  {tram_server, start_link, []},
                  permanent,
                  5000,
                  worker,
                  [tram_server]},
    {ok, {{one_for_one, 3, 10}, [TramServer]}}.

%% internal functions
