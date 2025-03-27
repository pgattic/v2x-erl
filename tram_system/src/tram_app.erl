%%%-------------------------------------------------------------------
%% @doc tram public API
%% @end
%%%-------------------------------------------------------------------

-module(tram_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    tram_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
