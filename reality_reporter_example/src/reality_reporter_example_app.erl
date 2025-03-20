%%%-------------------------------------------------------------------
%% @doc reality_reporter_example public API
%% @end
%%%-------------------------------------------------------------------

-module(reality_reporter_example_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    reality_reporter_example_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
