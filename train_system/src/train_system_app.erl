%%%-------------------------------------------------------------------
%% @doc train_system public API
%% @end
%%%-------------------------------------------------------------------

-module(train_system_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    train_system_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
