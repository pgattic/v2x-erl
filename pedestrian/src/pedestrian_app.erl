%% OTP application module for pedestrian V2X process
-module(pedestrian_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Start the pedestrian supervisor
    pedestrian_sup:start_link().

stop(_State) ->
    ok.
