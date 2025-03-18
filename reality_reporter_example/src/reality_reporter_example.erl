-module(reality_reporter_example).
-behaviour(gen_server).

%% API
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3, report_position_over_internet/1]).

%% Starts the reporter as a local process.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% Optionally, wait until the reality server is up.
    %% Schedule a periodic report every 1000 milliseconds (1 second)
    %timer:send_interval(1000, report_position),
    {ok, #{}}.

%% In your reporter module, instead of a local call, use rpc:call/4
report_position_over_internet(Position) ->
    %% Calculate or retrieve your current position
    %% Replace 'v2x@realityhost.example.com' with the remote node's name
    Result = rpc:call('v2xmage@164.92.104.30', reality, report_position, [car, self(), Position]),
    io:format("Remote report_position result: ~p~n", [Result]),
    Result.


%% On each timer tick, compute your current position (here, hard-coded) 
%% and report it to the reality module.
handle_info(report_position, State) ->
    %% Example: obtain your position from a sensor or calculation.
    Position = {10, 20},
    %% Report position to the v2x system.
    reality:report_position(car, self(), Position),
    io:format("Reported position: ~p~n", [Position]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

