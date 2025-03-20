-module(reality_reporter_example).
-behaviour(gen_server).

%% API
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3, report_position/1, see/2]).

%% Starts the reporter as a local process.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

%% `REPORT_POSITION` EXAMPLE
report_position(Position) ->
    %% Calculate or retrieve your current position
    Result = rpc:cast('v2xmage@164.92.104.30', reality, report_position, [car, self(), Position]),
    io:format("Remote report_position result: ~p~n", [Result]),
    Result.

%% `SEE` EXAMPLE
see(Position, Radius) ->
    %% Calculate or retrieve your current position
    Result = rpc:call('v2xmage@164.92.104.30', reality, see, [Position, Radius]),
    io:format("Remote report_position result: ~p~n", [Result]),
    Result.

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

