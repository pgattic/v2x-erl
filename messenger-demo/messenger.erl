-module(messenger).
-behaviour(gen_server).

%% API
-export([start_link/0, send_message/2, receive_message/1]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Starts the gen_server process
start_link() ->
    gen_server:start_link({local, messenger}, messenger, [], []).

%% Sends a message to a remote node
send_message(RemoteNode, Message) ->
    %% Assumes the messenger gen_server is running on the remote node
    gen_server:call({messenger, RemoteNode}, {send_message, Message}).

%% Handles receiving a message
receive_message(Pid) ->
    gen_server:call(Pid, get_message).

%% Callbacks

%% Initialize the server state
init([]) ->
    io:format("Messenger started on ~p~n", [node()]),
    {ok, []}.

%% Handle synchronous calls
handle_call({send_message, Message}, _From, State) ->
    io:format("Received message: ~p~n", [Message]),
    {reply, ok, State};

handle_call(get_message, _From, State) ->
    %% For simplicity, this example doesn't keep messages.
    %% You can expand this to maintain a message queue in the State.
    {reply, "No new messages", State};

handle_call(_Request, _From, State) ->
    {reply, error, State}.

%% Handle asynchronous messages
handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle other messages
handle_info(_Info, State) ->
    {noreply, State}.

%% Cleanup on termination
terminate(_Reason, _State) ->
    ok.

%% Code change (hot upgrades)
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

