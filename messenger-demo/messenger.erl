-module(messenger).
-export([start/0, start/1, send_message/2, receive_message/0]).

%% Starts the messenger server on the current node
start() ->
    register(messenger, spawn(fun loop/0)),
    io:format("Messenger started on ~p~n", [node()]).

%% Starts the messenger server and registers it with a specific name
start(Name) ->
    register(Name, spawn(fun loop/0)),
    io:format("Messenger ~p started on ~p~n", [Name, node()]).

%% Sends a message to a process on another node
send_message(RemoteNode, Message) ->
    %% The remote node must have started the `messenger` process.
    {messenger, RemoteNode} ! {self(), Message},
    io:format("Message sent to ~p: ~p~n", [RemoteNode, Message]).

%% Waits to receive a message
receive_message() ->
    receive
        {From, Message} ->
            io:format("Message received from ~p: ~p~n", [From, Message])
    after 10000 -> %% Timeout after 10 seconds
        io:format("No message received~n")
    end.

%% Internal loop for the messenger process
loop() ->
    receive
        {From, Message} ->
            io:format("Received message: ~p from ~p~n", [Message, From]),
            From ! {self(), "Message received!"};
        _ ->
            io:format("Unknown message received~n")
    end,
    loop().

