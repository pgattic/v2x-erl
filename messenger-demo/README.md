# messenger-demo

## How to Use

1. Start Erlang on Both Machines:

    - `erl -name node1@<local-ip> -setcookie secretcookie`

2. Compile the Code:

    - `c(messenger).`

3. Start the GenServer:

    - `messenger:start_link().`

4. Connect Nodes: Use the `net_adm:ping/1` command to connect the nodes:

    - `net_adm:ping('node2@<remote-ip>').`

5. Send a Message: From node1 to node2:

    - `messenger:send_message('node2@<remote-ip>', "Hello from node1!").`

6. Receive a Message: You can simulate checking for a message:

    - `messenger:receive_message(whereis(messenger)).`

