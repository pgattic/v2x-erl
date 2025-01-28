
# Traffic Light Controller

Manages the state of a traffic light

## Usage

- Start the server
    - `c(traffic_light).`
    - `{ok, Pid} = traffic_light:start_link().`
    - `traffic_light:change_light(Pid).`
- Set/clear emergency mode
    - `traffic_light:trigger_emergency(Pid).`
    - `traffic_light:stop_emergency(Pid).`
- Stop the server
    - `traffic_light:stop(Pid).`

