
# Vehicle Monitor + Supervisor

This code keeps track of the locations of vehicles, and can cover many areas.

## Usage

### With Supervisor

- Ensure both `monitor` and `v2x_monitor_sup` modules are compiled.
- Start the supervisor: `v2x_monitor_sup:start_link()`
    - This will automatically start all child processes specified in the `ChildSpecs` variable in the monitor's `init` function.
- Start sending data to one of the child processes: `gen_server:cast(monitor_North, {update_position, "Preston", {100, 250}}).`
- Retrieve data from a child process: `gen_server:call(monitor_North, {get_position, "Preston"}).`

### Single Monitor

The `monitor` module has a separate `start_link/0` function that takes no parameters, for a global instance. This should not be used in production. Please start a supervisor in production instead.

- Ensure the `monitor` module is compiled.
- `monitor:start_link().`
- `monitor:update_position("Preston", {90, 76}).`
- `monitor:get_position("Preston").`

