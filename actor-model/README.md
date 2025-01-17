
# Vehicle Actor Model

## Example Usage

1. Start the Aggregator:

    - `{ok, AggregatorPid} = traffic_aggregator:start_link().`

2. Start Vehicles:

    - `{ok, Vehicle1Pid} = vehicle_actor:start_link(vehicle_1, AggregatorPid).`
    - `{ok, Vehicle2Pid} = vehicle_actor:start_link(vehicle_2, AggregatorPid).`

3. Update Vehicle Positions:

    - `vehicle_actor:update_position(Vehicle1Pid, {{10, 15}, 60}). %% Position {10, 15}, Speed 60`
    - `vehicle_actor:update_position(Vehicle2Pid, {{12, 18}, 50}). %% Position {12, 18}, Speed 50`

4. Request Traffic Info:

    - `vehicle_actor:request_traffic_info(Vehicle1Pid).`

5. Traffic Aggregator Receives Updates:
    - Traffic updates are sent periodically from vehicles to the aggregator.
    - Traffic info requests query the aggregator for nearby vehicles or congestion data.

## Key Features of This Model

- Scalability:
    - Each vehicle is an independent actor, allowing for high scalability.
- Fault Tolerance:
    - Supervisors can monitor vehicle actors and restart them if necessary.
- Extensibility:
    - Additional features, such as collision detection or route optimization, can be added to the vehicle or aggregator actors without disrupting the system.

