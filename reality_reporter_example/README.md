
# Example of reporting/seeing through Reality module

In the system that you want to run `reality` in (probably the "v2x-mage" droplet):

- `cd reality`
- `rebar3 shell --name v2xmage@164.92.104.30 --setcookie elvenmage`
    - If the IP address of the system is something else, correct it in this command and in `src/reality_reporter_example.erl`, lines 19 and 26.

In the system that you want to talk to `reality`:

- `cd reality_reporter_example`
- `rebar3 shell --name yeet@164.92.90.193 --setcookie elvenmage`
- To report: `reality_reporter_example:report_position_over_internet({69, 420}).`
- To "see": `reality_reporter_example:see({68, 415}, 20).`

