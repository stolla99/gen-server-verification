<a id="readme-top"></a>

# GenServer Erlang generated code

This folder contains the automatically generated Erlang code for the GenServer, produced by the translation pipeline from the TLA⁺/PlusCal specification. The generated code adheres to the Erlang/OTP conventions and is designed to be directly usable in Erlang applications.

## Requirements
- Erlang/OTP 24 or higher
- Rebar3 build tool
- QuickCheck valid license (for running QuickCheck tests)

## Usage
1. To use the generated GenServer code, follow these steps:
Set the an execution option in ```code_generation.app.src``` to one of the following:
    ```erlang 
    % Execute only the benchmark:
    {mod, {code_generation_app, [benchmark]}},
    % Execute the QuickCheck tests:
    {mod, 
        {code_generation_app, [quickcheck, 
            [{state_persistency_test, 0, []}, 
            {mutual_exclusion_test, 0, []}]  ]}},
    % Execute one test only: 
    {mod, {code_generation_app, [test, [{state_persistency, 1, [100]}] ]}},
    ```
    More options can be defined as well. 
2. Execute the application in the rebar3 shell:
    ```shell
    rebar3 compile
    rebar3 shell
    ```
<p align="right">(<a href="#readme-top">↑ back to top</a>)</p>
