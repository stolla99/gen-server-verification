<a id="readme-top"></a>
<div align="center">
  <p>🧪
  </p>

  <h3 align="center">Test Results GenServer Suite</h3>

  <p align="center">
    <a href="https://stolla99.github.io/gen-server-verification/ct_logs/all_runs.html
">View CT Logs HTML Report</a>
  </p>
</div>

This directory contains the results from running **Common Test (CT)** for the generated Erlang code from the TLA⁺ specification of the GenServer. The tests were executed against the official OTP test suite, and the results are logged here for review.

## Execute test suite:
1. Clone OTP repository (if not already done)
2. Copy existing ```gen_server_SUITE.erl``` and data into a new suite
3. Run the following in the OTP root directory:
    ```shell
    ./otp_build configure
    make stdlib_test ARGS="-suite gen_server_NEW_SUITE"
    ```
4. The results will be in the `ct_logs` directory

## Links
[Erlang OTP GitHub](https://github.com/erlang/otp)
[Development guide](https://github.com/erlang/otp/blob/master/HOWTO/DEVELOPMENT.md)
<p align="right">(<a href="#readme-top">↑ back to top</a>)</p>