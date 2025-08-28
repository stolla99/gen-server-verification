-module(code_generation_app).
-behaviour(application).

-export([start/2, stop/1]).

print(ColorCode, Format, Data) ->
    io:format("\e[~sm===> ~s\e[0m", [ColorCode, io_lib:format(Format, Data)]).
print_success(Format, Data) -> 
    print("32", Format, Data).
print_error(Format, Data) ->
    print("31", Format, Data).
print_info(Format, Data) ->
    print("33", Format, Data).
print_normal(Format, Data) ->
    print("0", Format, Data).

try_function(F, Ar, Args, Mode) ->
    ok = gen_server_eqc_test:load(),
    case erlang:function_exported(gen_server_eqc_test, F, Ar) of
        true ->
            case Mode of
                quickcheck -> 
                    eqc:quickcheck(erlang:apply(gen_server_eqc_test, F, Args));
                test ->
                    erlang:apply(gen_server_eqc_test, F, Args);
                _ -> 
                    print_info("Mode ~p not supported for function ~p/~p(*~p)~n", [Mode, F, Ar, Args]),
                    false
            end;
        false ->
            print_error("Function gen_server_eqc_test:~p/~p(*~w) not exported in gen_server_eqc_test~n", [F, Ar, Args]),
            false
    end.

start(_StartType, StartArgs) ->
    io:format("Starting code_generation application with StartArgs: ~p~n", [StartArgs]),
    process_flag(trap_exit, true),
    Res = catch case StartArgs of 
        [debug] -> 
            debugger:start(),
            int:i(gen_server_eqc_test);
        [benchmark] ->
            Options = [verbose], % average, verbose
            tester:clear_file(gen_server),
            tester:clear_file(gen_server_behaviour_simple),
            Steps = lists:seq(1000, 10000, 500),
            [
                begin 
                    tester:test_mod(10, Calls, gen_server, [], Options),
                    tester:test_mod(10, Calls, gen_server_behaviour_simple, [], Options)
                end || Calls <- Steps
            ],
            benchmark;
        [Mode, Fs_Ars] -> 
            [try_function(F, Ar, Args, Mode) || {F, Ar, Args} <- Fs_Ars];
        _ -> 
            print_info("StartArgs ~p not supported~n", [StartArgs]),
            false
    end,
    case Res of 
        ok ->
            print_normal("No EQC test executed~n", []),
            {ok, self()};
        benchmark -> 
            print_success("Benchmarking done~n", []),
            {ok, self()};
        _ -> 
            print_success("Trace ~n~w~n", [Res]),
            {ok, self()}
    end.

stop(_State) ->
    ok.
