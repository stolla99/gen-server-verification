-module(tester).

-export([
    test_mod/5,
    benchmark/4,
    clear_file/1
]).

benchmark(0, Mod, Ref, Req) ->
    Mod:call(Ref, Req);
benchmark(Count, Mod, Ref, Req) ->
    Mod:call(Ref, Req),
    benchmark(Count - 1, Mod, Ref, Req).

clear_file(Mod) ->
    {ok, File} = file:open("benchmark_" ++ atom_to_list(Mod) ++ ".csv", [write]),
    file:write(File, "X Y\n"),
    file:close(File).

log_into_file(NumCalls, _CNT, ACC, _OWN, Mod) ->
    {ok, File} = file:open("benchmark_" ++ atom_to_list(Mod) ++ ".csv", [append]),
    file:write(File, io_lib:format("~p ~p\n", [NumCalls, ACC])),
    file:close(File).

test_mod(0, Calls, Mod, All, Options) ->
    case Options of
        [] -> 
            ok;
        [average] -> 
            Avg = lists:sum(All) / length(All),
            log_into_file(Calls, 0, Avg, 0, Mod),
            ok;
        _ -> 
            ok
    end;
test_mod(Samples, Calls, Mod, Result, Options) ->
    {ok, Ref} = Mod:start_link(gen_server_eqc_callbacks, [2], []),
    fprof:apply(tester, benchmark, [Calls, Mod, Ref, {step}]),
    fprof:profile(),
    fprof:analyse({dest, "fprof." ++ atom_to_list(Mod)}),
    {ok, Terms} = file:consult("fprof." ++ atom_to_list(Mod)),
    [_Options, [{totals, _CNT, ACC, _OWN}] | _Rest] = Terms,
    Mod:stop(Ref),
    ok = case Options of
        [] -> 
            ok;
        [verbose] -> 
            log_into_file(Calls, 0, ACC, 0, Mod),
            ok;
        _ -> 
            ok
    end,
    test_mod(Samples-1, Calls, Mod, [ACC | Result], Options).
