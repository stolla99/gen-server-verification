-module(gen_server_eqc_test).

-include("gen_server_eqc_test.hrl").

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").


-compile([
    export_all,
    nowarn_export_all,
    parse_transform, 
    eqc_parallelize
]).

perform_op(Op, CallerPid) ->
    gen_server_behaviour_simple:call(CallerPid, {Op}).

load() ->
    ok.

-spec state_persistency(integer(), list(term())) -> integer().
state_persistency(NUM_PIDS, OP_LIST) -> 
    OldFlag = process_flag(trap_exit, true),
    {ok, ServerPid} = gen_server_behaviour_simple:start_link(gen_server_eqc_callbacks, [], []),
    CallerPids = [
        begin 
            {ok, CallerPid} = gen_server_behaviour_simple:start(gen_server_eqc_callbacks, [caller, ServerPid, self()], []),
            perform_op(lists:nth(Num, OP_LIST), CallerPid),
            CallerPid
        end || Num <- lists:seq(1, NUM_PIDS)],
    _ = [   
        begin 
            Res = receive ok -> 
                _ = gen_server_behaviour_simple:stop(Pid),
                success
            after 5000 ->
                fail
            end,
            false = erlang:is_process_alive(Pid),
            Res
        end  || Pid <- CallerPids],
    State = sys:get_state(ServerPid),
    ok = gen_server_behaviour_simple:stop(ServerPid),
    false = erlang:is_process_alive(ServerPid),
    _ = process_flag(trap_exit, OldFlag),
    State.

state_persistency_test() ->
    ?FORALL(
        OPS, 
        ?LET(NUM, choose(100, 1000), vector(NUM, oneof([call_increment, cast_increment, info_increment]))),
        begin 
            NUM_PIDS = length(OPS),
            state_persistency(NUM_PIDS, OPS) =:= NUM_PIDS
        end
    ).

initial_state() ->
    {ok, Ref} = gen_server_behaviour_simple:start_link(gen_server_eqc_callbacks, [], []),
    #state{seen_states =  [], server_ref = Ref}.

precondition_common(#state{seen_states = _, server_ref = ServerRef}, _Calls) ->
    erlang:is_process_alive(ServerRef).

call_command(#state{seen_states = _, server_ref = ServerRef}) ->
    {call, gen_server_behaviour_simple, call, [ServerRef, {increment_reply}]}.
cast_command(#state{seen_states = _, server_ref = ServerRef}) ->
    {call, gen_server_behaviour_simple, cast, [ServerRef, {increment}]}.

call_next(#state{seen_states = SeenStates, server_ref = ServerRef}, Reply, _Meta) when is_integer(Reply) ->
    #state{seen_states = [Reply | SeenStates], server_ref = ServerRef};
call_next(S, _ = {var, _}, _) ->
    S.

cast_next(#state{seen_states = SeenStates, server_ref = ServerRef}, ok, _Meta) ->
    #state{seen_states = SeenStates, server_ref = ServerRef};
cast_next(S, _ = {var, _}, _) ->
    S.

call_post(#state{seen_states = SeenStates, server_ref = _ServerRef}, _Args, NewState) ->
    not lists:member(NewState, SeenStates).

postcondition_common(#state{seen_states = SeenStates, server_ref = _ServerRef}, _Calls, _Res) ->
    Desc = fun(A, B) -> A > B end, 
    lists:sort(Desc, SeenStates) =:= SeenStates.

mutual_exclusion_test() ->
    ?FORALL(
        CMDS,
        parallel_commands(?MODULE), 
        begin 
            {_H, _S, R} = run_parallel_commands(CMDS),
            case R of
                ok ->
                    true;
                _ -> 
                    false
            end
        end).
