-module(gen_server_behaviour_simple).

-export[
    abcast/2,abcast/3,
    call/2,call/3,
    cast/2,
    init_it/6,
    loop/8,
    reply/2,
    start/3,start/4,
    start_link/3,start_link/4,
    start_monitor/3,start_monitor/4,
    stop/1,stop/3,
    system_continue/3,
    system_get_state/1,
    system_terminate/4,
    wake_hib/9
].

-include("gen_server_behaviour_simple.hrl").


-spec loop_0(#state_genServer{procvar_ID::-1,var_raw_msg::'change_me',var_serverUp::'true',var_skipLoop::'false',var_skipLoopHibernate::'false',var_skipWait::boolean(),var_initResult::'change_me'}) -> no_return().
loop_0(State0) ->
    case
    State0#state_genServer.var_serverUp of
        true ->
            State1 = State0#state_genServer{var_smsg = ?const_null},
            case (not State1#state_genServer.var_skipWait andalso (util:isTimeout(State1#state_genServer.var_timeout) andalso (State1#state_genServer.var_hibernate =/= hibernate))) of
            true ->
                Message0 = receive Input ->
                    Input
                after State1#state_genServer.var_timeout -> 
                    timeout
                end,
                State2 = State1#state_genServer{var_raw_msg = Message0},
                case ((State2#state_genServer.var_raw_msg =:= timeout) orelse (hd(util:tuple_to_list(State2#state_genServer.var_raw_msg)) =/= system)) of
                true ->
                    State3 = State2#state_genServer{var_timeout = 'infinity'},
                    ok;
                false ->
                    State3 = State2,
                    ok
                end,
                State4 = State3,
                ok;
            false ->
                case (not State1#state_genServer.var_skipWait andalso (util:isInt(State1#state_genServer.var_hibernateAfterTimeout) andalso (State1#state_genServer.var_hibernate =/= hibernate))) of
                true ->
                    Message0 = receive Input ->
                        Input
                    after State1#state_genServer.var_hibernateAfterTimeout -> 
                        timeout
                    end,
                    State2 = State1#state_genServer{var_raw_msg = Message0},
                    case (util:isAtom(State2#state_genServer.var_raw_msg) andalso (State2#state_genServer.var_raw_msg =:= timeout)) of
                    true ->
                        State3 = State2#state_genServer{var_hibernate = 'hibernate'},
                        State4 = State3#state_genServer{var_skipLoopHibernate = true},
                        ok;
                    false ->
                        State4 = State2,
                        ok
                    end,
                    ok;
                false ->
                    case (not State1#state_genServer.var_skipWait andalso (State1#state_genServer.var_hibernate =:= hibernate)) of
                    true ->
                        proc_lib:hibernate(?MODULE, 'wake_hib', [State1#state_genServer.var_parent, State1#state_genServer.var_name, State1#state_genServer.var_serverState, State1#state_genServer.var_module, State1#state_genServer.var_hibernateAfterTimeout, State1#state_genServer.var_timeout, State1#state_genServer.var_hibernate, State1#state_genServer.var_continue, false]),
                        State2 = State1#state_genServer{var_skipLoop = true},
                        State4 = State2,
                        ok;
                    false ->
                        case (not State1#state_genServer.var_skipWait andalso (util:isTuple(State1#state_genServer.var_continue) andalso ((length(util:tuple_to_list(State1#state_genServer.var_continue)) =:= 2) andalso (hd(util:tuple_to_list(State1#state_genServer.var_continue)) =:= continue)))) of
                        true ->
                            M1 = State1#state_genServer.var_module,
                            State2 = State1#state_genServer{var_raw_msg = M1:handle_continue(lists:nth(2, util:tuple_to_list(State1#state_genServer.var_continue)), State1#state_genServer.var_serverState)},
                            State3 = State2#state_genServer{var_continue = ['continued']},
                            State4 = State3#state_genServer{var_skipLoop = false},
                            ok;
                        false ->
                            Message0 = receive Input ->
                                Input
                            end,
                            State2 = State1#state_genServer{var_raw_msg = Message0},
                            State3 = State2#state_genServer{var_skipWait = false},
                            State4 = State3#state_genServer{var_skipLoopHibernate = false},
                            ok
                        end,
                        ok
                    end,
                    ok
                end,
                ok
            end,
            case (not State4#state_genServer.var_skipLoop andalso not State4#state_genServer.var_skipLoopHibernate) of
            true ->
                case util:isTuple(State4#state_genServer.var_raw_msg) of
                true ->
                    State5 = State4#state_genServer{var_smsg = util:tuple_to_list(State4#state_genServer.var_raw_msg)},
                    ok;
                false ->
                    State5 = State4#state_genServer{var_smsg = State4#state_genServer.var_raw_msg},
                    ok
                end,
                case (util:isSeq(State5#state_genServer.var_smsg) andalso ((length(State5#state_genServer.var_smsg) =:= 3) andalso (lists:nth(1, State5#state_genServer.var_smsg) =:= system))) of
                true ->
                    case (State5#state_genServer.var_hibernate =:= hibernate) of
                    true ->
                        sys:handle_system_msg(lists:nth(3, State5#state_genServer.var_smsg), lists:nth(2, State5#state_genServer.var_smsg), State5#state_genServer.var_parent, ?MODULE, [], [State5#state_genServer.var_name, State5#state_genServer.var_serverState, State5#state_genServer.var_module, State5#state_genServer.var_timeout, State5#state_genServer.var_hibernate, State5#state_genServer.var_hibernateAfterTimeout], true),
                        ok;
                    false ->
                        sys:handle_system_msg(lists:nth(3, State5#state_genServer.var_smsg), lists:nth(2, State5#state_genServer.var_smsg), State5#state_genServer.var_parent, ?MODULE, [], [State5#state_genServer.var_name, State5#state_genServer.var_serverState, State5#state_genServer.var_module, State5#state_genServer.var_timeout, State5#state_genServer.var_hibernate, State5#state_genServer.var_hibernateAfterTimeout], false),
                        ok
                    end,
                    State6 = State5#state_genServer{var_hibernate = ""},
                    State11 = State6,
                    ok;
                false ->
                    case (util:isSeq(State5#state_genServer.var_smsg) andalso ((length(State5#state_genServer.var_smsg) =:= 3) andalso (lists:nth(1, State5#state_genServer.var_smsg) =:= 'EXIT'))) of
                    true ->
                        State6 = State5#state_genServer{var_hibernate = ""},
                        util:terminate(lists:nth(3, State6#state_genServer.var_smsg), [], State6#state_genServer.var_name, State6#state_genServer.procvar_ID, State6#state_genServer.var_smsg, State6#state_genServer.var_module, State6#state_genServer.var_serverState, []),
                        State11 = State6,
                        ok;
                    false ->
                        case (util:isSeq(State5#state_genServer.var_smsg) andalso ((length(State5#state_genServer.var_smsg) =:= 3) andalso (lists:nth(1, State5#state_genServer.var_smsg) =:= '$gen_call'))) of
                        true ->
                            M5 = State5#state_genServer.var_module,
                            Res0 = util:tuple_to_list(M5:handle_call(lists:nth(3, State5#state_genServer.var_smsg), lists:nth(2, State5#state_genServer.var_smsg), State5#state_genServer.var_serverState)),
                            case (util:isSeq(Res0) andalso ((length(Res0) =:= 3) andalso (lists:nth(1, Res0) =:= reply))) of
                            true ->
                                reply(lists:nth(2, State5#state_genServer.var_smsg), lists:nth(2, Res0)),
                                State6 = State5#state_genServer{var_serverState = lists:nth(3, Res0)},
                                State7 = State6#state_genServer{var_hibernate = ""},
                                State10 = State7,
                                ok;
                            false ->
                                case (util:isSeq(Res0) andalso ((length(Res0) =:= 4) andalso (lists:nth(1, Res0) =:= reply))) of
                                true ->
                                    reply(lists:nth(2, State5#state_genServer.var_smsg), lists:nth(2, Res0)),
                                    State6 = State5#state_genServer{var_serverState = lists:nth(3, Res0)},
                                    case util:isTimeout(lists:nth(4, Res0)) of
                                    true ->
                                        State7 = State6#state_genServer{var_timeout = lists:nth(4, Res0)},
                                        State10 = State7#state_genServer{var_hibernate = ""},
                                        ok;
                                    false ->
                                        case (util:isAtom(lists:nth(4, Res0)) andalso (lists:nth(4, Res0) =:= hibernate)) of
                                        true ->
                                            State7 = State6#state_genServer{var_hibernate = 'hibernate'},
                                            ok;
                                        false ->
                                            case (util:isTuple(lists:nth(4, Res0)) andalso ((length(util:tuple_to_list(lists:nth(4, Res0))) =:= 2) andalso (hd(util:tuple_to_list(lists:nth(4, Res0))) =:= continue))) of
                                            true ->
                                                State7 = State6#state_genServer{var_continue = lists:nth(4, Res0)},
                                                ok;
                                            false ->
                                                State7 = State6,
                                                ok
                                            end,
                                            ok
                                        end,
                                        State10 = State7,
                                        ok
                                    end,
                                    ok;
                                false ->
                                    case (util:isSeq(Res0) andalso ((length(Res0) =:= 2) andalso (lists:nth(1, Res0) =:= noreply))) of
                                    true ->
                                        State6 = State5#state_genServer{var_serverState = lists:nth(2, Res0)},
                                        State7 = State6#state_genServer{var_hibernate = ""},
                                        State10 = State7,
                                        ok;
                                    false ->
                                        case (util:isSeq(Res0) andalso ((length(Res0) =:= 3) andalso (lists:nth(1, Res0) =:= noreply))) of
                                        true ->
                                            State6 = State5#state_genServer{var_serverState = lists:nth(2, Res0)},
                                            case util:isTimeout(lists:nth(3, Res0)) of
                                            true ->
                                                State7 = State6#state_genServer{var_timeout = lists:nth(3, Res0)},
                                                State10 = State7#state_genServer{var_hibernate = ""},
                                                ok;
                                            false ->
                                                case (util:isAtom(lists:nth(3, Res0)) andalso (lists:nth(3, Res0) =:= hibernate)) of
                                                true ->
                                                    State7 = State6#state_genServer{var_hibernate = 'hibernate'},
                                                    ok;
                                                false ->
                                                    case (util:isTuple(lists:nth(3, Res0)) andalso ((length(util:tuple_to_list(lists:nth(3, Res0))) =:= 2) andalso (hd(util:tuple_to_list(lists:nth(3, Res0))) =:= continue))) of
                                                    true ->
                                                        State7 = State6#state_genServer{var_continue = lists:nth(3, Res0)},
                                                        ok;
                                                    false ->
                                                        State7 = State6,
                                                        ok
                                                    end,
                                                    ok
                                                end,
                                                State10 = State7,
                                                ok
                                            end,
                                            ok;
                                        false ->
                                            case (util:isSeq(Res0) andalso ((length(Res0) =:= 4) andalso (lists:nth(1, Res0) =:= stop))) of
                                            true ->
                                                reply(lists:nth(2, State5#state_genServer.var_smsg), lists:nth(3, Res0)),
                                                State6 = State5#state_genServer{var_hibernate = ""},
                                                util:terminate(lists:nth(2, Res0), [], State6#state_genServer.var_name, State6#state_genServer.procvar_ID, State6#state_genServer.var_smsg, State6#state_genServer.var_module, State6#state_genServer.var_serverState, []),
                                                ok;
                                            false ->
                                                case (util:isSeq(Res0) andalso ((length(Res0) =:= 3) andalso (lists:nth(1, Res0) =:= stop))) of
                                                true ->
                                                    util:terminate(lists:nth(2, Res0), [], State5#state_genServer.var_name, State5#state_genServer.procvar_ID, State5#state_genServer.var_smsg, State5#state_genServer.var_module, State5#state_genServer.var_serverState, []),
                                                    State6 = State5#state_genServer{var_hibernate = ""},
                                                    ok;
                                                false ->
                                                    case not util:isSeq(Res0) of
                                                    true ->
                                                        util:terminate({'bad_return_value', Res0}, [], State5#state_genServer.var_name, State5#state_genServer.procvar_ID, State5#state_genServer.var_smsg, State5#state_genServer.var_module, State5#state_genServer.var_serverState, []),
                                                        State6 = State5#state_genServer{var_hibernate = ""},
                                                        ok;
                                                    false ->
                                                        State6 = State5,
                                                        ok
                                                    end,
                                                    ok
                                                end,
                                                ok
                                            end,
                                            State10 = State6,
                                            ok
                                        end,
                                        ok
                                    end,
                                    ok
                                end,
                                ok
                            end,
                            State11 = State10,
                            ok;
                        false ->
                            case (util:isSeq(State5#state_genServer.var_smsg) andalso ((length(State5#state_genServer.var_smsg) =:= 2) andalso (lists:nth(1, State5#state_genServer.var_smsg) =:= '$gen_cast'))) of
                            true ->
                                M9 = State5#state_genServer.var_module,
                                Res0 = util:tuple_to_list(M9:handle_cast(lists:nth(2, State5#state_genServer.var_smsg), State5#state_genServer.var_serverState)),
                                case (util:isSeq(Res0) andalso ((length(Res0) =:= 2) andalso (lists:nth(1, Res0) =:= noreply))) of
                                true ->
                                    State6 = State5#state_genServer{var_serverState = lists:nth(2, Res0)},
                                    State7 = State6#state_genServer{var_hibernate = ""},
                                    State10 = State7,
                                    ok;
                                false ->
                                    case (util:isSeq(Res0) andalso ((length(Res0) =:= 3) andalso (lists:nth(1, Res0) =:= noreply))) of
                                    true ->
                                        State6 = State5#state_genServer{var_serverState = lists:nth(2, Res0)},
                                        case util:isTimeout(lists:nth(3, Res0)) of
                                        true ->
                                            State7 = State6#state_genServer{var_timeout = lists:nth(3, Res0)},
                                            State10 = State7#state_genServer{var_hibernate = ""},
                                            ok;
                                        false ->
                                            case (util:isAtom(lists:nth(3, Res0)) andalso (lists:nth(3, Res0) =:= hibernate)) of
                                            true ->
                                                State7 = State6#state_genServer{var_hibernate = 'hibernate'},
                                                ok;
                                            false ->
                                                case (util:isTuple(lists:nth(3, Res0)) andalso ((length(util:tuple_to_list(lists:nth(3, Res0))) =:= 2) andalso (hd(util:tuple_to_list(lists:nth(3, Res0))) =:= continue))) of
                                                true ->
                                                    State7 = State6#state_genServer{var_continue = lists:nth(3, Res0)},
                                                    ok;
                                                false ->
                                                    State7 = State6,
                                                    ok
                                                end,
                                                ok
                                            end,
                                            State10 = State7,
                                            ok
                                        end,
                                        ok;
                                    false ->
                                        case (util:isSeq(Res0) andalso ((length(Res0) =:= 3) andalso (lists:nth(1, Res0) =:= stop))) of
                                        true ->
                                            State6 = State5#state_genServer{var_serverState = lists:nth(3, Res0)},
                                            util:terminate(lists:nth(2, Res0), [], State6#state_genServer.var_name, State6#state_genServer.procvar_ID, State6#state_genServer.var_smsg, State6#state_genServer.var_module, State6#state_genServer.var_serverState, []),
                                            State7 = State6#state_genServer{var_hibernate = ""},
                                            ok;
                                        false ->
                                            State7 = State5,
                                            ok
                                        end,
                                        State10 = State7,
                                        ok
                                    end,
                                    ok
                                end,
                                State11 = State10,
                                ok;
                            false ->
                                case (util:isExported(State5#state_genServer.var_module,handle_info,2) andalso not ((util:isSeq(State5#state_genServer.var_continue) andalso (length(State5#state_genServer.var_continue) =:= 1)))) of
                                true ->
                                    M11 = State5#state_genServer.var_module,
                                    Res0 = util:tuple_to_list(M11:handle_info(State5#state_genServer.var_raw_msg, State5#state_genServer.var_serverState)),
                                    case (util:isSeq(Res0) andalso ((length(Res0) =:= 2) andalso (lists:nth(1, Res0) =:= noreply))) of
                                    true ->
                                        State6 = State5#state_genServer{var_serverState = lists:nth(2, Res0)},
                                        State7 = State6#state_genServer{var_hibernate = ""},
                                        State10 = State7,
                                        ok;
                                    false ->
                                        case (util:isSeq(Res0) andalso ((length(Res0) =:= 3) andalso (lists:nth(1, Res0) =:= noreply))) of
                                        true ->
                                            State6 = State5#state_genServer{var_serverState = lists:nth(2, Res0)},
                                            case util:isTimeout(lists:nth(3, Res0)) of
                                            true ->
                                                State7 = State6#state_genServer{var_timeout = lists:nth(3, Res0)},
                                                State10 = State7#state_genServer{var_hibernate = ""},
                                                ok;
                                            false ->
                                                case (util:isAtom(lists:nth(3, Res0)) andalso (lists:nth(3, Res0) =:= hibernate)) of
                                                true ->
                                                    State7 = State6#state_genServer{var_hibernate = 'hibernate'},
                                                    ok;
                                                false ->
                                                    case (util:isTuple(lists:nth(3, Res0)) andalso ((length(util:tuple_to_list(lists:nth(3, Res0))) =:= 2) andalso (hd(util:tuple_to_list(lists:nth(3, Res0))) =:= continue))) of
                                                    true ->
                                                        State7 = State6#state_genServer{var_continue = lists:nth(3, Res0)},
                                                        ok;
                                                    false ->
                                                        State7 = State6,
                                                        ok
                                                    end,
                                                    ok
                                                end,
                                                State10 = State7,
                                                ok
                                            end,
                                            ok;
                                        false ->
                                            case (util:isSeq(Res0) andalso ((length(Res0) =:= 3) andalso (lists:nth(1, Res0) =:= stop))) of
                                            true ->
                                                State6 = State5#state_genServer{var_serverState = lists:nth(3, Res0)},
                                                State7 = State6#state_genServer{var_hibernate = ""},
                                                util:terminate(lists:nth(2, Res0), [], State7#state_genServer.var_name, State7#state_genServer.procvar_ID, State7#state_genServer.var_smsg, State7#state_genServer.var_module, lists:nth(3, Res0), []),
                                                ok;
                                            false ->
                                                State7 = State5,
                                                ok
                                            end,
                                            State10 = State7,
                                            ok
                                        end,
                                        ok
                                    end,
                                    State11 = State10,
                                    ok;
                                false ->
                                    case (util:isSeq(State5#state_genServer.var_smsg) andalso ((length(State5#state_genServer.var_smsg) =:= 2) andalso (lists:nth(1, State5#state_genServer.var_smsg) =:= noreply))) of
                                    true ->
                                        State6 = State5#state_genServer{var_serverState = lists:nth(2, State5#state_genServer.var_smsg)},
                                        State7 = State6#state_genServer{var_hibernate = ""},
                                        State10 = State7#state_genServer{var_continue = []},
                                        State11 = State10,
                                        ok;
                                    false ->
                                        case (util:isSeq(State5#state_genServer.var_smsg) andalso ((length(State5#state_genServer.var_smsg) =:= 3) andalso (lists:nth(1, State5#state_genServer.var_smsg) =:= noreply))) of
                                        true ->
                                            State6 = State5#state_genServer{var_serverState = lists:nth(2, State5#state_genServer.var_smsg)},
                                            case util:isTimeout(lists:nth(3, State6#state_genServer.var_smsg)) of
                                            true ->
                                                State7 = State6#state_genServer{var_timeout = lists:nth(3, State6#state_genServer.var_smsg)},
                                                State10 = State7#state_genServer{var_hibernate = ""},
                                                State11 = State10#state_genServer{var_continue = []},
                                                ok;
                                            false ->
                                                case (util:isAtom(lists:nth(3, State6#state_genServer.var_smsg)) andalso (lists:nth(3, State6#state_genServer.var_smsg) =:= hibernate)) of
                                                true ->
                                                    State7 = State6#state_genServer{var_hibernate = 'hibernate'},
                                                    State10 = State7#state_genServer{var_continue = []},
                                                    ok;
                                                false ->
                                                    case (util:isTuple(lists:nth(3, State6#state_genServer.var_smsg)) andalso ((length(util:tuple_to_list(lists:nth(3, State6#state_genServer.var_smsg))) =:= 2) andalso (hd(util:tuple_to_list(lists:nth(3, State6#state_genServer.var_smsg))) =:= continue))) of
                                                    true ->
                                                        State7 = State6#state_genServer{var_continue = lists:nth(3, State6#state_genServer.var_smsg)},
                                                        ok;
                                                    false ->
                                                        State7 = State6,
                                                        ok
                                                    end,
                                                    State10 = State7,
                                                    ok
                                                end,
                                                State11 = State10,
                                                ok
                                            end,
                                            ok;
                                        false ->
                                            case (util:isSeq(State5#state_genServer.var_smsg) andalso ((length(State5#state_genServer.var_smsg) =:= 3) andalso (lists:nth(1, State5#state_genServer.var_smsg) =:= stop))) of
                                            true ->
                                                State6 = State5#state_genServer{var_serverState = lists:nth(3, State5#state_genServer.var_smsg)},
                                                State7 = State6#state_genServer{var_hibernate = ""},
                                                util:terminate(lists:nth(2, State7#state_genServer.var_smsg), [], State7#state_genServer.var_name, State7#state_genServer.procvar_ID, State7#state_genServer.var_smsg, State7#state_genServer.var_module, lists:nth(3, State7#state_genServer.var_smsg), []),
                                                State10 = State7#state_genServer{var_continue = []},
                                                ok;
                                            false ->
                                                State10 = State5,
                                                ok
                                            end,
                                            State11 = State10,
                                            ok
                                        end,
                                        ok
                                    end,
                                    ok
                                end,
                                ok
                            end,
                            ok
                        end,
                        ok
                    end,
                    ok
                end,
                ok;
            false ->
                State11 = State4,
                ok
            end,
            State12 = State11#state_genServer{var_initResult = ?const_null},
            State13 = State12#state_genServer{var_raw_msg = ?const_null},
            State14 = State13#state_genServer{var_skipLoop = false},
            State15 = State14#state_genServer{var_skipLoopHibernate = false},
            loop_0(State15);
        false ->
            State0
    end.

-spec loop(_,_,_,_,_,_,_,_) -> no_return().
loop(Name, Parent, State, Mod, HibernateAfterTimeout, Timeout, Hibernate, Continue) ->
    loop_0(#state_genServer{
        var_serverState = State,
         var_serverUp = true,
         var_name = Name,
         var_parent = Parent,
         var_module = Mod,
         var_hibernateAfterTimeout = HibernateAfterTimeout,
         var_timeout = Timeout,
         var_hibernate = Hibernate,
         var_continue = Continue
    }).

%%%=========================================================================
%%% 
%%% Exports available for calling
%%% 
%%%=========================================================================
-spec wake_hib(_,_,_,_,_,_,_,_,_) -> no_return().
wake_hib(Parent, Name, State, Mod, HibernateAfterTimeout, Timeout, Hibernate, Continue, _Debug) ->
    SkipWait = true,
    loop_0(#state_genServer{var_parent = Parent, var_name = Name, var_serverState = State, var_module = Mod, var_hibernateAfterTimeout = HibernateAfterTimeout, var_timeout = Timeout, var_continue = Continue, var_skipWait = SkipWait, var_serverUp = true, var_hibernate = Hibernate}).
    
-spec system_terminate(_,_,_,[any(),...]) -> any().
system_terminate(Reason, _Parent, Debug, Args) ->
    util:terminate(Reason, [], lists:nth(1, Args), [], [], lists:nth(3, Args), lists:nth(2, Args), Debug).
    
-spec system_continue(_,_,[any(),...]) -> any().
system_continue(Parent, _Debug, Args) ->
    util:continue(lists:nth(1, Args), Parent, lists:nth(2, Args), lists:nth(3, Args), lists:nth(6, Args), lists:nth(4, Args), lists:nth(5, Args), []).
    
-spec system_get_state(_) -> any().
system_get_state(Args) ->
    util:system_get_state(Args).
    
-spec init_it(_,_,_,_,_,_) -> any().
init_it(Starter, Parent, Name, Module, Args, Options) ->
    util:init_it(Starter, Parent, Name, Module, Args, Options).
    
-spec start_monitor({'global',_} | {'local',atom()} | {'via',atom(),_},atom(),_,[{'debug',['log' | 'statistics' | 'trace' | {_,_}]} | {'hibernate_after','infinity' | non_neg_integer()} | {'spawn_opt',['link' | 'monitor' | {_,_}]} | {'timeout','infinity' | non_neg_integer()}]) -> any().
start_monitor(ServerName, Module, Args, Options) ->
    case (util:isSeq(Options) andalso (util:isAtom(Module) andalso util:isTuple(ServerName))) of
    true ->
        TrVal0 = util:tuple_to_list(gen:start(?MODULE, 'monitor', ServerName, Module, Args, Options)),
        util:list_to_tuple(TrVal0);
    false ->
        TrVal0 = util:tuple_to_list(erlang:error('badarg', [ServerName, Module, Args, Options])),
        util:list_to_tuple(TrVal0)
    end.
    
-spec start_monitor(atom(),_,[{'debug',['log' | 'statistics' | 'trace' | {_,_}]} | {'hibernate_after','infinity' | non_neg_integer()} | {'spawn_opt',['link' | 'monitor' | {_,_}]} | {'timeout','infinity' | non_neg_integer()}]) -> any().
start_monitor(Module, Args, Options) ->
    case (util:isSeq(Options) andalso util:isAtom(Module)) of
    true ->
        TrVal0 = util:tuple_to_list(gen:start(?MODULE, 'monitor', Module, Args, Options)),
        util:list_to_tuple(TrVal0);
    false ->
        TrVal0 = util:tuple_to_list(erlang:error('badarg', [Module, Args, Options])),
        util:list_to_tuple(TrVal0)
    end.
    
-spec start_link({'global',_} | {'local',atom()} | {'via',atom(),_},atom(),_,[{'debug',['log' | 'statistics' | 'trace' | {_,_}]} | {'hibernate_after','infinity' | non_neg_integer()} | {'spawn_opt',['link' | 'monitor' | {_,_}]} | {'timeout','infinity' | non_neg_integer()}]) -> any().
start_link(ServerName, Module, Args, Options) ->
    case (util:isSeq(Options) andalso (util:isAtom(Module) andalso util:isTuple(ServerName))) of
    true ->
        TrVal0 = util:tuple_to_list(gen:start(?MODULE, 'link', ServerName, Module, Args, Options)),
        util:list_to_tuple(TrVal0);
    false ->
        TrVal0 = util:tuple_to_list(erlang:error('badarg', [ServerName, Module, Args, Options])),
        util:list_to_tuple(TrVal0)
    end.
    
-spec start_link(atom(),_,[{'debug',['log' | 'statistics' | 'trace' | {_,_}]} | {'hibernate_after','infinity' | non_neg_integer()} | {'spawn_opt',['link' | 'monitor' | {_,_}]} | {'timeout','infinity' | non_neg_integer()}]) -> any().
start_link(Module, Args, Options) ->
    case (util:isSeq(Options) andalso util:isAtom(Module)) of
    true ->
        TrVal0 = util:tuple_to_list(gen:start(?MODULE, 'link', Module, Args, Options)),
        util:list_to_tuple(TrVal0);
    false ->
        TrVal0 = util:tuple_to_list(erlang:error('badarg', [Module, Args, Options])),
        util:list_to_tuple(TrVal0)
    end.
    
-spec start({'global',_} | {'local',atom()} | {'via',atom(),_},atom(),_,[{'debug',['log' | 'statistics' | 'trace' | {_,_}]} | {'hibernate_after','infinity' | non_neg_integer()} | {'spawn_opt',['link' | 'monitor' | {_,_}]} | {'timeout','infinity' | non_neg_integer()}]) -> any().
start(ServerName, Module, Args, Options) ->
    case (util:isSeq(Options) andalso (util:isAtom(Module) andalso util:isTuple(ServerName))) of
    true ->
        TrVal0 = util:tuple_to_list(gen:start(?MODULE, 'nolink', ServerName, Module, Args, Options)),
        util:list_to_tuple(TrVal0);
    false ->
        TrVal0 = util:tuple_to_list(erlang:error('badarg', [ServerName, Module, Args, Options])),
        util:list_to_tuple(TrVal0)
    end.
    
-spec start(atom(),_,[{'debug',['log' | 'statistics' | 'trace' | {_,_}]} | {'hibernate_after','infinity' | non_neg_integer()} | {'spawn_opt',['link' | 'monitor' | {_,_}]} | {'timeout','infinity' | non_neg_integer()}]) -> any().
start(Module, Args, Options) ->
    case (util:isSeq(Options) andalso util:isAtom(Module)) of
    true ->
        TrVal0 = util:tuple_to_list(gen:start(?MODULE, 'nolink', Module, Args, Options)),
        util:list_to_tuple(TrVal0);
    false ->
        TrVal0 = util:tuple_to_list(erlang:error('badarg', [Module, Args, Options])),
        util:list_to_tuple(TrVal0)
    end.
    
-spec stop(atom() | pid() | {_,_} | {'via',atom(),_},_,'infinity' | non_neg_integer()) -> 'ok' | reference() | {'ok',_}.
stop(ServerRef, Reason, Timeout) ->
    gen:stop(ServerRef, Reason, Timeout).
    
-spec stop(atom() | pid() | {_,_} | {'via',atom(),_}) -> 'ok' | reference() | {'ok',_}.
stop(ServerRef) ->
    stop(ServerRef, normal, infinity).
    
-spec call(_,_) -> any().
call(ServerRef, Request) ->
    TrValTemp0 = util:tuple_to_list(catch gen:call(ServerRef, '$gen_call', Request, ?const_default_timeout)),
    case (lists:nth(1, TrValTemp0) =:= ok) of
    true ->
        lists:nth(2, TrValTemp0);
    false ->
        case (lists:nth(1, TrValTemp0) =:= 'EXIT') of
        true ->
            erlang:exit({lists:nth(2, TrValTemp0), {?MODULE, 'call', [ServerRef, Request]}});
        false ->
            ok
        end
    end.
    
-spec call(_,_,_) -> any().
call(ServerRef, Request, Timeout) ->
    TrValTemp0 = util:tuple_to_list(catch gen:call(ServerRef, '$gen_call', Request, Timeout)),
    case (lists:nth(1, TrValTemp0) =:= ok) of
    true ->
        lists:nth(2, TrValTemp0);
    false ->
        case (lists:nth(1, TrValTemp0) =:= 'EXIT') of
        true ->
            erlang:exit({lists:nth(2, TrValTemp0), {?MODULE, 'call', [ServerRef, Request]}});
        false ->
            ok
        end
    end.
    
-spec reply({_,_},_) -> 'ok'.
reply(Client, Reply) ->
    gen:reply(Client, Reply).
    
-spec cast(_,_) -> 'ok'.
cast(Dest, Request) ->
    case util:isTuple(Dest) of
    true ->
        RValTemp = util:tuple_to_list(Dest),
        case (lists:nth(1, RValTemp) =:= global) of
        true ->
            global:send(lists:nth(2, RValTemp), util:list_to_tuple(['$gen_cast', Request]));
        false ->
            case (lists:nth(1, RValTemp) =:= via) of
            true ->
                Module = lists:nth(2, RValTemp),
                Module:send(lists:nth(3, RValTemp), util:list_to_tuple(['$gen_cast', Request]));
            false ->
                erlang:send(Dest, util:list_to_tuple(['$gen_cast', Request]))
            end
        end;
    false ->
        erlang:send(Dest, util:list_to_tuple(['$gen_cast', Request]))
    end,
    ok.
    
-spec abcast([any()],_,_) -> 'abcast'.
abcast(Nodes, Name, Request) ->
    AbcastNodes = Nodes,
    case (length(Nodes) > 0) of
        true ->
            RValTemp1 = hd(AbcastNodes),
            RValTemp = tl(AbcastNodes),
            erlang:send(util:list_to_tuple([Name, RValTemp1]), util:list_to_tuple(['$gen_cast', Request])),
            abcast(RValTemp, Name, Request);
        false ->
            abcast
    end.
    
-spec abcast(_,_) -> 'abcast'.
abcast(Name, Request) ->
    abcast(([util:getNode()] ++ util:getNodes()), Name, Request).
    