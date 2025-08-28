------------------------------- MODULE gen_server_behaviour_simple -------------------------------

EXTENDS Naturals, Sequences, TLC, FiniteSets, Typecheck, Timeouts, GenServerUtil, ShiVizExt, SymmConfig
CONSTANT WorkerClients, ControlClients, TrafficPerClient, Server, TimerSet, SERVERID, syncStart

(*--algorithm gen_server
  variables
    null;
    rootModule =        a"?MODULE";                    (* Used for init callback i.e. maps to ?MODULE in erlang *)
    uuid =              100;
  
    queues =            [id \in (NodeSet ++ TimerSet) |-> <<>>];
    signals =           [id \in NodeSet |-> {}];
    mRefTable =         [id \in (uuid)..(uuid + Cardinality(AllClients) + Cardinality(AllClients) * TrafficPerClient) |-> null];
    processesLinked =   {};
    processesMonitored = {};
    
    calls =             [id \in NodeSet |-> <<>>];
    results =           [id \in NodeSet |-> <<>>];

    timers =            [id \in NodeSet |-> [isReached |-> FALSE, running |-> FALSE]];
    
    clientUp =          [id \in AllClients |-> TRUE];
    openConnection =    [id \in NodeSet |-> FALSE];
    wasStopped =        [id \in ServerSet |-> FALSE];
    
    rVal =              [id \in NodeSet |-> [result |-> ""]];
    rValTemp =          [id \in NodeSet |-> null];
    rValTemp1 =         [id \in NodeSet |-> null];
    rValTemp2 =         [id \in NodeSet |-> null];
    rValTemp3 =         [id \in NodeSet |-> null];
    
    (* Just for checking and verification *)
    trace =             [id \in WorkerClients |-> <<>>];
    callTrace =         [id \in WorkerClients |-> <<>>];
    
    replyTrace =        <<>>;
    globalClock =       0;
    
    (* Shared client variables *)
    reqStart     = [id \in AllClients |-> FALSE];
    reqTerminate = [id \in AllClients |-> FALSE];
    
    (* For syncing start and stop *)
    canStart = ~syncStart;
  define
    ServerSet     == Server
    NodeSet       == ServerSet ++ (WorkerClients ++ ControlClients)
    AllClients    == WorkerClients ++ ControlClients
    
    unalias(mRef) == mRefTable[mRef]
    receiveSelective(at, matcher(_)) == (CHOOSE q \in toSetOfRecs(queues[at]): matcher(q))
    receivedDown(at, r) == \E e \in toSetOfSeqs(queues[at]): isSeq(e) /\ e[1] = "DOWN" /\ e[2] = r
    list_to_tuple(in) == in
    tuple_to_list(in) == in
    
    default_timeout == 5000
  end define;
  
  (*************************************************************************)
  (* Utils                                                                 *)
  (*************************************************************************)
  macro sendTo(what, to) begin
    queues := [queues EXCEPT ![to] = @ (+) what];
  end macro;
  
  §redirect(module=erlang, function=send, into=)
  macro doSend(dest, msg) begin
    if isSeq(dest) then
        sendTo(msg, dest[1]); (* Ignore here {name, node} just take name for message sending *)
    else
        sendTo(msg, dest);
    end if;
  end macro;
  
  §redirect(module=global, function=send, into=)
  macro globalSend(dest, msg) begin
    sendTo(msg, dest);
  end macro;
  
  §redirect(module=$module, function=send, into=)
  macro modSend(dest, msg) begin
    sendTo(msg, dest);
  end macro;
  
  macro postCleanup() begin
    initResult := null;
    rValTemp[self]:= null;
    raw_msg := null;
    skipLoop := FALSE;
    skipLoopHibernate := FALSE;
  end macro;
  
  macro preCleanup() begin
    rVal[self] := null;
    smsg := null;
  end macro;
  
  macro alias(mRef, Pid) begin
    mRefTable[mRef] := Pid
  end macro;
  
  macro reset() begin
    monitorSuccess := FALSE;
  end macro;
  
  macro signal(who, what) begin
    signals := [signals EXCEPT ![who] = @ ++ what];
  end macro;
  
  §redirect(module=erlang, function=error, into=rVal)
  macro throwError(cause, context) begin
    if context = null then
        rVal[self] := [
            result  |-> "error",
            reason  |-> cause, 
            context |-> <<"", <<>>>>
        ];
    else
        rVal[self] := [
            result  |-> "error",
            reason  |-> cause, 
            context |-> context
        ];
    end if;
  end macro;
  
  §redirect(module=erlang, function=exit, into=)
  macro exit1(Reason) begin
    clientUp[self] := FALSE;
    goto "Done";
  end macro;
  
  (*************************************************************************)
  (* Message sending and receiving                                         *)
  (*************************************************************************)
  macro monitor(Pid) begin
                        signal(Pid, {<<"MONITOR", uuid, self>>});
                        mRef := uuid;
                        alias(mRef, Pid);
                        uuid := uuid + 1;
  MONITOR_WAIT_FOR_ACK: await ~openConnection[Pid] \/ \E s \in signals[self]: s[1] = "ACK" /\ s[2] = mRef;
                        if ~openConnection[Pid] /\ ~(\E s \in signals[self]: s[1] = "ACK" /\ s[2] = mRef) then
                            queues := [queues EXCEPT ![self] = @ (+) <<"DOWN", mRef, Pid, "no_proc">>];
                            signals[Pid] := signals[Pid] \ {<<"MONITOR", mRef, self>>}; (* Delete that before server acks *)
                        else
                            signals[self] := signals[self] \ {<<"ACK", mRef>>};
                            monitorSuccess := TRUE;
                        end if;
                        mRefRes := mRef;
  end macro;
  
  macro demonitor(ref) begin
    await receivedDown(self, ref) \/ \E s \in signals[self]: s[1] = "ACK" /\ s[2] = ref;
    if ~openConnection[unalias(ref)] then
        queues[self] := deleteFromTuple(queues[self], LAMBDA head: 
            (isSeq(head) /\ head[1] = "DOWN" /\ head[2] = ref) 
                \/ (isRecord(head) /\ head(.)<<"requestId", ref>>), <<>>);
    else 
        signals[self] := signals[self] \ {<<"ACK", ref>>};
        queues[self] := deleteFromTuple(queues[self], LAMBDA head: 
            (isRecord(head) /\ head(.)<<"requestId", ref>>), <<>>);
    end if;
  end macro;
  
  macro handleSignals() begin
    with signal \in signals[self] do 
        if signal[1] = "MONITOR" then
            mRefs := mRefs ++ {<<signal[2], signal[3]>>};
        elsif signal[1] = "DEMONITOR" then
            mRefs := mRefs \ {<<signal[2], signal[3]>>};
        end if;
        signals := [signals EXCEPT ![signal[3]] = @ ++ {<<"ACK", signal[2]>>}, ![self] = @ \ {signal}];
    end with;
  end macro;
  
  macro handleMonitors() begin
    queues := [pid \in DOMAIN queues |-> 
        IF \E lmRef \in mRefs: lmRef[2] = pid
        THEN queues[pid] (+) <<"DOWN", (CHOOSE ref \in mRefs: ref[2] = pid)[1], self, "terminated">>
        ELSE queues[pid]
    ];
  end macro;
  
  macro awaitTmoSigRcv(ref, msg, forever) begin
    await Len(queues[self]) # 0 \/ (~forever /\ timers[self].isReached); 
    if choosePossible(toSetOfRecs(queues[self]), {ref}) then
        msg := receiveSelective(self, LAMBDA q: hasKey("requestId", q) /\ q.requestId = ref);   (* msg is decoded -> since queues is mapped to encoded*)
        queues[self] := deleteFromTuple(queues[self], LAMBDA head: isRecord(head) /\ msg = head, <<>>);
    elsif receivedDown(self, ref) then
        msg := [result |-> "error", reason |-> "DOWN", requestId |-> ref];
    elsif timers[self].isReached then
        msg := [
            result    |-> "timeout",
            requestId |-> ref
        ];
    else
        msg := [result |-> "", other |-> ref];
    end if;
  end macro;
  
  (*<<"$gen_call", <<self, uuid>>, IF timeout THEN "#CALL TMO" ELSE "#CALL INF">>*)
  (*<<"reply", inMsg, inState (+) inMsg>>*)
  macro sendWithOrWithoutRequestId(from, reply) begin
    if from[2] > 0 then
        sendTo([
            result      |-> reply, 
            requestId   |-> from[2],
            tag         |-> "reply"
        ], from[1]);
    end if;
    replyTrace := replyTrace (+) from[3];
  end macro;
  
  macro receiveSignal(out, timeoutValue) begin
                if isTimeout(timeoutValue) then
                    timers[self] := [timers[self] EXCEPT !.running = TRUE];
                end if;
  S_RECEIVE:    await Len(queues[self]) # 0 \/ Cardinality(signals[self]) # 0 \/ (isTimeout(timeoutValue) /\ timers[self].isReached);
                if Len(queues[self]) # 0 then
                    out := Head(queues[self]);
                    queues[self] := Tail(queues[self]);
                    skipLoop := FALSE;
                elsif Cardinality(signals[self]) # 0 then
                    handleSignals();
                    skipLoop := TRUE;
                elsif timers[self].isReached then
                    out := "timeout";             
                end if;
  end macro;
  
  §redirect(module=, function=loop_0, into=)
  macro loop_0(State) begin
    skip;
  end macro;
  
  §context(from=genServer)
  §pin(name=wake_hib)
  macro wakeHib(parent, name, state, mod, hibernateAfterTimeout, timeout, hibernate, continue, debug) begin
    skipWait := TRUE;
    loop_0([
        var_parent                  |-> parent, 
        var_name                    |-> name, 
        var_serverState             |-> state, 
        var_module                  |-> mod, 
        var_hibernateAfterTimeout   |-> hibernateAfterTimeout, 
        var_timeout                 |-> timeout, 
        var_continue                |-> continue, 
        var_skipWait                |-> skipWait,
        var_serverUp                |-> TRUE,
        var_hibernate               |-> hibernate
    ]);
  end macro;
  
  §redirect(module=proc_lib, function=hibernate, into=)
  macro procLibHibernate(mod, function, args) begin
    await Len(queues[self]) # 0 \/ Cardinality(signals[self]) # 0;
    wakeHib(args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], FALSE);
  end macro;
  
  (*************************************************************************)
  (* Task handling                                                         *)
  (*************************************************************************)
  macro task(receiver, module, function, args) begin
    await Len(calls[module]) = 0;
    calls[module] := calls[module] (+) <<receiver, function, args>>;
    queues[module] := queues[module] (+) <<"$sys">>;
  end macro;
  
  macro taskWaitResult(module, beforeConnection) begin
    await Len(results[self]) > 0 \/ beforeConnection (/) openConnection[module] \/ wasStopped[module];
    if Len(results[self]) > 0 then     
        rVal[self] := Head(results[self]);
        results[self] := Tail(results[self]);
        assert rVal[self].result = "ack";
    elsif beforeConnection (/) openConnection[module] then
        throwError(a"already_started", <<module>>);
    else
        throwError(a"server_down", <<module>>);
        calls[module] := <<>>;                                                 
    end if;                                                                        
  end macro;
 
  macro handleTask(out) begin 
    await Len(calls[self]) > 0;
    out := Head(calls[self]);
    calls[self] := Tail(calls[self]);
    queues[self] := deleteOnceFromTuple(queues[self], <<"$sys">>, <<>>);
  end macro;
  
  macro ackAndUp() begin
    serverUp             := TRUE;
    openConnection[self] := TRUE;
    rVal[self] := [
        result |-> "ack",
        ret    |-> [result |-> "ok", from |-> self]
    ];
  end macro;
  
  macro nackAndFail() begin
    serverUp := FALSE;
    rVal[self] := [
        result |-> "nack",
        ret    |-> [result |-> "error", reason |-> "init error"]
    ];
  end macro;
  
  (*************************************************************************)
  (* CLIENT CALLBACKS                                                      *)
  (*                                                                       *)
  (*                                                                       *)
  (*                                                                       *)
  (*************************************************************************)  
  macro init(inArgs) begin
    initResult := <<"ok", <<>>, 999>>;
    (* Too expensive               
    either
        initResult := <<"ok", <<>>>>;
    or
        initResult := <<"ok", <<>>, 999>>;          (* 999 = infinity *)
    or
        initResult := <<"ok", <<>>, 1000>>;         (* 1000 = hibernate *)
    or
        initResult := <<"ok", <<>>, 1>>;            (* 1 = timeout *)
    end either;*)
  end macro;
  
  §redirect(module=$module, function=handle_call, into=rVal)
  macro handleCall(inMsg, inFrom, inState) begin
    rVal[self] := <<"reply", inMsg, inState (+) <<inMsg>>>>;
  end macro;
  
  §redirect(module=$module, function=handle_cast, into=rVal)
  macro handleCast(inMsg, inState) begin
    rVal[self] := <<"noreply", inState (+) <<inMsg>>>>;
  end macro;
  
  §redirect(module=$module, function=handle_info, into=rVal)
  macro handleInfo(inMsg, inState) begin
    rVal[self] := <<"noreply", inState (+) inMsg>>;
  end macro;
  
  §redirect(module=$module, function=handle_continue, into=raw_msg)
  macro handleContinue(inMsg, inState) begin
    raw_msg := <<"noreply", inState (+) inMsg>>;
  end macro;
  
  §redirect(module=util, function=terminate, into=)
  macro terminate(reason, stacktrace, name, from, msg, mod, state, debug) begin
    serverUp             := FALSE;
    openConnection[self] := FALSE;
    wasStopped[self]     := TRUE;
  end macro;
  
  §redirect(module=util, function=continue, into=)
  macro continue(reason, stacktrace, name, from, msg, mod, state, debug) begin
    skip;
  end macro;
  
  (*************************************************************************)
  (* GEN EXPORTS                                                           *)
  (*************************************************************************)
  §redirect(module=gen, function=reply, into=)
  macro genReply(client, reply) begin 
    sendWithOrWithoutRequestId(client, reply);
  end macro;
  
  §redirect(module=gen, function=start, into=rVal)
  macro genStart6(genModule, linkParameter, name, module, args, options) begin
                if ~openConnection[SERVERID] then
                    if linkParameter = a"nolink" then
                        task(self, SERVERID, a"init_it", [
                            starter    |-> genModule,
                            parent     |-> self, 
                            parentName |-> "self",
                            name       |-> name, 
                            module     |-> module, 
                            args       |-> args, 
                            options    |-> options
                        ]);
                        z := openConnection[SERVERID];
  C_WAIT_ACK:           taskWaitResult(SERVERID, z);  
                    elsif linkParameter = a"monitor" then
                        task(self, SERVERID, a"init_it", [
                            starter    |-> genModule,
                            parent     |-> self, 
                            parentName |-> "self",
                            name       |-> name, 
                            module     |-> module, 
                            args       |-> args, 
                            options    |-> options
                        ]);
                        processesMonitored := processesMonitored ++ {self};
                        z := openConnection[SERVERID];
  C_WAIT_ACK_LINK:      taskWaitResult(SERVERID, z);  
                    elsif linkParameter = a"link" then
                        task(self, SERVERID, a"init_it", [
                            starter    |-> genModule,
                            parent     |-> self, 
                            parentName |-> "self",
                            name       |-> name, 
                            module     |-> module, 
                            args       |-> args, 
                            options    |-> options
                        ]);
                        processesLinked := processesLinked ++ {self};
                        z := openConnection[SERVERID];
  C_WAIT_ACK_MON:       taskWaitResult(SERVERID, z);  
                    else
                        skip;
                    end if;
                else 
                    throwError(a"already_started", <<SERVERID>>);
                end if;
  end macro;
  
  §redirect(module=gen, function=start, into=rVal)
  macro genStart5(genModule, linkParameter, module, args, options) begin
                if linkParameter = a"nolink" then
                    task(self, SERVERID, a"init_it", [
                        starter    |-> genModule,
                        parent     |-> self, 
                        parentName |-> "self",
                        module     |-> module, 
                        args       |-> args, 
                        options    |-> options
                    ]);
                    z := openConnection[SERVERID];
  C_WAIT_ACK:       taskWaitResult(SERVERID, z);  
                elsif linkParameter = a"monitor" then
                    skip;
                elsif linkParameter = a"link" then
                    skip;
                else
                    skip;
                end if;
  end macro;
  
  §redirect(module=gen, function=stop, into=)
  macro genStop(serverRef, reason, timeout) begin
                monitor(serverRef);
  STOP_SEND:    if monitorSuccess /\ ~receivedDown(self, mRefRes) then
                    sendTo(<<"system", <<self, mRefRes>>, <<"terminate", reason>>>>, SERVERID);
                    reqTerminate[self] := TRUE;
                    if isInt(timeout) /\ timeout > 0 then
                        timers[self] := [timers[self] EXCEPT !.running = TRUE];
                    end if;
  STOP_RECEIVE:     awaitTmoSigRcv(mRefRes, rVal[self], isString(timeout));
                    if ~reqIdMatch(rVal[self], mRefRes) /\ ~(rVal[self].result = "timeout" \/ rVal[self].result = "error") then
                        goto "STOP_RECEIVE";
                    else 
                        signal(unalias(mRefRes), {<<"DEMONITOR", mRefRes, self>>});
                    end if;
                else
                    rVal[self] := [result |-> "error", reason |-> "DOWN", requestId |-> mRefRes, status |-> "noproc"];
                end if;
  STOP_DEMON:   demonitor(mRefRes);
                reset();
  end macro;
  
  §redirect(module=gen, function=call, into=rValTemp, catch)
  macro genCall(serverRef, label, request, timeout) begin
                monitor(serverRef);
  CALL_SEND:    if monitorSuccess /\ ~receivedDown(self, mRefRes) then
                    sendTo(<<label, <<self, mRefRes, globalClock>>, request>>, serverRef);
                    globalClock := globalClock + 1;
                    if timeout then
                        timers[self] := [timers[self] EXCEPT !.running = TRUE];
                    end if;
  CALL_RECEIVE:     awaitTmoSigRcv(mRefRes, rVal[self], ~timeout);
                    if ~reqIdMatch(rVal[self], mRefRes) /\ ~(rVal[self].result = "timeout" \/ rVal[self].result = "error") then
                        goto "CALL_RECEIVE";
                    elsif reqIdMatch(rVal[self], mRefRes) then
                        rValTemp[self] := <<"ok", rVal[self]>>;
                        signal(unalias(mRefRes), {<<"DEMONITOR", mRefRes, self>>});
                    else 
                        rValTemp[self] := <<"nook", rVal[self]>>;
                        signal(unalias(mRefRes), {<<"DEMONITOR", mRefRes, self>>});
                    end if;
                else
                    rValTemp[self] := <<"nook", [result |-> "error", reason |-> "DOWN", requestId |-> mRefRes]>>;
                    rVal[self] := [result |-> "error", reason |-> "DOWN", requestId |-> mRefRes];
                end if;
  CALL_DEMON:   demonitor(mRefRes);
                reset();
  end macro;
  
  §pin(name=system_terminate)
  macro systemTerminate(reason, parent, debug, args) begin (* [Name, State, Mod] *) 
    terminate(reason, <<>>, args[1], <<>>, <<>>, args[3], args[2], debug)
  end macro;
  
  §pin(name=system_continue)
  macro systemContinue(parent, debug, args) begin (* [name, state, mod, timeout, Hibernate, HibernateAfterTimeout] *) 
    continue(args[1], parent, args[2], args[3], args[6], args[4], args[5], <<>>);
  end macro;
  
  §pin(name=system_get_state, lazy=true)
  macro systemGetState(args) begin (* [name, state, mod, timeout, Hibernate, HibernateAfterTimeout] *) 
    skip;
  end macro;
  
  (*************************************************************************)
  (* SERVER EXPORTS (API)                                                  *)
  (*************************************************************************)
  §redirect(module=sys, function=handle_system_msg, into=)
  macro handleSystemMsg(req, from, parent, module, debug, misc, hib) begin
    if isSeq(req) /\ req[1] = a"terminate" then
        systemTerminate(req[2], parent, debug, misc);
    else
        skip; 
    end if;
  end macro;
  
  §pin(name=init_it, lazy=true)
  macro initIt(starter, parent, name, module, args, options) begin
    init(args);
    if (initResult[1] = a"ok") then
        serverState := initResult[2];
        if isSeq(options) /\ \E e \in toSetOfRecs(options): hasKey("hibernate_after", e) then
            hibernateAfterTimeout := 1; (* either 0 or 1 *)
        else
            hibernateAfterTimeout := 0;
        end if;
        if Len(initResult) = 3 /\ ~isSeq(initResult[3]) then
            if initResult[3] = 1000 then
                hibernate := a"hibernate";
                ackAndUp();
            elsif initResult[3] = 999 then
                timeout := a"infinity";
                ackAndUp();
            elsif isInt(initResult[3]) then
                timeout := a"1"; (* 1 # infinity error in TLC cmd *)
                ackAndUp();
            else
                nackAndFail();
            end if;
        elsif Len(initResult) = 4 /\ isSeq(initResult[4]) /\ initResult[4][1] = a"continue" then
            continue := initResult[3];
            ackAndUp();
        else 
            ackAndUp();
        end if;
    else
        nackAndFail();
    end if;
    results[parent] := results[parent] (+) rVal[self];
  end macro;
  
  §unused
  §pin(name=start_monitor, convert=list_to_tuple)
  macro startMonitor4(serverName, module, args, options) begin 
    reqStart[self] := TRUE;
    if isSeq(options) /\ isAtom(module) /\ isTuple(serverName) then
        genStart6(rootModule, a"monitor", serverName, module, args, options);
    else
        throwError(a"badarg", <<serverName, module, args, options>>);
    end if;
  end macro;
  
  §unused
  §pin(name=start_monitor, convert=list_to_tuple)
  macro startMonitor3(module, args, options) begin 
    reqStart[self] := TRUE;
    if isSeq(options) /\ isAtom(module) then
        genStart5(rootModule, a"monitor", module, args, options);
    else
        throwError(a"badarg", <<module, args, options>>);
    end if;
  end macro;
  
  §unused
  §pin(name=start_link, convert=list_to_tuple)
  macro startLink4(serverName, module, args, options) begin 
    reqStart[self] := TRUE;
    if isSeq(options) /\ isAtom(module) /\ isTuple(serverName) then
        genStart6(rootModule, a"link", serverName, module, args, options);
    else
        throwError(a"badarg", <<serverName, module, args, options>>);
    end if;
  end macro;
  
  §unused
  §pin(name=start_link, convert=list_to_tuple)
  macro startLink3(module, args, options) begin 
    reqStart[self] := TRUE;
    if isSeq(options) /\ isAtom(module) then
        genStart5(rootModule, a"link", module, args, options);
    else
        throwError(a"badarg", <<module, args, options>>);
    end if;
  end macro;
  
  §pin(name=start, convert=list_to_tuple)
  macro start4(serverName, module, args, options) begin
    reqStart[self] := TRUE;
    if isSeq(options) /\ isAtom(module) /\ isTuple(serverName) then
        genStart6(rootModule, a"nolink", serverName, module, args, options);
    else
        throwError(a"badarg", <<serverName, module, args, options>>);
    end if;
  end macro;
  
  §unused
  §pin(name=start, convert=list_to_tuple)
  macro start3(module, args, options) begin
    reqStart[self] := TRUE;
    if isSeq(options) /\ isAtom(module) then
        genStart5(rootModule, a"nolink", module, args, options);
    else
        throwError(a"badarg", <<module, args, options>>);
    end if;
  end macro;
  
  §pin(name=stop)
  macro stop3(serverRef, reason, timeout) begin
    genStop(serverRef, reason, timeout);
  end macro;
  
  §pin(name=stop)
  macro stop1(serverRef) begin
    stop3(serverRef, a"normal", a"infinity");
  end macro;
  
  §pin(name=call)
  macro call2(serverRef, request) begin
    genCall(serverRef, a"$gen_call", request, default_timeout);
    if rValTemp[self][1] = a"ok" then
        rVal[self] := rValTemp[self][2];
    elsif rValTemp[self][1] = a"EXIT" then
        exit1(<|rValTemp[self][2],<|rootModule, a"call", <<serverRef, request>>|>|>);        
    end if;
  end macro;
  
  §pin(name=call)
  macro call3(serverRef, request, timeout) begin
    genCall(serverRef, a"$gen_call", request, timeout);
    if rValTemp[self][1] = a"ok" then
        rVal[self] := rValTemp[self][2];
    elsif rValTemp[self][1] = a"EXIT" then 
        exit1(<|rValTemp[self][2], <|rootModule, a"call", <<serverRef, request>>|>|>);      
    end if;
  end macro;
  
  §pin(name=reply)
  macro reply2(client, reply) begin
    genReply(client, reply);
  end macro;
  
  §pin(name=cast)
  macro cast(dest, request) begin
    if isTuple(dest) then
        rValTemp[self] := tuple_to_list(dest);
        if rValTemp[self][1] = a"global" then
            globalSend(rValTemp[self][2], list_to_tuple(<<a"$gen_cast", request>>));
        elsif rValTemp[self][1] = a"via" then
            module := rValTemp[self][2];
            modSend(rValTemp[self][3], list_to_tuple(<<a"$gen_cast", request>>));
        else 
            doSend(dest, list_to_tuple(<<a"$gen_cast", request>>));
        end if;
    else
        doSend(dest, list_to_tuple(<<a"$gen_cast", request>>));
    end if;
    rValTemp2[self] := a"ok";
  end macro;
  
  §unused
  §pin(name=abcast, rec=rValTemp,_,_)
  macro abcast3(nodes, name, request) begin
                        abcastNodes := nodes;
  ABCAST_WHILE:         while Len(nodes) > 0 do
                            rValTemp1[self] := Head(abcastNodes);
                            rValTemp[self] := Tail(abcastNodes);
                            doSend(list_to_tuple(<<name, rValTemp1[self]>>), list_to_tuple(<<a"$gen_cast", request>>));
                        end while;
  end macro;
  
  §unused
  §pin(name=abcast)
  macro abcast2(name, request) begin
    abcast3(<<getNode()>> \o getNodes(), name, request);
  end macro;
  
  §unused
  macro abcastQuick3(nodes, name, request) begin
    queues := [id \in DOMAIN queues |-> IF id \in {name} THEN queues[id] (+) <<a"$gen_cast", request>> ELSE queues[id]]
  end macro;
    
  (*************************************************************************)
  (* SERVER FUNCTIONALITY                                                  *)
  (*************************************************************************)
  §collapse(S_WAIT_SPAWN, S_EMIT_MONITOR_DOWN, S_RELEASE_RESOURCES)
  fair process genServer \in ServerSet
  variables
                        (* Message receive variables *)
                        smsg = null;
                        raw_msg = null;
                        
                        (* Server internals *)
                        serverUp = FALSE;
                        serverState = <<>>;
                        
                        timeout = null;
                        hibernate = "";
                        continue = <<>>; (* infinity, integer, <<continue, C>> *)
                        hibernateAfterTimeout = null;
                        skipLoop = FALSE;
                        skipLoopHibernate = FALSE;
                        skipWait = FALSE;
                        
                        parent = "";
                        name = "";
                        initResult = null;
                        module = a"gen_server_callbacks";
                        
                        §internal begin
                            mRef    = null;
                            mRefRes = null;
                            mRefs   = {};
                            z       = null;
                        end;
  begin
  S_WAIT_SPAWN:         handleTask(smsg); (*rcv func args*)
                        if (smsg[2] = "init_it") then
                            initIt(smsg[3].starter, smsg[3].parent, smsg[3].name, smsg[3].module, smsg[3].args, smsg[3].options);
                            parent := smsg[3].parent;
                            name := smsg[3].name;
                        else
                            goto "S_WAIT_SPAWN";
                        end if;
  S_ENTER_LOOP:         while serverUp do
                            preCleanup();
                            if ~skipWait /\ isTimeout(timeout) /\ hibernate # a"hibernate" then
                                receiveSignal(raw_msg, timeout);
                                if raw_msg = a"timeout" \/ Head(tuple_to_list(raw_msg)) # a"system" then
                                    timeout := a"infinity";
                                end if;
                            elsif ~skipWait /\ isInt(hibernateAfterTimeout) /\ hibernate # a"hibernate" then
                                receiveSignal(raw_msg, hibernateAfterTimeout);
                                if isAtom(raw_msg) /\ raw_msg = a"timeout" then
                                    hibernate := a"hibernate";
                                    skipLoopHibernate := TRUE;
                                end if;
                            elsif ~skipWait /\ hibernate = a"hibernate" then
                                procLibHibernate(rootModule, a"wake_hib", <<parent, name, serverState, module, hibernateAfterTimeout, timeout, hibernate, continue, FALSE>>);
                                skipLoop := TRUE;
                            elsif ~skipWait /\ isTuple(continue) /\ Len(tuple_to_list(continue)) = 2 /\ Head(tuple_to_list(continue)) = a"continue"  then  
                                handleContinue(tuple_to_list(continue)[2], serverState);                                      (* Handle continue *)
                                continue := <<a"continued">>;
                                skipLoop := FALSE;
                            else 
                                receiveSignal(raw_msg, a"infinity");     
                                skipWait := FALSE;    
                                skipLoopHibernate := FALSE;  
                            end if;
  S_PROCESS:                if ~skipLoop /\ ~skipLoopHibernate then
                                if isTuple(raw_msg) then
                                    smsg := tuple_to_list(raw_msg);
                                else
                                    smsg := raw_msg;
                                end if;
                                if isSeq(smsg) /\ Len(smsg) = 3 /\ smsg[1] = a"system" then
                                    if hibernate = a"hibernate" then
                                        handleSystemMsg(smsg[3], smsg[2], parent, rootModule, <<>>, <<name, serverState, module, timeout, hibernate, hibernateAfterTimeout>>, TRUE); (*[Name, State, CbCache, Time, HibernateAfterTimeout]*)
                                    else
                                        handleSystemMsg(smsg[3], smsg[2], parent, rootModule, <<>>, <<name, serverState, module, timeout, hibernate, hibernateAfterTimeout>>, FALSE);
                                    end if;
                                    hibernate := "";
                                elsif isSeq(smsg) /\ Len(smsg) = 3 /\ smsg[1] = a"EXIT" then
                                    hibernate := "";
                                    terminate(smsg[3], <<>>, name, self, smsg, module, serverState, <<>>);
                                elsif isSeq(smsg) /\ Len(smsg) = 3 /\ smsg[1] = a"$gen_call" then                       (* Handle calls *)
                                    handleCall(smsg[3], smsg[2], serverState);
                                    if isSeq(rVal[self]) /\ Len(rVal[self]) = 3 /\ rVal[self][1] = a"reply" then        (* {reply, Reply, NState} *)
                                        reply2(smsg[2], rVal[self][2]);
                                        serverState := rVal[self][3];
                                        hibernate := "";
                                    elsif isSeq(rVal[self]) /\ Len(rVal[self]) = 4 /\ rVal[self][1] = a"reply" then     (* {reply, Reply, NState, Timeout} *)
                                        reply2(smsg[2], rVal[self][2]);
                                        serverState := rVal[self][3];
                                        if isTimeout(rVal[self][4]) then
                                            timeout := rVal[self][4];
                                            hibernate := "";
                                        elsif isAtom(rVal[self][4]) /\ rVal[self][4] = a"hibernate" then                
                                            hibernate := a"hibernate";
                                        elsif isTuple(rVal[self][4]) /\ Len(tuple_to_list(rVal[self][4])) = 2 /\ Head(tuple_to_list(rVal[self][4])) = a"continue" then (* {reply, Reply, NState, {Continue, cont}} *)
                                            continue := rVal[self][4];
                                        end if;
                                    elsif isSeq(rVal[self]) /\ Len(rVal[self]) = 2 /\ rVal[self][1] = a"noreply" then   (* {noreply, NState} *)
                                        serverState := rVal[self][2];
                                        hibernate := "";
                                    elsif isSeq(rVal[self]) /\ Len(rVal[self]) = 3 /\ rVal[self][1] = a"noreply" then   (* {noreply, NState, Timeout} *)
                                        serverState := rVal[self][2];
                                        if isTimeout(rVal[self][3]) then
                                            timeout := rVal[self][3];
                                            hibernate := "";
                                        elsif isAtom(rVal[self][3]) /\ rVal[self][3] = a"hibernate" then
                                            hibernate := a"hibernate";
                                        elsif isTuple(rVal[self][3]) /\ Len(tuple_to_list(rVal[self][3])) = 2 /\ Head(tuple_to_list(rVal[self][3])) = a"continue" then (* {noreply, NState, {Continue, cont}} *)
                                            continue := rVal[self][3];
                                        end if;
                                    elsif isSeq(rVal[self]) /\ Len(rVal[self]) = 4 /\ (rVal[self])[1] = a"stop" then    (* {stop, Reason, Reply, NState} *)
                                        reply2(smsg[2], rVal[self][3]);
                                        hibernate := "";
                                        terminate(rVal[self][2], <<>>, name, self, smsg, module, serverState, <<>>);
                                    elsif isSeq(rVal[self]) /\ Len(rVal[self]) = 3 /\ (rVal[self])[1] = a"stop" then    (* {stop, Reason, NState} *)
                                        terminate(rVal[self][2], <<>>, name, self, smsg, module, serverState, <<>>);
                                        hibernate := "";
                                    elsif ~isSeq(rVal[self]) then
                                        terminate(<|a"bad_return_value", rVal[self]|>, <<>>, name, self, smsg, module, serverState, <<>>);
                                        hibernate := "";
                                    end if;
                                elsif isSeq(smsg) /\ Len(smsg) = 2 /\ smsg[1] = a"$gen_cast" then                       (* Handle casts *)
                                    handleCast(smsg[2], serverState);
                                    if isSeq(rVal[self]) /\ Len(rVal[self]) = 2 /\ rVal[self][1] = a"noreply" then      (* {noreply, State} *)
                                        serverState := rVal[self][2];
                                        hibernate := "";
                                    elsif isSeq(rVal[self]) /\ Len(rVal[self]) = 3 /\ rVal[self][1] = a"noreply" then   (* {noreply, State, Timeout} *)
                                        serverState := rVal[self][2];
                                        if isTimeout(rVal[self][3]) then
                                            timeout := rVal[self][3];
                                            hibernate := "";
                                        elsif isAtom(rVal[self][3]) /\ rVal[self][3] = a"hibernate" then
                                            hibernate := a"hibernate";
                                        elsif isTuple(rVal[self][3]) /\ Len(tuple_to_list(rVal[self][3])) = 2 /\ Head(tuple_to_list(rVal[self][3])) = a"continue" then (* {noreply, NState, {Continue, cont}} *)
                                            continue := rVal[self][3];
                                        end if;
                                    elsif isSeq(rVal[self]) /\ Len(rVal[self]) = 3 /\ rVal[self][1] = a"stop" then      (* {stop, Reason, State} *)
                                        serverState := rVal[self][3];
                                        terminate(rVal[self][2], <<>>, name, self, smsg, module, serverState, <<>>);
                                        hibernate := "";
                                    end if;
                                else    
                                    if isExported(module, a"handle_info", 2) /\ ~(isSeq(continue) /\ Len(continue) = 1) then (* Handle info *)
                                        handleInfo(raw_msg, serverState);
                                        if isSeq(rVal[self]) /\ Len(rVal[self]) = 2 /\ rVal[self][1] = a"noreply" then      (* {noreply, State} *)
                                            serverState := rVal[self][2];
                                            hibernate := "";
                                        elsif isSeq(rVal[self]) /\ Len(rVal[self]) = 3 /\ rVal[self][1] = a"noreply" then   (* {noreply, State, Timeout} *)
                                            serverState := rVal[self][2];
                                            if isTimeout(rVal[self][3]) then
                                                timeout := rVal[self][3];
                                                hibernate := "";
                                            elsif isAtom(rVal[self][3]) /\ rVal[self][3] = a"hibernate" then
                                                hibernate := a"hibernate";
                                            elsif isTuple(rVal[self][3]) /\ Len(tuple_to_list(rVal[self][3])) = 2 /\ Head(tuple_to_list(rVal[self][3])) = a"continue" then (* {noreply, NState, {Continue, cont}} *)
                                                continue := rVal[self][3];
                                            end if;
                                        elsif isSeq(rVal[self]) /\ Len(rVal[self]) = 3 /\ rVal[self][1] = a"stop" then      (* {stop, Reason, State} *)
                                            serverState := rVal[self][3];
                                            hibernate := "";
                                            terminate(rVal[self][2], <<>>, name, self, smsg, module, rVal[self][3], <<>>);
                                        end if;
                                    else
                                        if isSeq(smsg) /\ Len(smsg) = 2 /\ smsg[1] = a"noreply" then      (* {noreply, State} *)
                                            serverState := smsg[2];
                                            hibernate := "";
                                            continue := <<>>;
                                        elsif isSeq(smsg) /\ Len(smsg) = 3 /\ smsg[1] = a"noreply" then   (* {noreply, State, Timeout} *)
                                            serverState := smsg[2];
                                            if isTimeout(smsg[3]) then
                                                timeout := smsg[3];
                                                hibernate := "";
                                                continue := <<>>;
                                            elsif isAtom(smsg[3]) /\ smsg[3] = a"hibernate" then
                                                hibernate := a"hibernate";
                                                continue := <<>>;
                                            elsif isTuple(smsg[3]) /\ Len(tuple_to_list(smsg[3])) = 2 /\ Head(tuple_to_list(smsg[3])) = a"continue" then (* {noreply, NState, {Continue, cont}} *)
                                                continue := smsg[3];
                                            end if;
                                        elsif isSeq(smsg) /\ Len(smsg) = 3 /\ smsg[1] = a"stop" then      (* {stop, Reason, State} *)
                                            serverState := smsg[3];
                                            hibernate := "";
                                            terminate(smsg[2], <<>>, name, self, smsg, module, smsg[3], <<>>);
                                            continue := <<>>;
                                        end if;
                                    end if;
                                end if;
                            end if;
                            postCleanup();
                        end while;
  S_RELEASE_RESOURCES:  z := openConnection[self];
                        handleMonitors();
                        if z then                       
                            terminate("normal", <<>>, name, self, <<>>, module, serverState, <<>>);
                        end if;
  end process;

  (*************************************************************************)
  (* CLIENT FUNCTIONALITY                                                  *)
  (*************************************************************************)
  §hide
  fair process workerClient \in WorkerClients
  variables
                        iterations = TrafficPerClient;
                        z          = null;
                        module     = "";                    (* Any module for $module callback in redirect *)
                        monitorSuccess = FALSE;
                        mRef         = null;
                        mRefRes      = null;
                        
                        (* Call variables *)
                        reqId           = null;
                        previousReqId   = null;
                        timeout         = FALSE;
                        callMsg         = "";
  begin
  C_WAIT:               await canStart;
  C_LOOP:               while iterations > 0 do
                            either
                                (*******************************************)
                                (* gen_server:call                         *)
                                (*******************************************)
                                timeout := FALSE;
                                rValTemp1[self] := <<"$gen_call", "#CALL INF", timeout, uuid>>;
                                call3(SERVERID, "#CALL INF", timeout);
                            or 
                                (*******************************************)
                                (* gen_server:call                         *)
                                (*******************************************)
                                timeout := TRUE;
                                rValTemp1[self] := <<"$gen_call", "#CALL TMO", timeout, uuid>>;
                                call3(SERVERID, "#CALL TMO", timeout);
                            or
                                (*******************************************)
                                (* gen_server:cast                         *)
                                (*******************************************)
                                cast(SERVERID, "#CAST");
                                rValTemp1[self] := <<"$gen_cast", "#CAST">>;
                            or
                                (*******************************************)
                                (* erlang:send                             *)
                                (*******************************************)
                                sendTo(<<"100">>, SERVERID);
                                rValTemp1[self] := <<"100">>;
                            end either;
  C_LOG:                    trace[self] := trace[self] (+) rValTemp1[self];
                            if rVal[self] # null then 
                                callTrace[self] := callTrace[self] (+) rVal[self]
                            end if;
                            timers[self] := [timers[self] EXCEPT !.running = FALSE, !.isReached = FALSE];
                            rVal[self] := null;
                            rValTemp[self] := null;
                            rValTemp1[self] := null;
                            z := null;
                            iterations := iterations - 1;
                        end while;
                        clientUp[self] := FALSE;
                        processesMonitored := processesMonitored \ {self};
  end process;
  
  §hide
  fair process controlClient \in ControlClients
  variables
                        z          = null;
                        module     = "";                    (* Any module for $module callback in redirect *)
                        monitorSuccess = FALSE;
                        mRef         = null;
                        mRefRes      = null;
  begin
  W_START:              start4(<<a"local", "server">>, a"gen_server_callbacks", <<>>, <<[hibernate_after |-> 0]>>);
  W_SYNC:               if syncStart then
                            canStart := TRUE;
                        end if;
  W_STOP:               assert ~rVal[self](.)<<"result", "error">> \/ ~rVal[self](.)<<"reason", "bad_arg">>;
                        await \A c \in WorkerClients: ~clientUp[c];
                        rVal[self] := null;
                        stop3(SERVERID, a"normal", a"infinity");
                        clientUp[self] := FALSE;
  end process;
  
  §hide
  fair process timer \in TimerSet
  begin
  TIMER_TICK:   while (\E c \in AllClients: clientUp[c]) do
                    await (\A c \in AllClients: ~(clientUp[c])) \/ anyRunning(timers);     
                    timers := tickTimers(timers);
                end while;
  end process;
end algorithm;
*)

(***************************************************************************)
(*                                                                         *)
(*                                                                         *)
(*                                                                         *)
(* SPEC                                                                    *)
(*                                                                         *)
(*                                                                         *)
(*                                                                         *)
(***************************************************************************)

\* BEGIN TRANSLATION
\* Process variable timeout of process genServer at line 704 col 25 changed to timeout_
\* Process variable module of process genServer at line 715 col 25 changed to module_
\* Process variable mRef of process genServer at line 718 col 29 changed to mRef_
\* Process variable mRefRes of process genServer at line 719 col 29 changed to mRefRes_
\* Process variable z of process genServer at line 721 col 29 changed to z_
\* Process variable z of process workerClient at line 897 col 25 changed to z_w
\* Process variable module of process workerClient at line 898 col 25 changed to module_w
\* Process variable monitorSuccess of process workerClient at line 899 col 25 changed to monitorSuccess_
\* Process variable mRef of process workerClient at line 900 col 25 changed to mRef_w
\* Process variable mRefRes of process workerClient at line 901 col 25 changed to mRefRes_w
CONSTANT defaultInitValue
VARIABLES null, rootModule, uuid, queues, signals, mRefTable, processesLinked, 
          processesMonitored, calls, results, timers, clientUp, 
          openConnection, wasStopped, rVal, rValTemp, rValTemp1, rValTemp2, 
          rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, 
          reqTerminate, canStart, pc

(* define statement *)
ServerSet     == Server
NodeSet       == ServerSet ++ (WorkerClients ++ ControlClients)
AllClients    == WorkerClients ++ ControlClients

unalias(mRef) == mRefTable[mRef]
receiveSelective(at, matcher(_)) == (CHOOSE q \in toSetOfRecs(queues[at]): matcher(q))
receivedDown(at, r) == \E e \in toSetOfSeqs(queues[at]): isSeq(e) /\ e[1] = "DOWN" /\ e[2] = r
list_to_tuple(in) == in
tuple_to_list(in) == in

default_timeout == 5000

VARIABLES smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, 
          hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, 
          parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, 
          iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, 
          reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, 
          mRef, mRefRes

vars == << null, rootModule, uuid, queues, signals, mRefTable, 
           processesLinked, processesMonitored, calls, results, timers, 
           clientUp, openConnection, wasStopped, rVal, rValTemp, rValTemp1, 
           rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, 
           reqStart, reqTerminate, canStart, pc, smsg, raw_msg, serverUp, 
           serverState, timeout_, hibernate, continue, hibernateAfterTimeout, 
           skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, 
           module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, 
           monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, 
           callMsg, z, module, monitorSuccess, mRef, mRefRes >>

ProcSet == (ServerSet) \cup (WorkerClients) \cup (ControlClients) \cup (TimerSet)

Init == (* Global variables *)
        /\ null = defaultInitValue
        /\ rootModule =  "?MODULE"
        /\ uuid = 100
        /\ queues = [id \in (NodeSet ++ TimerSet) |-> <<>>]
        /\ signals = [id \in NodeSet |-> {}]
        /\ mRefTable = [id \in (uuid)..(uuid + Cardinality(AllClients) + Cardinality(AllClients) * TrafficPerClient) |-> null]
        /\ processesLinked = {}
        /\ processesMonitored = {}
        /\ calls = [id \in NodeSet |-> <<>>]
        /\ results = [id \in NodeSet |-> <<>>]
        /\ timers = [id \in NodeSet |-> [isReached |-> FALSE, running |-> FALSE]]
        /\ clientUp = [id \in AllClients |-> TRUE]
        /\ openConnection = [id \in NodeSet |-> FALSE]
        /\ wasStopped = [id \in ServerSet |-> FALSE]
        /\ rVal = [id \in NodeSet |-> [result |-> ""]]
        /\ rValTemp = [id \in NodeSet |-> null]
        /\ rValTemp1 = [id \in NodeSet |-> null]
        /\ rValTemp2 = [id \in NodeSet |-> null]
        /\ rValTemp3 = [id \in NodeSet |-> null]
        /\ trace = [id \in WorkerClients |-> <<>>]
        /\ callTrace = [id \in WorkerClients |-> <<>>]
        /\ replyTrace = <<>>
        /\ globalClock = 0
        /\ reqStart = [id \in AllClients |-> FALSE]
        /\ reqTerminate = [id \in AllClients |-> FALSE]
        /\ canStart = ~syncStart
        (* Process genServer *)
        /\ smsg = [self \in ServerSet |-> null]
        /\ raw_msg = [self \in ServerSet |-> null]
        /\ serverUp = [self \in ServerSet |-> FALSE]
        /\ serverState = [self \in ServerSet |-> <<>>]
        /\ timeout_ = [self \in ServerSet |-> null]
        /\ hibernate = [self \in ServerSet |-> ""]
        /\ continue = [self \in ServerSet |-> <<>>]
        /\ hibernateAfterTimeout = [self \in ServerSet |-> null]
        /\ skipLoop = [self \in ServerSet |-> FALSE]
        /\ skipLoopHibernate = [self \in ServerSet |-> FALSE]
        /\ skipWait = [self \in ServerSet |-> FALSE]
        /\ parent = [self \in ServerSet |-> ""]
        /\ name = [self \in ServerSet |-> ""]
        /\ initResult = [self \in ServerSet |-> null]
        /\ module_ = [self \in ServerSet |-> "gen_server_callbacks"]
        /\ mRef_ = [self \in ServerSet |-> null]
        /\ mRefRes_ = [self \in ServerSet |-> null]
        /\ mRefs = [self \in ServerSet |-> {}]
        /\ z_ = [self \in ServerSet |-> null]
        (* Process workerClient *)
        /\ iterations = [self \in WorkerClients |-> TrafficPerClient]
        /\ z_w = [self \in WorkerClients |-> null]
        /\ module_w = [self \in WorkerClients |-> ""]
        /\ monitorSuccess_ = [self \in WorkerClients |-> FALSE]
        /\ mRef_w = [self \in WorkerClients |-> null]
        /\ mRefRes_w = [self \in WorkerClients |-> null]
        /\ reqId = [self \in WorkerClients |-> null]
        /\ previousReqId = [self \in WorkerClients |-> null]
        /\ timeout = [self \in WorkerClients |-> FALSE]
        /\ callMsg = [self \in WorkerClients |-> ""]
        (* Process controlClient *)
        /\ z = [self \in ControlClients |-> null]
        /\ module = [self \in ControlClients |-> ""]
        /\ monitorSuccess = [self \in ControlClients |-> FALSE]
        /\ mRef = [self \in ControlClients |-> null]
        /\ mRefRes = [self \in ControlClients |-> null]
        /\ pc = [self \in ProcSet |-> CASE self \in ServerSet -> "S_WAIT_SPAWN"
                                        [] self \in WorkerClients -> "C_WAIT"
                                        [] self \in ControlClients -> "W_START"
                                        [] self \in TimerSet -> "TIMER_TICK"]

S_WAIT_SPAWN(self) == /\ pc[self] = "S_WAIT_SPAWN"
                      /\ Len(calls[self]) > 0
                      /\ smsg' = [smsg EXCEPT ![self] = Head(calls[self])]
                      /\ calls' = [calls EXCEPT ![self] = Tail(calls[self])]
                      /\ queues' = [queues EXCEPT ![self] = deleteOnceFromTuple(queues[self], <<"$sys">>, <<>>)]
                      /\ IF (smsg'[self][2] = "init_it")
                            THEN /\ initResult' = [initResult EXCEPT ![self] = <<"ok", <<>>, 999>>]
                                 /\ IF (initResult'[self][1] =  "ok")
                                       THEN /\ serverState' = [serverState EXCEPT ![self] = initResult'[self][2]]
                                            /\ IF isSeq((smsg'[self][3].options)) /\ \E e \in toSetOfRecs((smsg'[self][3].options)): hasKey("hibernate_after", e)
                                                  THEN /\ hibernateAfterTimeout' = [hibernateAfterTimeout EXCEPT ![self] = 1]
                                                  ELSE /\ hibernateAfterTimeout' = [hibernateAfterTimeout EXCEPT ![self] = 0]
                                            /\ IF Len(initResult'[self]) = 3 /\ ~isSeq(initResult'[self][3])
                                                  THEN /\ IF initResult'[self][3] = 1000
                                                             THEN /\ hibernate' = [hibernate EXCEPT ![self] = "hibernate"]
                                                                  /\ serverUp' = [serverUp EXCEPT ![self] = TRUE]
                                                                  /\ openConnection' = [openConnection EXCEPT ![self] = TRUE]
                                                                  /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                                        result |-> "ack",
                                                                                                        ret    |-> [result |-> "ok", from |-> self]
                                                                                                    ]]
                                                                  /\ UNCHANGED timeout_
                                                             ELSE /\ IF initResult'[self][3] = 999
                                                                        THEN /\ timeout_' = [timeout_ EXCEPT ![self] = "infinity"]
                                                                             /\ serverUp' = [serverUp EXCEPT ![self] = TRUE]
                                                                             /\ openConnection' = [openConnection EXCEPT ![self] = TRUE]
                                                                             /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                                                   result |-> "ack",
                                                                                                                   ret    |-> [result |-> "ok", from |-> self]
                                                                                                               ]]
                                                                        ELSE /\ IF isInt(initResult'[self][3])
                                                                                   THEN /\ timeout_' = [timeout_ EXCEPT ![self] = "1"]
                                                                                        /\ serverUp' = [serverUp EXCEPT ![self] = TRUE]
                                                                                        /\ openConnection' = [openConnection EXCEPT ![self] = TRUE]
                                                                                        /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                                                              result |-> "ack",
                                                                                                                              ret    |-> [result |-> "ok", from |-> self]
                                                                                                                          ]]
                                                                                   ELSE /\ serverUp' = [serverUp EXCEPT ![self] = FALSE]
                                                                                        /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                                                              result |-> "nack",
                                                                                                                              ret    |-> [result |-> "error", reason |-> "init error"]
                                                                                                                          ]]
                                                                                        /\ UNCHANGED << openConnection, 
                                                                                                        timeout_ >>
                                                                  /\ UNCHANGED hibernate
                                                       /\ UNCHANGED continue
                                                  ELSE /\ IF Len(initResult'[self]) = 4 /\ isSeq(initResult'[self][4]) /\ initResult'[self][4][1] =  "continue"
                                                             THEN /\ continue' = [continue EXCEPT ![self] = initResult'[self][3]]
                                                                  /\ serverUp' = [serverUp EXCEPT ![self] = TRUE]
                                                                  /\ openConnection' = [openConnection EXCEPT ![self] = TRUE]
                                                                  /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                                        result |-> "ack",
                                                                                                        ret    |-> [result |-> "ok", from |-> self]
                                                                                                    ]]
                                                             ELSE /\ serverUp' = [serverUp EXCEPT ![self] = TRUE]
                                                                  /\ openConnection' = [openConnection EXCEPT ![self] = TRUE]
                                                                  /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                                        result |-> "ack",
                                                                                                        ret    |-> [result |-> "ok", from |-> self]
                                                                                                    ]]
                                                                  /\ UNCHANGED continue
                                                       /\ UNCHANGED << timeout_, 
                                                                       hibernate >>
                                       ELSE /\ serverUp' = [serverUp EXCEPT ![self] = FALSE]
                                            /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                  result |-> "nack",
                                                                                  ret    |-> [result |-> "error", reason |-> "init error"]
                                                                              ]]
                                            /\ UNCHANGED << openConnection, 
                                                            serverState, 
                                                            timeout_, 
                                                            hibernate, 
                                                            continue, 
                                                            hibernateAfterTimeout >>
                                 /\ results' = [results EXCEPT ![(smsg'[self][3].parent)] = results[(smsg'[self][3].parent)] (+) rVal'[self]]
                                 /\ parent' = [parent EXCEPT ![self] = smsg'[self][3].parent]
                                 /\ name' = [name EXCEPT ![self] = smsg'[self][3].name]
                                 /\ pc' = [pc EXCEPT ![self] = "S_ENTER_LOOP"]
                            ELSE /\ pc' = [pc EXCEPT ![self] = "S_WAIT_SPAWN"]
                                 /\ UNCHANGED << results, openConnection, rVal, 
                                                 serverUp, serverState, 
                                                 timeout_, hibernate, continue, 
                                                 hibernateAfterTimeout, parent, 
                                                 name, initResult >>
                      /\ UNCHANGED << null, rootModule, uuid, signals, mRefTable, processesLinked, processesMonitored, timers, clientUp, wasStopped, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, raw_msg, skipLoop, skipLoopHibernate, skipWait, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

S_ENTER_LOOP(self) == /\ pc[self] = "S_ENTER_LOOP"
                      /\ IF serverUp[self]
                            THEN /\ rVal' = [rVal EXCEPT ![self] = null]
                                 /\ smsg' = [smsg EXCEPT ![self] = null]
                                 /\ IF ~skipWait[self] /\ isTimeout(timeout_[self]) /\ hibernate[self] #  "hibernate"
                                       THEN /\ IF isTimeout(timeout_[self])
                                                  THEN /\ timers' = [timers EXCEPT ![self] = [timers[self] EXCEPT !.running = TRUE]]
                                                  ELSE /\ TRUE
                                                       /\ UNCHANGED timers
                                            /\ pc' = [pc EXCEPT ![self] = "S_RECEIVE"]
                                            /\ UNCHANGED << raw_msg, continue, 
                                                            skipLoop, skipWait >>
                                       ELSE /\ IF ~skipWait[self] /\ isInt(hibernateAfterTimeout[self]) /\ hibernate[self] #  "hibernate"
                                                  THEN /\ IF isTimeout(hibernateAfterTimeout[self])
                                                             THEN /\ timers' = [timers EXCEPT ![self] = [timers[self] EXCEPT !.running = TRUE]]
                                                             ELSE /\ TRUE
                                                                  /\ UNCHANGED timers
                                                       /\ pc' = [pc EXCEPT ![self] = "S_RECEIVE_receiveSignal"]
                                                       /\ UNCHANGED << raw_msg, 
                                                                       continue, 
                                                                       skipLoop, 
                                                                       skipWait >>
                                                  ELSE /\ IF ~skipWait[self] /\ hibernate[self] =  "hibernate"
                                                             THEN /\ Len(queues[self]) # 0 \/ Cardinality(signals[self]) # 0
                                                                  /\ skipWait' = [skipWait EXCEPT ![self] = TRUE]
                                                                  /\ TRUE
                                                                  /\ skipLoop' = [skipLoop EXCEPT ![self] = TRUE]
                                                                  /\ pc' = [pc EXCEPT ![self] = "S_PROCESS"]
                                                                  /\ UNCHANGED << timers, 
                                                                                  raw_msg, 
                                                                                  continue >>
                                                             ELSE /\ IF ~skipWait[self] /\ isTuple(continue[self]) /\ Len(tuple_to_list(continue[self])) = 2 /\ Head(tuple_to_list(continue[self])) =  "continue"
                                                                        THEN /\ raw_msg' = [raw_msg EXCEPT ![self] = <<"noreply", serverState[self] (+) (tuple_to_list(continue[self])[2])>>]
                                                                             /\ continue' = [continue EXCEPT ![self] = << "continued">>]
                                                                             /\ skipLoop' = [skipLoop EXCEPT ![self] = FALSE]
                                                                             /\ pc' = [pc EXCEPT ![self] = "S_PROCESS"]
                                                                             /\ UNCHANGED timers
                                                                        ELSE /\ IF isTimeout("infinity")
                                                                                   THEN /\ timers' = [timers EXCEPT ![self] = [timers[self] EXCEPT !.running = TRUE]]
                                                                                   ELSE /\ TRUE
                                                                                        /\ UNCHANGED timers
                                                                             /\ pc' = [pc EXCEPT ![self] = "S_RECEIVE_receiveSignal_receiveSignal"]
                                                                             /\ UNCHANGED << raw_msg, 
                                                                                             continue, 
                                                                                             skipLoop >>
                                                                  /\ UNCHANGED skipWait
                            ELSE /\ pc' = [pc EXCEPT ![self] = "S_RELEASE_RESOURCES"]
                                 /\ UNCHANGED << timers, rVal, smsg, raw_msg, 
                                                 continue, skipLoop, skipWait >>
                      /\ UNCHANGED << null, rootModule, uuid, queues, signals, mRefTable, processesLinked, processesMonitored, calls, results, clientUp, openConnection, wasStopped, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, serverUp, serverState, timeout_, hibernate, hibernateAfterTimeout, skipLoopHibernate, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

S_PROCESS(self) == /\ pc[self] = "S_PROCESS"
                   /\ IF ~skipLoop[self] /\ ~skipLoopHibernate[self]
                         THEN /\ IF isTuple(raw_msg[self])
                                    THEN /\ smsg' = [smsg EXCEPT ![self] = tuple_to_list(raw_msg[self])]
                                    ELSE /\ smsg' = [smsg EXCEPT ![self] = raw_msg[self]]
                              /\ IF isSeq(smsg'[self]) /\ Len(smsg'[self]) = 3 /\ smsg'[self][1] =  "system"
                                    THEN /\ IF hibernate[self] =  "hibernate"
                                               THEN /\ IF isSeq((smsg'[self][3])) /\ (smsg'[self][3])[1] =  "terminate"
                                                          THEN /\ serverUp' = [serverUp EXCEPT ![self] = FALSE]
                                                               /\ openConnection' = [openConnection EXCEPT ![self] = FALSE]
                                                               /\ wasStopped' = [wasStopped EXCEPT ![self] = TRUE]
                                                          ELSE /\ TRUE
                                                               /\ UNCHANGED << openConnection, 
                                                                               wasStopped, 
                                                                               serverUp >>
                                               ELSE /\ IF isSeq((smsg'[self][3])) /\ (smsg'[self][3])[1] =  "terminate"
                                                          THEN /\ serverUp' = [serverUp EXCEPT ![self] = FALSE]
                                                               /\ openConnection' = [openConnection EXCEPT ![self] = FALSE]
                                                               /\ wasStopped' = [wasStopped EXCEPT ![self] = TRUE]
                                                          ELSE /\ TRUE
                                                               /\ UNCHANGED << openConnection, 
                                                                               wasStopped, 
                                                                               serverUp >>
                                         /\ hibernate' = [hibernate EXCEPT ![self] = ""]
                                         /\ UNCHANGED << queues, rVal, 
                                                         replyTrace, 
                                                         serverState, timeout_, 
                                                         continue >>
                                    ELSE /\ IF isSeq(smsg'[self]) /\ Len(smsg'[self]) = 3 /\ smsg'[self][1] =  "EXIT"
                                               THEN /\ hibernate' = [hibernate EXCEPT ![self] = ""]
                                                    /\ serverUp' = [serverUp EXCEPT ![self] = FALSE]
                                                    /\ openConnection' = [openConnection EXCEPT ![self] = FALSE]
                                                    /\ wasStopped' = [wasStopped EXCEPT ![self] = TRUE]
                                                    /\ UNCHANGED << queues, 
                                                                    rVal, 
                                                                    replyTrace, 
                                                                    serverState, 
                                                                    timeout_, 
                                                                    continue >>
                                               ELSE /\ IF isSeq(smsg'[self]) /\ Len(smsg'[self]) = 3 /\ smsg'[self][1] =  "$gen_call"
                                                          THEN /\ rVal' = [rVal EXCEPT ![self] = <<"reply", (smsg'[self][3]), serverState[self] (+) <<(smsg'[self][3])>>>>]
                                                               /\ IF isSeq(rVal'[self]) /\ Len(rVal'[self]) = 3 /\ rVal'[self][1] =  "reply"
                                                                     THEN /\ IF (smsg'[self][2])[2] > 0
                                                                                THEN /\ queues' = [queues EXCEPT ![((smsg'[self][2])[1])] = @ (+) (       [
                                                                                                      result      |-> (rVal'[self][2]),
                                                                                                      requestId   |-> (smsg'[self][2])[2],
                                                                                                      tag         |-> "reply"
                                                                                                  ])]
                                                                                ELSE /\ TRUE
                                                                                     /\ UNCHANGED queues
                                                                          /\ replyTrace' = replyTrace (+) (smsg'[self][2])[3]
                                                                          /\ serverState' = [serverState EXCEPT ![self] = rVal'[self][3]]
                                                                          /\ hibernate' = [hibernate EXCEPT ![self] = ""]
                                                                          /\ UNCHANGED << openConnection, 
                                                                                          wasStopped, 
                                                                                          serverUp, 
                                                                                          timeout_, 
                                                                                          continue >>
                                                                     ELSE /\ IF isSeq(rVal'[self]) /\ Len(rVal'[self]) = 4 /\ rVal'[self][1] =  "reply"
                                                                                THEN /\ IF (smsg'[self][2])[2] > 0
                                                                                           THEN /\ queues' = [queues EXCEPT ![((smsg'[self][2])[1])] = @ (+) (       [
                                                                                                                 result      |-> (rVal'[self][2]),
                                                                                                                 requestId   |-> (smsg'[self][2])[2],
                                                                                                                 tag         |-> "reply"
                                                                                                             ])]
                                                                                           ELSE /\ TRUE
                                                                                                /\ UNCHANGED queues
                                                                                     /\ replyTrace' = replyTrace (+) (smsg'[self][2])[3]
                                                                                     /\ serverState' = [serverState EXCEPT ![self] = rVal'[self][3]]
                                                                                     /\ IF isTimeout(rVal'[self][4])
                                                                                           THEN /\ timeout_' = [timeout_ EXCEPT ![self] = rVal'[self][4]]
                                                                                                /\ hibernate' = [hibernate EXCEPT ![self] = ""]
                                                                                                /\ UNCHANGED continue
                                                                                           ELSE /\ IF isAtom(rVal'[self][4]) /\ rVal'[self][4] =  "hibernate"
                                                                                                      THEN /\ hibernate' = [hibernate EXCEPT ![self] = "hibernate"]
                                                                                                           /\ UNCHANGED continue
                                                                                                      ELSE /\ IF isTuple(rVal'[self][4]) /\ Len(tuple_to_list(rVal'[self][4])) = 2 /\ Head(tuple_to_list(rVal'[self][4])) =  "continue"
                                                                                                                 THEN /\ continue' = [continue EXCEPT ![self] = rVal'[self][4]]
                                                                                                                 ELSE /\ TRUE
                                                                                                                      /\ UNCHANGED continue
                                                                                                           /\ UNCHANGED hibernate
                                                                                                /\ UNCHANGED timeout_
                                                                                     /\ UNCHANGED << openConnection, 
                                                                                                     wasStopped, 
                                                                                                     serverUp >>
                                                                                ELSE /\ IF isSeq(rVal'[self]) /\ Len(rVal'[self]) = 2 /\ rVal'[self][1] =  "noreply"
                                                                                           THEN /\ serverState' = [serverState EXCEPT ![self] = rVal'[self][2]]
                                                                                                /\ hibernate' = [hibernate EXCEPT ![self] = ""]
                                                                                                /\ UNCHANGED << queues, 
                                                                                                                openConnection, 
                                                                                                                wasStopped, 
                                                                                                                replyTrace, 
                                                                                                                serverUp, 
                                                                                                                timeout_, 
                                                                                                                continue >>
                                                                                           ELSE /\ IF isSeq(rVal'[self]) /\ Len(rVal'[self]) = 3 /\ rVal'[self][1] =  "noreply"
                                                                                                      THEN /\ serverState' = [serverState EXCEPT ![self] = rVal'[self][2]]
                                                                                                           /\ IF isTimeout(rVal'[self][3])
                                                                                                                 THEN /\ timeout_' = [timeout_ EXCEPT ![self] = rVal'[self][3]]
                                                                                                                      /\ hibernate' = [hibernate EXCEPT ![self] = ""]
                                                                                                                      /\ UNCHANGED continue
                                                                                                                 ELSE /\ IF isAtom(rVal'[self][3]) /\ rVal'[self][3] =  "hibernate"
                                                                                                                            THEN /\ hibernate' = [hibernate EXCEPT ![self] = "hibernate"]
                                                                                                                                 /\ UNCHANGED continue
                                                                                                                            ELSE /\ IF isTuple(rVal'[self][3]) /\ Len(tuple_to_list(rVal'[self][3])) = 2 /\ Head(tuple_to_list(rVal'[self][3])) =  "continue"
                                                                                                                                       THEN /\ continue' = [continue EXCEPT ![self] = rVal'[self][3]]
                                                                                                                                       ELSE /\ TRUE
                                                                                                                                            /\ UNCHANGED continue
                                                                                                                                 /\ UNCHANGED hibernate
                                                                                                                      /\ UNCHANGED timeout_
                                                                                                           /\ UNCHANGED << queues, 
                                                                                                                           openConnection, 
                                                                                                                           wasStopped, 
                                                                                                                           replyTrace, 
                                                                                                                           serverUp >>
                                                                                                      ELSE /\ IF isSeq(rVal'[self]) /\ Len(rVal'[self]) = 4 /\ (rVal'[self])[1] =  "stop"
                                                                                                                 THEN /\ IF (smsg'[self][2])[2] > 0
                                                                                                                            THEN /\ queues' = [queues EXCEPT ![((smsg'[self][2])[1])] = @ (+) (       [
                                                                                                                                                  result      |-> (rVal'[self][3]),
                                                                                                                                                  requestId   |-> (smsg'[self][2])[2],
                                                                                                                                                  tag         |-> "reply"
                                                                                                                                              ])]
                                                                                                                            ELSE /\ TRUE
                                                                                                                                 /\ UNCHANGED queues
                                                                                                                      /\ replyTrace' = replyTrace (+) (smsg'[self][2])[3]
                                                                                                                      /\ hibernate' = [hibernate EXCEPT ![self] = ""]
                                                                                                                      /\ serverUp' = [serverUp EXCEPT ![self] = FALSE]
                                                                                                                      /\ openConnection' = [openConnection EXCEPT ![self] = FALSE]
                                                                                                                      /\ wasStopped' = [wasStopped EXCEPT ![self] = TRUE]
                                                                                                                 ELSE /\ IF isSeq(rVal'[self]) /\ Len(rVal'[self]) = 3 /\ (rVal'[self])[1] =  "stop"
                                                                                                                            THEN /\ serverUp' = [serverUp EXCEPT ![self] = FALSE]
                                                                                                                                 /\ openConnection' = [openConnection EXCEPT ![self] = FALSE]
                                                                                                                                 /\ wasStopped' = [wasStopped EXCEPT ![self] = TRUE]
                                                                                                                                 /\ hibernate' = [hibernate EXCEPT ![self] = ""]
                                                                                                                            ELSE /\ IF ~isSeq(rVal'[self])
                                                                                                                                       THEN /\ serverUp' = [serverUp EXCEPT ![self] = FALSE]
                                                                                                                                            /\ openConnection' = [openConnection EXCEPT ![self] = FALSE]
                                                                                                                                            /\ wasStopped' = [wasStopped EXCEPT ![self] = TRUE]
                                                                                                                                            /\ hibernate' = [hibernate EXCEPT ![self] = ""]
                                                                                                                                       ELSE /\ TRUE
                                                                                                                                            /\ UNCHANGED << openConnection, 
                                                                                                                                                            wasStopped, 
                                                                                                                                                            serverUp, 
                                                                                                                                                            hibernate >>
                                                                                                                      /\ UNCHANGED << queues, 
                                                                                                                                      replyTrace >>
                                                                                                           /\ UNCHANGED << serverState, 
                                                                                                                           timeout_, 
                                                                                                                           continue >>
                                                          ELSE /\ IF isSeq(smsg'[self]) /\ Len(smsg'[self]) = 2 /\ smsg'[self][1] =  "$gen_cast"
                                                                     THEN /\ rVal' = [rVal EXCEPT ![self] = <<"noreply", serverState[self] (+) <<(smsg'[self][2])>>>>]
                                                                          /\ IF isSeq(rVal'[self]) /\ Len(rVal'[self]) = 2 /\ rVal'[self][1] =  "noreply"
                                                                                THEN /\ serverState' = [serverState EXCEPT ![self] = rVal'[self][2]]
                                                                                     /\ hibernate' = [hibernate EXCEPT ![self] = ""]
                                                                                     /\ UNCHANGED << openConnection, 
                                                                                                     wasStopped, 
                                                                                                     serverUp, 
                                                                                                     timeout_, 
                                                                                                     continue >>
                                                                                ELSE /\ IF isSeq(rVal'[self]) /\ Len(rVal'[self]) = 3 /\ rVal'[self][1] =  "noreply"
                                                                                           THEN /\ serverState' = [serverState EXCEPT ![self] = rVal'[self][2]]
                                                                                                /\ IF isTimeout(rVal'[self][3])
                                                                                                      THEN /\ timeout_' = [timeout_ EXCEPT ![self] = rVal'[self][3]]
                                                                                                           /\ hibernate' = [hibernate EXCEPT ![self] = ""]
                                                                                                           /\ UNCHANGED continue
                                                                                                      ELSE /\ IF isAtom(rVal'[self][3]) /\ rVal'[self][3] =  "hibernate"
                                                                                                                 THEN /\ hibernate' = [hibernate EXCEPT ![self] = "hibernate"]
                                                                                                                      /\ UNCHANGED continue
                                                                                                                 ELSE /\ IF isTuple(rVal'[self][3]) /\ Len(tuple_to_list(rVal'[self][3])) = 2 /\ Head(tuple_to_list(rVal'[self][3])) =  "continue"
                                                                                                                            THEN /\ continue' = [continue EXCEPT ![self] = rVal'[self][3]]
                                                                                                                            ELSE /\ TRUE
                                                                                                                                 /\ UNCHANGED continue
                                                                                                                      /\ UNCHANGED hibernate
                                                                                                           /\ UNCHANGED timeout_
                                                                                                /\ UNCHANGED << openConnection, 
                                                                                                                wasStopped, 
                                                                                                                serverUp >>
                                                                                           ELSE /\ IF isSeq(rVal'[self]) /\ Len(rVal'[self]) = 3 /\ rVal'[self][1] =  "stop"
                                                                                                      THEN /\ serverState' = [serverState EXCEPT ![self] = rVal'[self][3]]
                                                                                                           /\ serverUp' = [serverUp EXCEPT ![self] = FALSE]
                                                                                                           /\ openConnection' = [openConnection EXCEPT ![self] = FALSE]
                                                                                                           /\ wasStopped' = [wasStopped EXCEPT ![self] = TRUE]
                                                                                                           /\ hibernate' = [hibernate EXCEPT ![self] = ""]
                                                                                                      ELSE /\ TRUE
                                                                                                           /\ UNCHANGED << openConnection, 
                                                                                                                           wasStopped, 
                                                                                                                           serverUp, 
                                                                                                                           serverState, 
                                                                                                                           hibernate >>
                                                                                                /\ UNCHANGED << timeout_, 
                                                                                                                continue >>
                                                                     ELSE /\ IF isExported(module_[self],  "handle_info", 2) /\ ~(isSeq(continue[self]) /\ Len(continue[self]) = 1)
                                                                                THEN /\ rVal' = [rVal EXCEPT ![self] = <<"noreply", serverState[self] (+) raw_msg[self]>>]
                                                                                     /\ IF isSeq(rVal'[self]) /\ Len(rVal'[self]) = 2 /\ rVal'[self][1] =  "noreply"
                                                                                           THEN /\ serverState' = [serverState EXCEPT ![self] = rVal'[self][2]]
                                                                                                /\ hibernate' = [hibernate EXCEPT ![self] = ""]
                                                                                                /\ UNCHANGED << openConnection, 
                                                                                                                wasStopped, 
                                                                                                                serverUp, 
                                                                                                                timeout_, 
                                                                                                                continue >>
                                                                                           ELSE /\ IF isSeq(rVal'[self]) /\ Len(rVal'[self]) = 3 /\ rVal'[self][1] =  "noreply"
                                                                                                      THEN /\ serverState' = [serverState EXCEPT ![self] = rVal'[self][2]]
                                                                                                           /\ IF isTimeout(rVal'[self][3])
                                                                                                                 THEN /\ timeout_' = [timeout_ EXCEPT ![self] = rVal'[self][3]]
                                                                                                                      /\ hibernate' = [hibernate EXCEPT ![self] = ""]
                                                                                                                      /\ UNCHANGED continue
                                                                                                                 ELSE /\ IF isAtom(rVal'[self][3]) /\ rVal'[self][3] =  "hibernate"
                                                                                                                            THEN /\ hibernate' = [hibernate EXCEPT ![self] = "hibernate"]
                                                                                                                                 /\ UNCHANGED continue
                                                                                                                            ELSE /\ IF isTuple(rVal'[self][3]) /\ Len(tuple_to_list(rVal'[self][3])) = 2 /\ Head(tuple_to_list(rVal'[self][3])) =  "continue"
                                                                                                                                       THEN /\ continue' = [continue EXCEPT ![self] = rVal'[self][3]]
                                                                                                                                       ELSE /\ TRUE
                                                                                                                                            /\ UNCHANGED continue
                                                                                                                                 /\ UNCHANGED hibernate
                                                                                                                      /\ UNCHANGED timeout_
                                                                                                           /\ UNCHANGED << openConnection, 
                                                                                                                           wasStopped, 
                                                                                                                           serverUp >>
                                                                                                      ELSE /\ IF isSeq(rVal'[self]) /\ Len(rVal'[self]) = 3 /\ rVal'[self][1] =  "stop"
                                                                                                                 THEN /\ serverState' = [serverState EXCEPT ![self] = rVal'[self][3]]
                                                                                                                      /\ hibernate' = [hibernate EXCEPT ![self] = ""]
                                                                                                                      /\ serverUp' = [serverUp EXCEPT ![self] = FALSE]
                                                                                                                      /\ openConnection' = [openConnection EXCEPT ![self] = FALSE]
                                                                                                                      /\ wasStopped' = [wasStopped EXCEPT ![self] = TRUE]
                                                                                                                 ELSE /\ TRUE
                                                                                                                      /\ UNCHANGED << openConnection, 
                                                                                                                                      wasStopped, 
                                                                                                                                      serverUp, 
                                                                                                                                      serverState, 
                                                                                                                                      hibernate >>
                                                                                                           /\ UNCHANGED << timeout_, 
                                                                                                                           continue >>
                                                                                ELSE /\ IF isSeq(smsg'[self]) /\ Len(smsg'[self]) = 2 /\ smsg'[self][1] =  "noreply"
                                                                                           THEN /\ serverState' = [serverState EXCEPT ![self] = smsg'[self][2]]
                                                                                                /\ hibernate' = [hibernate EXCEPT ![self] = ""]
                                                                                                /\ continue' = [continue EXCEPT ![self] = <<>>]
                                                                                                /\ UNCHANGED << openConnection, 
                                                                                                                wasStopped, 
                                                                                                                serverUp, 
                                                                                                                timeout_ >>
                                                                                           ELSE /\ IF isSeq(smsg'[self]) /\ Len(smsg'[self]) = 3 /\ smsg'[self][1] =  "noreply"
                                                                                                      THEN /\ serverState' = [serverState EXCEPT ![self] = smsg'[self][2]]
                                                                                                           /\ IF isTimeout(smsg'[self][3])
                                                                                                                 THEN /\ timeout_' = [timeout_ EXCEPT ![self] = smsg'[self][3]]
                                                                                                                      /\ hibernate' = [hibernate EXCEPT ![self] = ""]
                                                                                                                      /\ continue' = [continue EXCEPT ![self] = <<>>]
                                                                                                                 ELSE /\ IF isAtom(smsg'[self][3]) /\ smsg'[self][3] =  "hibernate"
                                                                                                                            THEN /\ hibernate' = [hibernate EXCEPT ![self] = "hibernate"]
                                                                                                                                 /\ continue' = [continue EXCEPT ![self] = <<>>]
                                                                                                                            ELSE /\ IF isTuple(smsg'[self][3]) /\ Len(tuple_to_list(smsg'[self][3])) = 2 /\ Head(tuple_to_list(smsg'[self][3])) =  "continue"
                                                                                                                                       THEN /\ continue' = [continue EXCEPT ![self] = smsg'[self][3]]
                                                                                                                                       ELSE /\ TRUE
                                                                                                                                            /\ UNCHANGED continue
                                                                                                                                 /\ UNCHANGED hibernate
                                                                                                                      /\ UNCHANGED timeout_
                                                                                                           /\ UNCHANGED << openConnection, 
                                                                                                                           wasStopped, 
                                                                                                                           serverUp >>
                                                                                                      ELSE /\ IF isSeq(smsg'[self]) /\ Len(smsg'[self]) = 3 /\ smsg'[self][1] =  "stop"
                                                                                                                 THEN /\ serverState' = [serverState EXCEPT ![self] = smsg'[self][3]]
                                                                                                                      /\ hibernate' = [hibernate EXCEPT ![self] = ""]
                                                                                                                      /\ serverUp' = [serverUp EXCEPT ![self] = FALSE]
                                                                                                                      /\ openConnection' = [openConnection EXCEPT ![self] = FALSE]
                                                                                                                      /\ wasStopped' = [wasStopped EXCEPT ![self] = TRUE]
                                                                                                                      /\ continue' = [continue EXCEPT ![self] = <<>>]
                                                                                                                 ELSE /\ TRUE
                                                                                                                      /\ UNCHANGED << openConnection, 
                                                                                                                                      wasStopped, 
                                                                                                                                      serverUp, 
                                                                                                                                      serverState, 
                                                                                                                                      hibernate, 
                                                                                                                                      continue >>
                                                                                                           /\ UNCHANGED timeout_
                                                                                     /\ rVal' = rVal
                                                               /\ UNCHANGED << queues, 
                                                                               replyTrace >>
                         ELSE /\ TRUE
                              /\ UNCHANGED << queues, openConnection, 
                                              wasStopped, rVal, replyTrace, 
                                              smsg, serverUp, serverState, 
                                              timeout_, hibernate, continue >>
                   /\ initResult' = [initResult EXCEPT ![self] = null]
                   /\ rValTemp' = [rValTemp EXCEPT ![self] = null]
                   /\ raw_msg' = [raw_msg EXCEPT ![self] = null]
                   /\ skipLoop' = [skipLoop EXCEPT ![self] = FALSE]
                   /\ skipLoopHibernate' = [skipLoopHibernate EXCEPT ![self] = FALSE]
                   /\ pc' = [pc EXCEPT ![self] = "S_ENTER_LOOP"]
                   /\ UNCHANGED << null, rootModule, uuid, signals, mRefTable, processesLinked, processesMonitored, calls, results, timers, clientUp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, globalClock, reqStart, reqTerminate, canStart, hibernateAfterTimeout, skipWait, parent, name, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

S_RECEIVE(self) == /\ pc[self] = "S_RECEIVE"
                   /\ Len(queues[self]) # 0 \/ Cardinality(signals[self]) # 0 \/ (isTimeout(timeout_[self]) /\ timers[self].isReached)
                   /\ IF Len(queues[self]) # 0
                         THEN /\ raw_msg' = [raw_msg EXCEPT ![self] = Head(queues[self])]
                              /\ queues' = [queues EXCEPT ![self] = Tail(queues[self])]
                              /\ skipLoop' = [skipLoop EXCEPT ![self] = FALSE]
                              /\ UNCHANGED << signals, mRefs >>
                         ELSE /\ IF Cardinality(signals[self]) # 0
                                    THEN /\ \E signal \in signals[self]:
                                              /\ IF signal[1] = "MONITOR"
                                                    THEN /\ mRefs' = [mRefs EXCEPT ![self] = mRefs[self] ++ {<<signal[2], signal[3]>>}]
                                                    ELSE /\ IF signal[1] = "DEMONITOR"
                                                               THEN /\ mRefs' = [mRefs EXCEPT ![self] = mRefs[self] \ {<<signal[2], signal[3]>>}]
                                                               ELSE /\ TRUE
                                                                    /\ mRefs' = mRefs
                                              /\ signals' = [signals EXCEPT ![signal[3]] = @ ++ {<<"ACK", signal[2]>>}, ![self] = @ \ {signal}]
                                         /\ skipLoop' = [skipLoop EXCEPT ![self] = TRUE]
                                         /\ UNCHANGED raw_msg
                                    ELSE /\ IF timers[self].isReached
                                               THEN /\ raw_msg' = [raw_msg EXCEPT ![self] = "timeout"]
                                               ELSE /\ TRUE
                                                    /\ UNCHANGED raw_msg
                                         /\ UNCHANGED << signals, skipLoop, 
                                                         mRefs >>
                              /\ UNCHANGED queues
                   /\ IF raw_msg'[self] =  "timeout" \/ Head(tuple_to_list(raw_msg'[self])) #  "system"
                         THEN /\ timeout_' = [timeout_ EXCEPT ![self] = "infinity"]
                         ELSE /\ TRUE
                              /\ UNCHANGED timeout_
                   /\ pc' = [pc EXCEPT ![self] = "S_PROCESS"]
                   /\ UNCHANGED << null, rootModule, uuid, mRefTable, processesLinked, processesMonitored, calls, results, timers, clientUp, openConnection, wasStopped, rVal, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, serverUp, serverState, hibernate, continue, hibernateAfterTimeout, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

S_RECEIVE_receiveSignal(self) == /\ pc[self] = "S_RECEIVE_receiveSignal"
                                 /\ Len(queues[self]) # 0 \/ Cardinality(signals[self]) # 0 \/ (isTimeout(hibernateAfterTimeout[self]) /\ timers[self].isReached)
                                 /\ IF Len(queues[self]) # 0
                                       THEN /\ raw_msg' = [raw_msg EXCEPT ![self] = Head(queues[self])]
                                            /\ queues' = [queues EXCEPT ![self] = Tail(queues[self])]
                                            /\ skipLoop' = [skipLoop EXCEPT ![self] = FALSE]
                                            /\ UNCHANGED << signals, mRefs >>
                                       ELSE /\ IF Cardinality(signals[self]) # 0
                                                  THEN /\ \E signal \in signals[self]:
                                                            /\ IF signal[1] = "MONITOR"
                                                                  THEN /\ mRefs' = [mRefs EXCEPT ![self] = mRefs[self] ++ {<<signal[2], signal[3]>>}]
                                                                  ELSE /\ IF signal[1] = "DEMONITOR"
                                                                             THEN /\ mRefs' = [mRefs EXCEPT ![self] = mRefs[self] \ {<<signal[2], signal[3]>>}]
                                                                             ELSE /\ TRUE
                                                                                  /\ mRefs' = mRefs
                                                            /\ signals' = [signals EXCEPT ![signal[3]] = @ ++ {<<"ACK", signal[2]>>}, ![self] = @ \ {signal}]
                                                       /\ skipLoop' = [skipLoop EXCEPT ![self] = TRUE]
                                                       /\ UNCHANGED raw_msg
                                                  ELSE /\ IF timers[self].isReached
                                                             THEN /\ raw_msg' = [raw_msg EXCEPT ![self] = "timeout"]
                                                             ELSE /\ TRUE
                                                                  /\ UNCHANGED raw_msg
                                                       /\ UNCHANGED << signals, 
                                                                       skipLoop, 
                                                                       mRefs >>
                                            /\ UNCHANGED queues
                                 /\ IF isAtom(raw_msg'[self]) /\ raw_msg'[self] =  "timeout"
                                       THEN /\ hibernate' = [hibernate EXCEPT ![self] = "hibernate"]
                                            /\ skipLoopHibernate' = [skipLoopHibernate EXCEPT ![self] = TRUE]
                                       ELSE /\ TRUE
                                            /\ UNCHANGED << hibernate, 
                                                            skipLoopHibernate >>
                                 /\ pc' = [pc EXCEPT ![self] = "S_PROCESS"]
                                 /\ UNCHANGED << null, rootModule, uuid, mRefTable, processesLinked, processesMonitored, calls, results, timers, clientUp, openConnection, wasStopped, rVal, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, serverUp, serverState, timeout_, continue, hibernateAfterTimeout, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

S_RECEIVE_receiveSignal_receiveSignal(self) == /\ pc[self] = "S_RECEIVE_receiveSignal_receiveSignal"
                                               /\ Len(queues[self]) # 0 \/ Cardinality(signals[self]) # 0 \/ (isTimeout("infinity") /\ timers[self].isReached)
                                               /\ IF Len(queues[self]) # 0
                                                     THEN /\ raw_msg' = [raw_msg EXCEPT ![self] = Head(queues[self])]
                                                          /\ queues' = [queues EXCEPT ![self] = Tail(queues[self])]
                                                          /\ skipLoop' = [skipLoop EXCEPT ![self] = FALSE]
                                                          /\ UNCHANGED << signals, 
                                                                          mRefs >>
                                                     ELSE /\ IF Cardinality(signals[self]) # 0
                                                                THEN /\ \E signal \in signals[self]:
                                                                          /\ IF signal[1] = "MONITOR"
                                                                                THEN /\ mRefs' = [mRefs EXCEPT ![self] = mRefs[self] ++ {<<signal[2], signal[3]>>}]
                                                                                ELSE /\ IF signal[1] = "DEMONITOR"
                                                                                           THEN /\ mRefs' = [mRefs EXCEPT ![self] = mRefs[self] \ {<<signal[2], signal[3]>>}]
                                                                                           ELSE /\ TRUE
                                                                                                /\ mRefs' = mRefs
                                                                          /\ signals' = [signals EXCEPT ![signal[3]] = @ ++ {<<"ACK", signal[2]>>}, ![self] = @ \ {signal}]
                                                                     /\ skipLoop' = [skipLoop EXCEPT ![self] = TRUE]
                                                                     /\ UNCHANGED raw_msg
                                                                ELSE /\ IF timers[self].isReached
                                                                           THEN /\ raw_msg' = [raw_msg EXCEPT ![self] = "timeout"]
                                                                           ELSE /\ TRUE
                                                                                /\ UNCHANGED raw_msg
                                                                     /\ UNCHANGED << signals, 
                                                                                     skipLoop, 
                                                                                     mRefs >>
                                                          /\ UNCHANGED queues
                                               /\ skipWait' = [skipWait EXCEPT ![self] = FALSE]
                                               /\ skipLoopHibernate' = [skipLoopHibernate EXCEPT ![self] = FALSE]
                                               /\ pc' = [pc EXCEPT ![self] = "S_PROCESS"]
                                               /\ UNCHANGED << null, rootModule, uuid, mRefTable, processesLinked, processesMonitored, calls, results, timers, clientUp, openConnection, wasStopped, rVal, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, parent, name, initResult, module_, mRef_, mRefRes_, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

S_RELEASE_RESOURCES(self) == /\ pc[self] = "S_RELEASE_RESOURCES"
                             /\ z_' = [z_ EXCEPT ![self] = openConnection[self]]
                             /\ queues' =           [pid \in DOMAIN queues |->
                                              IF \E lmRef \in mRefs[self]: lmRef[2] = pid
                                              THEN queues[pid] (+) <<"DOWN", (CHOOSE ref \in mRefs[self]: ref[2] = pid)[1], self, "terminated">>
                                              ELSE queues[pid]
                                          ]
                             /\ IF z_'[self]
                                   THEN /\ serverUp' = [serverUp EXCEPT ![self] = FALSE]
                                        /\ openConnection' = [openConnection EXCEPT ![self] = FALSE]
                                        /\ wasStopped' = [wasStopped EXCEPT ![self] = TRUE]
                                   ELSE /\ TRUE
                                        /\ UNCHANGED << openConnection, 
                                                        wasStopped, serverUp >>
                             /\ pc' = [pc EXCEPT ![self] = "Done"]
                             /\ UNCHANGED << null, rootModule, uuid, signals, mRefTable, processesLinked, processesMonitored, calls, results, timers, clientUp, rVal, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, raw_msg, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

genServer(self) == S_WAIT_SPAWN(self) \/ S_ENTER_LOOP(self)
                      \/ S_PROCESS(self) \/ S_RECEIVE(self)
                      \/ S_RECEIVE_receiveSignal(self)
                      \/ S_RECEIVE_receiveSignal_receiveSignal(self)
                      \/ S_RELEASE_RESOURCES(self)

C_WAIT(self) == /\ pc[self] = "C_WAIT"
                /\ canStart
                /\ pc' = [pc EXCEPT ![self] = "C_LOOP"]
                /\ UNCHANGED << null, rootModule, uuid, queues, signals, mRefTable, processesLinked, processesMonitored, calls, results, timers, clientUp, openConnection, wasStopped, rVal, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

C_LOOP(self) == /\ pc[self] = "C_LOOP"
                /\ IF iterations[self] > 0
                      THEN /\ \/ /\ timeout' = [timeout EXCEPT ![self] = FALSE]
                                 /\ rValTemp1' = [rValTemp1 EXCEPT ![self] = <<"$gen_call", "#CALL INF", timeout'[self], uuid>>]
                                 /\ signals' = [signals EXCEPT ![SERVERID] = @ ++ ({<<"MONITOR", uuid, self>>})]
                                 /\ mRef_w' = [mRef_w EXCEPT ![self] = uuid]
                                 /\ mRefTable' = [mRefTable EXCEPT ![mRef_w'[self]] = SERVERID]
                                 /\ uuid' = uuid + 1
                                 /\ pc' = [pc EXCEPT ![self] = "MONITOR_WAIT_FOR_ACK_monitor_genCall"]
                                 /\ UNCHANGED <<queues, rValTemp, rValTemp2, module_w>>
                              \/ /\ timeout' = [timeout EXCEPT ![self] = TRUE]
                                 /\ rValTemp1' = [rValTemp1 EXCEPT ![self] = <<"$gen_call", "#CALL TMO", timeout'[self], uuid>>]
                                 /\ signals' = [signals EXCEPT ![SERVERID] = @ ++ ({<<"MONITOR", uuid, self>>})]
                                 /\ mRef_w' = [mRef_w EXCEPT ![self] = uuid]
                                 /\ mRefTable' = [mRefTable EXCEPT ![mRef_w'[self]] = SERVERID]
                                 /\ uuid' = uuid + 1
                                 /\ pc' = [pc EXCEPT ![self] = "MONITOR_WAIT_FOR_ACK_monitor_genCall_call3"]
                                 /\ UNCHANGED <<queues, rValTemp, rValTemp2, module_w>>
                              \/ /\ IF isTuple(SERVERID)
                                       THEN /\ rValTemp' = [rValTemp EXCEPT ![self] = tuple_to_list(SERVERID)]
                                            /\ IF rValTemp'[self][1] =  "global"
                                                  THEN /\ queues' = [queues EXCEPT ![(rValTemp'[self][2])] = @ (+) (list_to_tuple(<< "$gen_cast", "#CAST">>))]
                                                       /\ UNCHANGED module_w
                                                  ELSE /\ IF rValTemp'[self][1] =  "via"
                                                             THEN /\ module_w' = [module_w EXCEPT ![self] = rValTemp'[self][2]]
                                                                  /\ queues' = [queues EXCEPT ![(rValTemp'[self][3])] = @ (+) (list_to_tuple(<< "$gen_cast", "#CAST">>))]
                                                             ELSE /\ IF isSeq(SERVERID)
                                                                        THEN /\ queues' = [queues EXCEPT ![(SERVERID[1])] = @ (+) (list_to_tuple(<< "$gen_cast", "#CAST">>))]
                                                                        ELSE /\ queues' = [queues EXCEPT ![SERVERID] = @ (+) (list_to_tuple(<< "$gen_cast", "#CAST">>))]
                                                                  /\ UNCHANGED module_w
                                       ELSE /\ IF isSeq(SERVERID)
                                                  THEN /\ queues' = [queues EXCEPT ![(SERVERID[1])] = @ (+) (list_to_tuple(<< "$gen_cast", "#CAST">>))]
                                                  ELSE /\ queues' = [queues EXCEPT ![SERVERID] = @ (+) (list_to_tuple(<< "$gen_cast", "#CAST">>))]
                                            /\ UNCHANGED << rValTemp, module_w >>
                                 /\ rValTemp2' = [rValTemp2 EXCEPT ![self] = "ok"]
                                 /\ rValTemp1' = [rValTemp1 EXCEPT ![self] = <<"$gen_cast", "#CAST">>]
                                 /\ pc' = [pc EXCEPT ![self] = "C_LOG"]
                                 /\ UNCHANGED <<uuid, signals, mRefTable, mRef_w, timeout>>
                              \/ /\ queues' = [queues EXCEPT ![SERVERID] = @ (+) (<<"100">>)]
                                 /\ rValTemp1' = [rValTemp1 EXCEPT ![self] = <<"100">>]
                                 /\ pc' = [pc EXCEPT ![self] = "C_LOG"]
                                 /\ UNCHANGED <<uuid, signals, mRefTable, rValTemp, rValTemp2, module_w, mRef_w, timeout>>
                           /\ UNCHANGED << processesMonitored, clientUp >>
                      ELSE /\ clientUp' = [clientUp EXCEPT ![self] = FALSE]
                           /\ processesMonitored' = processesMonitored \ {self}
                           /\ pc' = [pc EXCEPT ![self] = "Done"]
                           /\ UNCHANGED << uuid, queues, signals, mRefTable, 
                                           rValTemp, rValTemp1, rValTemp2, 
                                           module_w, mRef_w, timeout >>
                /\ UNCHANGED << null, rootModule, processesLinked, calls, results, timers, openConnection, wasStopped, rVal, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, monitorSuccess_, mRefRes_w, reqId, previousReqId, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

C_LOG(self) == /\ pc[self] = "C_LOG"
               /\ trace' = [trace EXCEPT ![self] = trace[self] (+) rValTemp1[self]]
               /\ IF rVal[self] # null
                     THEN /\ callTrace' = [callTrace EXCEPT ![self] = callTrace[self] (+) rVal[self]]
                     ELSE /\ TRUE
                          /\ UNCHANGED callTrace
               /\ timers' = [timers EXCEPT ![self] = [timers[self] EXCEPT !.running = FALSE, !.isReached = FALSE]]
               /\ rVal' = [rVal EXCEPT ![self] = null]
               /\ rValTemp' = [rValTemp EXCEPT ![self] = null]
               /\ rValTemp1' = [rValTemp1 EXCEPT ![self] = null]
               /\ z_w' = [z_w EXCEPT ![self] = null]
               /\ iterations' = [iterations EXCEPT ![self] = iterations[self] - 1]
               /\ pc' = [pc EXCEPT ![self] = "C_LOOP"]
               /\ UNCHANGED << null, rootModule, uuid, queues, signals, mRefTable, processesLinked, processesMonitored, calls, results, clientUp, openConnection, wasStopped, rValTemp2, rValTemp3, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

MONITOR_WAIT_FOR_ACK_monitor_genCall(self) == /\ pc[self] = "MONITOR_WAIT_FOR_ACK_monitor_genCall"
                                              /\ ~openConnection[SERVERID] \/ \E s \in signals[self]: s[1] = "ACK" /\ s[2] = mRef_w[self]
                                              /\ IF ~openConnection[SERVERID] /\ ~(\E s \in signals[self]: s[1] = "ACK" /\ s[2] = mRef_w[self])
                                                    THEN /\ queues' = [queues EXCEPT ![self] = @ (+) <<"DOWN", mRef_w[self], SERVERID, "no_proc">>]
                                                         /\ signals' = [signals EXCEPT ![SERVERID] = signals[SERVERID] \ {<<"MONITOR", mRef_w[self], self>>}]
                                                         /\ UNCHANGED monitorSuccess_
                                                    ELSE /\ signals' = [signals EXCEPT ![self] = signals[self] \ {<<"ACK", mRef_w[self]>>}]
                                                         /\ monitorSuccess_' = [monitorSuccess_ EXCEPT ![self] = TRUE]
                                                         /\ UNCHANGED queues
                                              /\ mRefRes_w' = [mRefRes_w EXCEPT ![self] = mRef_w[self]]
                                              /\ pc' = [pc EXCEPT ![self] = "CALL_SEND_genCall"]
                                              /\ UNCHANGED << null, rootModule, uuid, mRefTable, processesLinked, processesMonitored, calls, results, timers, clientUp, openConnection, wasStopped, rVal, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, mRef_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

CALL_SEND_genCall(self) == /\ pc[self] = "CALL_SEND_genCall"
                           /\ IF monitorSuccess_[self] /\ ~receivedDown(self, mRefRes_w[self])
                                 THEN /\ queues' = [queues EXCEPT ![SERVERID] = @ (+) (<<"$gen_call", <<self, mRefRes_w[self], globalClock>>, "#CALL INF">>)]
                                      /\ globalClock' = globalClock + 1
                                      /\ IF timeout[self]
                                            THEN /\ timers' = [timers EXCEPT ![self] = [timers[self] EXCEPT !.running = TRUE]]
                                            ELSE /\ TRUE
                                                 /\ UNCHANGED timers
                                      /\ pc' = [pc EXCEPT ![self] = "CALL_RECEIVE_genCall"]
                                      /\ UNCHANGED << rVal, rValTemp >>
                                 ELSE /\ rValTemp' = [rValTemp EXCEPT ![self] = <<"nook", [result |-> "error", reason |-> "DOWN", requestId |-> mRefRes_w[self]]>>]
                                      /\ rVal' = [rVal EXCEPT ![self] = [result |-> "error", reason |-> "DOWN", requestId |-> mRefRes_w[self]]]
                                      /\ pc' = [pc EXCEPT ![self] = "CALL_DEMON_genCall"]
                                      /\ UNCHANGED << queues, timers, 
                                                      globalClock >>
                           /\ UNCHANGED << null, rootModule, uuid, signals, mRefTable, processesLinked, processesMonitored, calls, results, clientUp, openConnection, wasStopped, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, reqStart, reqTerminate, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

CALL_RECEIVE_genCall(self) == /\ pc[self] = "CALL_RECEIVE_genCall"
                              /\ Len(queues[self]) # 0 \/ (~(~timeout[self]) /\ timers[self].isReached)
                              /\ IF choosePossible(toSetOfRecs(queues[self]), {mRefRes_w[self]})
                                    THEN /\ rVal' = [rVal EXCEPT ![self] = receiveSelective(self, LAMBDA q: hasKey("requestId", q) /\ q.requestId = mRefRes_w[self])]
                                         /\ queues' = [queues EXCEPT ![self] = deleteFromTuple(queues[self], LAMBDA head: isRecord(head) /\ (rVal'[self]) = head, <<>>)]
                                    ELSE /\ IF receivedDown(self, mRefRes_w[self])
                                               THEN /\ rVal' = [rVal EXCEPT ![self] = [result |-> "error", reason |-> "DOWN", requestId |-> mRefRes_w[self]]]
                                               ELSE /\ IF timers[self].isReached
                                                          THEN /\ rVal' = [rVal EXCEPT ![self] =        [
                                                                                                     result    |-> "timeout",
                                                                                                     requestId |-> mRefRes_w[self]
                                                                                                 ]]
                                                          ELSE /\ rVal' = [rVal EXCEPT ![self] = [result |-> "", other |-> mRefRes_w[self]]]
                                         /\ UNCHANGED queues
                              /\ IF ~reqIdMatch(rVal'[self], mRefRes_w[self]) /\ ~(rVal'[self].result = "timeout" \/ rVal'[self].result = "error")
                                    THEN /\ pc' = [pc EXCEPT ![self] = "CALL_RECEIVE_genCall"]
                                         /\ UNCHANGED << signals, rValTemp >>
                                    ELSE /\ IF reqIdMatch(rVal'[self], mRefRes_w[self])
                                               THEN /\ rValTemp' = [rValTemp EXCEPT ![self] = <<"ok", rVal'[self]>>]
                                                    /\ signals' = [signals EXCEPT ![(unalias(mRefRes_w[self]))] = @ ++ ({<<"DEMONITOR", mRefRes_w[self], self>>})]
                                               ELSE /\ rValTemp' = [rValTemp EXCEPT ![self] = <<"nook", rVal'[self]>>]
                                                    /\ signals' = [signals EXCEPT ![(unalias(mRefRes_w[self]))] = @ ++ ({<<"DEMONITOR", mRefRes_w[self], self>>})]
                                         /\ pc' = [pc EXCEPT ![self] = "CALL_DEMON_genCall"]
                              /\ UNCHANGED << null, rootModule, uuid, mRefTable, processesLinked, processesMonitored, calls, results, timers, clientUp, openConnection, wasStopped, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

CALL_DEMON_genCall(self) == /\ pc[self] = "CALL_DEMON_genCall"
                            /\ receivedDown(self, mRefRes_w[self]) \/ \E s \in signals[self]: s[1] = "ACK" /\ s[2] = mRefRes_w[self]
                            /\ IF ~openConnection[unalias(mRefRes_w[self])]
                                  THEN /\ queues' = [queues EXCEPT ![self] =             deleteFromTuple(queues[self], LAMBDA head:
                                                                             (isSeq(head) /\ head[1] = "DOWN" /\ head[2] = mRefRes_w[self])
                                                                                 \/ (isRecord(head) /\ head(.)<<"requestId", mRefRes_w[self]>>), <<>>)]
                                       /\ UNCHANGED signals
                                  ELSE /\ signals' = [signals EXCEPT ![self] = signals[self] \ {<<"ACK", mRefRes_w[self]>>}]
                                       /\ queues' = [queues EXCEPT ![self] =             deleteFromTuple(queues[self], LAMBDA head:
                                                                             (isRecord(head) /\ head(.)<<"requestId", mRefRes_w[self]>>), <<>>)]
                            /\ monitorSuccess_' = [monitorSuccess_ EXCEPT ![self] = FALSE]
                            /\ IF rValTemp[self][1] =  "ok"
                                  THEN /\ rVal' = [rVal EXCEPT ![self] = rValTemp[self][2]]
                                       /\ pc' = [pc EXCEPT ![self] = "C_LOG"]
                                       /\ UNCHANGED clientUp
                                  ELSE /\ IF rValTemp[self][1] =  "EXIT"
                                             THEN /\ clientUp' = [clientUp EXCEPT ![self] = FALSE]
                                                  /\ pc' = [pc EXCEPT ![self] = "Done"]
                                             ELSE /\ pc' = [pc EXCEPT ![self] = "C_LOG"]
                                                  /\ UNCHANGED clientUp
                                       /\ rVal' = rVal
                            /\ UNCHANGED << null, rootModule, uuid, mRefTable, processesLinked, processesMonitored, calls, results, timers, openConnection, wasStopped, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

MONITOR_WAIT_FOR_ACK_monitor_genCall_call3(self) == /\ pc[self] = "MONITOR_WAIT_FOR_ACK_monitor_genCall_call3"
                                                    /\ ~openConnection[SERVERID] \/ \E s \in signals[self]: s[1] = "ACK" /\ s[2] = mRef_w[self]
                                                    /\ IF ~openConnection[SERVERID] /\ ~(\E s \in signals[self]: s[1] = "ACK" /\ s[2] = mRef_w[self])
                                                          THEN /\ queues' = [queues EXCEPT ![self] = @ (+) <<"DOWN", mRef_w[self], SERVERID, "no_proc">>]
                                                               /\ signals' = [signals EXCEPT ![SERVERID] = signals[SERVERID] \ {<<"MONITOR", mRef_w[self], self>>}]
                                                               /\ UNCHANGED monitorSuccess_
                                                          ELSE /\ signals' = [signals EXCEPT ![self] = signals[self] \ {<<"ACK", mRef_w[self]>>}]
                                                               /\ monitorSuccess_' = [monitorSuccess_ EXCEPT ![self] = TRUE]
                                                               /\ UNCHANGED queues
                                                    /\ mRefRes_w' = [mRefRes_w EXCEPT ![self] = mRef_w[self]]
                                                    /\ pc' = [pc EXCEPT ![self] = "CALL_SEND_genCall_call3"]
                                                    /\ UNCHANGED << null, rootModule, uuid, mRefTable, processesLinked, processesMonitored, calls, results, timers, clientUp, openConnection, wasStopped, rVal, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, mRef_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

CALL_SEND_genCall_call3(self) == /\ pc[self] = "CALL_SEND_genCall_call3"
                                 /\ IF monitorSuccess_[self] /\ ~receivedDown(self, mRefRes_w[self])
                                       THEN /\ queues' = [queues EXCEPT ![SERVERID] = @ (+) (<<"$gen_call", <<self, mRefRes_w[self], globalClock>>, "#CALL TMO">>)]
                                            /\ globalClock' = globalClock + 1
                                            /\ IF timeout[self]
                                                  THEN /\ timers' = [timers EXCEPT ![self] = [timers[self] EXCEPT !.running = TRUE]]
                                                  ELSE /\ TRUE
                                                       /\ UNCHANGED timers
                                            /\ pc' = [pc EXCEPT ![self] = "CALL_RECEIVE_genCall_call3"]
                                            /\ UNCHANGED << rVal, rValTemp >>
                                       ELSE /\ rValTemp' = [rValTemp EXCEPT ![self] = <<"nook", [result |-> "error", reason |-> "DOWN", requestId |-> mRefRes_w[self]]>>]
                                            /\ rVal' = [rVal EXCEPT ![self] = [result |-> "error", reason |-> "DOWN", requestId |-> mRefRes_w[self]]]
                                            /\ pc' = [pc EXCEPT ![self] = "CALL_DEMON_genCall_call3"]
                                            /\ UNCHANGED << queues, timers, 
                                                            globalClock >>
                                 /\ UNCHANGED << null, rootModule, uuid, signals, mRefTable, processesLinked, processesMonitored, calls, results, clientUp, openConnection, wasStopped, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, reqStart, reqTerminate, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

CALL_RECEIVE_genCall_call3(self) == /\ pc[self] = "CALL_RECEIVE_genCall_call3"
                                    /\ Len(queues[self]) # 0 \/ (~(~timeout[self]) /\ timers[self].isReached)
                                    /\ IF choosePossible(toSetOfRecs(queues[self]), {mRefRes_w[self]})
                                          THEN /\ rVal' = [rVal EXCEPT ![self] = receiveSelective(self, LAMBDA q: hasKey("requestId", q) /\ q.requestId = mRefRes_w[self])]
                                               /\ queues' = [queues EXCEPT ![self] = deleteFromTuple(queues[self], LAMBDA head: isRecord(head) /\ (rVal'[self]) = head, <<>>)]
                                          ELSE /\ IF receivedDown(self, mRefRes_w[self])
                                                     THEN /\ rVal' = [rVal EXCEPT ![self] = [result |-> "error", reason |-> "DOWN", requestId |-> mRefRes_w[self]]]
                                                     ELSE /\ IF timers[self].isReached
                                                                THEN /\ rVal' = [rVal EXCEPT ![self] =        [
                                                                                                           result    |-> "timeout",
                                                                                                           requestId |-> mRefRes_w[self]
                                                                                                       ]]
                                                                ELSE /\ rVal' = [rVal EXCEPT ![self] = [result |-> "", other |-> mRefRes_w[self]]]
                                               /\ UNCHANGED queues
                                    /\ IF ~reqIdMatch(rVal'[self], mRefRes_w[self]) /\ ~(rVal'[self].result = "timeout" \/ rVal'[self].result = "error")
                                          THEN /\ pc' = [pc EXCEPT ![self] = "CALL_RECEIVE_genCall"]
                                               /\ UNCHANGED << signals, 
                                                               rValTemp >>
                                          ELSE /\ IF reqIdMatch(rVal'[self], mRefRes_w[self])
                                                     THEN /\ rValTemp' = [rValTemp EXCEPT ![self] = <<"ok", rVal'[self]>>]
                                                          /\ signals' = [signals EXCEPT ![(unalias(mRefRes_w[self]))] = @ ++ ({<<"DEMONITOR", mRefRes_w[self], self>>})]
                                                     ELSE /\ rValTemp' = [rValTemp EXCEPT ![self] = <<"nook", rVal'[self]>>]
                                                          /\ signals' = [signals EXCEPT ![(unalias(mRefRes_w[self]))] = @ ++ ({<<"DEMONITOR", mRefRes_w[self], self>>})]
                                               /\ pc' = [pc EXCEPT ![self] = "CALL_DEMON_genCall_call3"]
                                    /\ UNCHANGED << null, rootModule, uuid, mRefTable, processesLinked, processesMonitored, calls, results, timers, clientUp, openConnection, wasStopped, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

CALL_DEMON_genCall_call3(self) == /\ pc[self] = "CALL_DEMON_genCall_call3"
                                  /\ receivedDown(self, mRefRes_w[self]) \/ \E s \in signals[self]: s[1] = "ACK" /\ s[2] = mRefRes_w[self]
                                  /\ IF ~openConnection[unalias(mRefRes_w[self])]
                                        THEN /\ queues' = [queues EXCEPT ![self] =             deleteFromTuple(queues[self], LAMBDA head:
                                                                                   (isSeq(head) /\ head[1] = "DOWN" /\ head[2] = mRefRes_w[self])
                                                                                       \/ (isRecord(head) /\ head(.)<<"requestId", mRefRes_w[self]>>), <<>>)]
                                             /\ UNCHANGED signals
                                        ELSE /\ signals' = [signals EXCEPT ![self] = signals[self] \ {<<"ACK", mRefRes_w[self]>>}]
                                             /\ queues' = [queues EXCEPT ![self] =             deleteFromTuple(queues[self], LAMBDA head:
                                                                                   (isRecord(head) /\ head(.)<<"requestId", mRefRes_w[self]>>), <<>>)]
                                  /\ monitorSuccess_' = [monitorSuccess_ EXCEPT ![self] = FALSE]
                                  /\ IF rValTemp[self][1] =  "ok"
                                        THEN /\ rVal' = [rVal EXCEPT ![self] = rValTemp[self][2]]
                                             /\ pc' = [pc EXCEPT ![self] = "C_LOG"]
                                             /\ UNCHANGED clientUp
                                        ELSE /\ IF rValTemp[self][1] =  "EXIT"
                                                   THEN /\ clientUp' = [clientUp EXCEPT ![self] = FALSE]
                                                        /\ pc' = [pc EXCEPT ![self] = "Done"]
                                                   ELSE /\ pc' = [pc EXCEPT ![self] = "C_LOG"]
                                                        /\ UNCHANGED clientUp
                                             /\ rVal' = rVal
                                  /\ UNCHANGED << null, rootModule, uuid, mRefTable, processesLinked, processesMonitored, calls, results, timers, openConnection, wasStopped, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

workerClient(self) == C_WAIT(self) \/ C_LOOP(self) \/ C_LOG(self)
                         \/ MONITOR_WAIT_FOR_ACK_monitor_genCall(self)
                         \/ CALL_SEND_genCall(self)
                         \/ CALL_RECEIVE_genCall(self)
                         \/ CALL_DEMON_genCall(self)
                         \/ MONITOR_WAIT_FOR_ACK_monitor_genCall_call3(self)
                         \/ CALL_SEND_genCall_call3(self)
                         \/ CALL_RECEIVE_genCall_call3(self)
                         \/ CALL_DEMON_genCall_call3(self)

W_START(self) == /\ pc[self] = "W_START"
                 /\ reqStart' = [reqStart EXCEPT ![self] = TRUE]
                 /\ IF isSeq((<<[hibernate_after |-> 0]>>)) /\ isAtom("gen_server_callbacks") /\ isTuple((<< "local", "server">>))
                       THEN /\ IF ~openConnection[SERVERID]
                                  THEN /\ IF "nolink" =  "nolink"
                                             THEN /\ Len(calls[SERVERID]) = 0
                                                  /\ calls' = [calls EXCEPT ![SERVERID] = calls[SERVERID] (+) <<self, "init_it", (                                 [
                                                                                              starter    |-> rootModule,
                                                                                              parent     |-> self,
                                                                                              parentName |-> "self",
                                                                                              name       |-> (<< "local", "server">>),
                                                                                              module     |-> "gen_server_callbacks",
                                                                                              args       |-> (<<>>),
                                                                                              options    |-> (<<[hibernate_after |-> 0]>>)
                                                                                          ])>>]
                                                  /\ queues' = [queues EXCEPT ![SERVERID] = queues[SERVERID] (+) <<"$sys">>]
                                                  /\ z' = [z EXCEPT ![self] = openConnection[SERVERID]]
                                                  /\ pc' = [pc EXCEPT ![self] = "C_WAIT_ACK_genStart6_genStart6"]
                                                  /\ UNCHANGED << processesLinked, 
                                                                  processesMonitored >>
                                             ELSE /\ IF "nolink" =  "monitor"
                                                        THEN /\ Len(calls[SERVERID]) = 0
                                                             /\ calls' = [calls EXCEPT ![SERVERID] = calls[SERVERID] (+) <<self, "init_it", (                                 [
                                                                                                         starter    |-> rootModule,
                                                                                                         parent     |-> self,
                                                                                                         parentName |-> "self",
                                                                                                         name       |-> (<< "local", "server">>),
                                                                                                         module     |-> "gen_server_callbacks",
                                                                                                         args       |-> (<<>>),
                                                                                                         options    |-> (<<[hibernate_after |-> 0]>>)
                                                                                                     ])>>]
                                                             /\ queues' = [queues EXCEPT ![SERVERID] = queues[SERVERID] (+) <<"$sys">>]
                                                             /\ processesMonitored' = processesMonitored ++ {self}
                                                             /\ z' = [z EXCEPT ![self] = openConnection[SERVERID]]
                                                             /\ pc' = [pc EXCEPT ![self] = "C_WAIT_ACK_LINK_genStart6_genStart6"]
                                                             /\ UNCHANGED processesLinked
                                                        ELSE /\ IF "nolink" =  "link"
                                                                   THEN /\ Len(calls[SERVERID]) = 0
                                                                        /\ calls' = [calls EXCEPT ![SERVERID] = calls[SERVERID] (+) <<self, "init_it", (                                 [
                                                                                                                    starter    |-> rootModule,
                                                                                                                    parent     |-> self,
                                                                                                                    parentName |-> "self",
                                                                                                                    name       |-> (<< "local", "server">>),
                                                                                                                    module     |-> "gen_server_callbacks",
                                                                                                                    args       |-> (<<>>),
                                                                                                                    options    |-> (<<[hibernate_after |-> 0]>>)
                                                                                                                ])>>]
                                                                        /\ queues' = [queues EXCEPT ![SERVERID] = queues[SERVERID] (+) <<"$sys">>]
                                                                        /\ processesLinked' = processesLinked ++ {self}
                                                                        /\ z' = [z EXCEPT ![self] = openConnection[SERVERID]]
                                                                        /\ pc' = [pc EXCEPT ![self] = "C_WAIT_ACK_MON_genStart6_genStart6"]
                                                                   ELSE /\ TRUE
                                                                        /\ pc' = [pc EXCEPT ![self] = "W_SYNC"]
                                                                        /\ UNCHANGED << queues, 
                                                                                        processesLinked, 
                                                                                        calls, 
                                                                                        z >>
                                                             /\ UNCHANGED processesMonitored
                                       /\ rVal' = rVal
                                  ELSE /\ IF (<<SERVERID>>) = null
                                             THEN /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                        result  |-> "error",
                                                                                        reason  |-> "already_started",
                                                                                        context |-> <<"", <<>>>>
                                                                                    ]]
                                             ELSE /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                        result  |-> "error",
                                                                                        reason  |-> "already_started",
                                                                                        context |-> (<<SERVERID>>)
                                                                                    ]]
                                       /\ pc' = [pc EXCEPT ![self] = "W_SYNC"]
                                       /\ UNCHANGED << queues, processesLinked, 
                                                       processesMonitored, 
                                                       calls, z >>
                       ELSE /\ IF (<<(<< "local", "server">>), "gen_server_callbacks", (<<>>), (<<[hibernate_after |-> 0]>>)>>) = null
                                  THEN /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                             result  |-> "error",
                                                                             reason  |-> "badarg",
                                                                             context |-> <<"", <<>>>>
                                                                         ]]
                                  ELSE /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                             result  |-> "error",
                                                                             reason  |-> "badarg",
                                                                             context |-> (<<(<< "local", "server">>), "gen_server_callbacks", (<<>>), (<<[hibernate_after |-> 0]>>)>>)
                                                                         ]]
                            /\ pc' = [pc EXCEPT ![self] = "W_SYNC"]
                            /\ UNCHANGED << queues, processesLinked, 
                                            processesMonitored, calls, z >>
                 /\ UNCHANGED << null, rootModule, uuid, signals, mRefTable, results, timers, clientUp, openConnection, wasStopped, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqTerminate, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, module, monitorSuccess, mRef, mRefRes >>

C_WAIT_ACK_genStart6_genStart6(self) == /\ pc[self] = "C_WAIT_ACK_genStart6_genStart6"
                                        /\ Len(results[self]) > 0 \/ z[self] (/) openConnection[SERVERID] \/ wasStopped[SERVERID]
                                        /\ IF Len(results[self]) > 0
                                              THEN /\ rVal' = [rVal EXCEPT ![self] = Head(results[self])]
                                                   /\ results' = [results EXCEPT ![self] = Tail(results[self])]
                                                   /\ Assert(rVal'[self].result = "ack", 
                                                             "Failure of assertion at line 275, column 9 of macro called at line 962, column 25.")
                                                   /\ calls' = calls
                                              ELSE /\ IF z[self] (/) openConnection[SERVERID]
                                                         THEN /\ IF (<<SERVERID>>) = null
                                                                    THEN /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                                               result  |-> "error",
                                                                                                               reason  |-> "already_started",
                                                                                                               context |-> <<"", <<>>>>
                                                                                                           ]]
                                                                    ELSE /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                                               result  |-> "error",
                                                                                                               reason  |-> "already_started",
                                                                                                               context |-> (<<SERVERID>>)
                                                                                                           ]]
                                                              /\ calls' = calls
                                                         ELSE /\ IF (<<SERVERID>>) = null
                                                                    THEN /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                                               result  |-> "error",
                                                                                                               reason  |-> "server_down",
                                                                                                               context |-> <<"", <<>>>>
                                                                                                           ]]
                                                                    ELSE /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                                               result  |-> "error",
                                                                                                               reason  |-> "server_down",
                                                                                                               context |-> (<<SERVERID>>)
                                                                                                           ]]
                                                              /\ calls' = [calls EXCEPT ![SERVERID] = <<>>]
                                                   /\ UNCHANGED results
                                        /\ pc' = [pc EXCEPT ![self] = "W_SYNC"]
                                        /\ UNCHANGED << null, rootModule, uuid, queues, signals, mRefTable, processesLinked, processesMonitored, timers, clientUp, openConnection, wasStopped, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

C_WAIT_ACK_LINK_genStart6_genStart6(self) == /\ pc[self] = "C_WAIT_ACK_LINK_genStart6_genStart6"
                                             /\ Len(results[self]) > 0 \/ z[self] (/) openConnection[SERVERID] \/ wasStopped[SERVERID]
                                             /\ IF Len(results[self]) > 0
                                                   THEN /\ rVal' = [rVal EXCEPT ![self] = Head(results[self])]
                                                        /\ results' = [results EXCEPT ![self] = Tail(results[self])]
                                                        /\ Assert(rVal'[self].result = "ack", 
                                                                  "Failure of assertion at line 275, column 9 of macro called at line 962, column 25.")
                                                        /\ calls' = calls
                                                   ELSE /\ IF z[self] (/) openConnection[SERVERID]
                                                              THEN /\ IF (<<SERVERID>>) = null
                                                                         THEN /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                                                    result  |-> "error",
                                                                                                                    reason  |-> "already_started",
                                                                                                                    context |-> <<"", <<>>>>
                                                                                                                ]]
                                                                         ELSE /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                                                    result  |-> "error",
                                                                                                                    reason  |-> "already_started",
                                                                                                                    context |-> (<<SERVERID>>)
                                                                                                                ]]
                                                                   /\ calls' = calls
                                                              ELSE /\ IF (<<SERVERID>>) = null
                                                                         THEN /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                                                    result  |-> "error",
                                                                                                                    reason  |-> "server_down",
                                                                                                                    context |-> <<"", <<>>>>
                                                                                                                ]]
                                                                         ELSE /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                                                    result  |-> "error",
                                                                                                                    reason  |-> "server_down",
                                                                                                                    context |-> (<<SERVERID>>)
                                                                                                                ]]
                                                                   /\ calls' = [calls EXCEPT ![SERVERID] = <<>>]
                                                        /\ UNCHANGED results
                                             /\ pc' = [pc EXCEPT ![self] = "W_SYNC"]
                                             /\ UNCHANGED << null, rootModule, uuid, queues, signals, mRefTable, processesLinked, processesMonitored, timers, clientUp, openConnection, wasStopped, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

C_WAIT_ACK_MON_genStart6_genStart6(self) == /\ pc[self] = "C_WAIT_ACK_MON_genStart6_genStart6"
                                            /\ Len(results[self]) > 0 \/ z[self] (/) openConnection[SERVERID] \/ wasStopped[SERVERID]
                                            /\ IF Len(results[self]) > 0
                                                  THEN /\ rVal' = [rVal EXCEPT ![self] = Head(results[self])]
                                                       /\ results' = [results EXCEPT ![self] = Tail(results[self])]
                                                       /\ Assert(rVal'[self].result = "ack", 
                                                                 "Failure of assertion at line 275, column 9 of macro called at line 962, column 25.")
                                                       /\ calls' = calls
                                                  ELSE /\ IF z[self] (/) openConnection[SERVERID]
                                                             THEN /\ IF (<<SERVERID>>) = null
                                                                        THEN /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                                                   result  |-> "error",
                                                                                                                   reason  |-> "already_started",
                                                                                                                   context |-> <<"", <<>>>>
                                                                                                               ]]
                                                                        ELSE /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                                                   result  |-> "error",
                                                                                                                   reason  |-> "already_started",
                                                                                                                   context |-> (<<SERVERID>>)
                                                                                                               ]]
                                                                  /\ calls' = calls
                                                             ELSE /\ IF (<<SERVERID>>) = null
                                                                        THEN /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                                                   result  |-> "error",
                                                                                                                   reason  |-> "server_down",
                                                                                                                   context |-> <<"", <<>>>>
                                                                                                               ]]
                                                                        ELSE /\ rVal' = [rVal EXCEPT ![self] =               [
                                                                                                                   result  |-> "error",
                                                                                                                   reason  |-> "server_down",
                                                                                                                   context |-> (<<SERVERID>>)
                                                                                                               ]]
                                                                  /\ calls' = [calls EXCEPT ![SERVERID] = <<>>]
                                                       /\ UNCHANGED results
                                            /\ pc' = [pc EXCEPT ![self] = "W_SYNC"]
                                            /\ UNCHANGED << null, rootModule, uuid, queues, signals, mRefTable, processesLinked, processesMonitored, timers, clientUp, openConnection, wasStopped, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

W_SYNC(self) == /\ pc[self] = "W_SYNC"
                /\ IF syncStart
                      THEN /\ canStart' = TRUE
                      ELSE /\ TRUE
                           /\ UNCHANGED canStart
                /\ pc' = [pc EXCEPT ![self] = "W_STOP"]
                /\ UNCHANGED << null, rootModule, uuid, queues, signals, mRefTable, processesLinked, processesMonitored, calls, results, timers, clientUp, openConnection, wasStopped, rVal, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

W_STOP(self) == /\ pc[self] = "W_STOP"
                /\ Assert(~rVal[self](.)<<"result", "error">> \/ ~rVal[self](.)<<"reason", "bad_arg">>, 
                          "Failure of assertion at line 966, column 25.")
                /\ \A c \in WorkerClients: ~clientUp[c]
                /\ rVal' = [rVal EXCEPT ![self] = null]
                /\ signals' = [signals EXCEPT ![SERVERID] = @ ++ ({<<"MONITOR", uuid, self>>})]
                /\ mRef' = [mRef EXCEPT ![self] = uuid]
                /\ mRefTable' = [mRefTable EXCEPT ![mRef'[self]] = SERVERID]
                /\ uuid' = uuid + 1
                /\ pc' = [pc EXCEPT ![self] = "MONITOR_WAIT_FOR_ACK_stop3"]
                /\ UNCHANGED << null, rootModule, queues, processesLinked, processesMonitored, calls, results, timers, clientUp, openConnection, wasStopped, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRefRes >>

MONITOR_WAIT_FOR_ACK_stop3(self) == /\ pc[self] = "MONITOR_WAIT_FOR_ACK_stop3"
                                    /\ ~openConnection[SERVERID] \/ \E s \in signals[self]: s[1] = "ACK" /\ s[2] = mRef[self]
                                    /\ IF ~openConnection[SERVERID] /\ ~(\E s \in signals[self]: s[1] = "ACK" /\ s[2] = mRef[self])
                                          THEN /\ queues' = [queues EXCEPT ![self] = @ (+) <<"DOWN", mRef[self], SERVERID, "no_proc">>]
                                               /\ signals' = [signals EXCEPT ![SERVERID] = signals[SERVERID] \ {<<"MONITOR", mRef[self], self>>}]
                                               /\ UNCHANGED monitorSuccess
                                          ELSE /\ signals' = [signals EXCEPT ![self] = signals[self] \ {<<"ACK", mRef[self]>>}]
                                               /\ monitorSuccess' = [monitorSuccess EXCEPT ![self] = TRUE]
                                               /\ UNCHANGED queues
                                    /\ mRefRes' = [mRefRes EXCEPT ![self] = mRef[self]]
                                    /\ pc' = [pc EXCEPT ![self] = "STOP_SEND_stop3"]
                                    /\ UNCHANGED << null, rootModule, uuid, mRefTable, processesLinked, processesMonitored, calls, results, timers, clientUp, openConnection, wasStopped, rVal, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, mRef >>

STOP_SEND_stop3(self) == /\ pc[self] = "STOP_SEND_stop3"
                         /\ IF monitorSuccess[self] /\ ~receivedDown(self, mRefRes[self])
                               THEN /\ queues' = [queues EXCEPT ![SERVERID] = @ (+) (<<"system", <<self, mRefRes[self]>>, <<"terminate", "normal">>>>)]
                                    /\ reqTerminate' = [reqTerminate EXCEPT ![self] = TRUE]
                                    /\ IF isInt("infinity") /\ "infinity" > 0
                                          THEN /\ timers' = [timers EXCEPT ![self] = [timers[self] EXCEPT !.running = TRUE]]
                                          ELSE /\ TRUE
                                               /\ UNCHANGED timers
                                    /\ pc' = [pc EXCEPT ![self] = "STOP_RECEIVE_stop3"]
                                    /\ rVal' = rVal
                               ELSE /\ rVal' = [rVal EXCEPT ![self] = [result |-> "error", reason |-> "DOWN", requestId |-> mRefRes[self], status |-> "noproc"]]
                                    /\ pc' = [pc EXCEPT ![self] = "STOP_DEMON_stop3"]
                                    /\ UNCHANGED << queues, timers, 
                                                    reqTerminate >>
                         /\ UNCHANGED << null, rootModule, uuid, signals, mRefTable, processesLinked, processesMonitored, calls, results, clientUp, openConnection, wasStopped, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

STOP_RECEIVE_stop3(self) == /\ pc[self] = "STOP_RECEIVE_stop3"
                            /\ Len(queues[self]) # 0 \/ (~(isString("infinity")) /\ timers[self].isReached)
                            /\ IF choosePossible(toSetOfRecs(queues[self]), {mRefRes[self]})
                                  THEN /\ rVal' = [rVal EXCEPT ![self] = receiveSelective(self, LAMBDA q: hasKey("requestId", q) /\ q.requestId = mRefRes[self])]
                                       /\ queues' = [queues EXCEPT ![self] = deleteFromTuple(queues[self], LAMBDA head: isRecord(head) /\ (rVal'[self]) = head, <<>>)]
                                  ELSE /\ IF receivedDown(self, mRefRes[self])
                                             THEN /\ rVal' = [rVal EXCEPT ![self] = [result |-> "error", reason |-> "DOWN", requestId |-> mRefRes[self]]]
                                             ELSE /\ IF timers[self].isReached
                                                        THEN /\ rVal' = [rVal EXCEPT ![self] =        [
                                                                                                   result    |-> "timeout",
                                                                                                   requestId |-> mRefRes[self]
                                                                                               ]]
                                                        ELSE /\ rVal' = [rVal EXCEPT ![self] = [result |-> "", other |-> mRefRes[self]]]
                                       /\ UNCHANGED queues
                            /\ IF ~reqIdMatch(rVal'[self], mRefRes[self]) /\ ~(rVal'[self].result = "timeout" \/ rVal'[self].result = "error")
                                  THEN /\ pc' = [pc EXCEPT ![self] = "STOP_RECEIVE_stop3"]
                                       /\ UNCHANGED signals
                                  ELSE /\ signals' = [signals EXCEPT ![(unalias(mRefRes[self]))] = @ ++ ({<<"DEMONITOR", mRefRes[self], self>>})]
                                       /\ pc' = [pc EXCEPT ![self] = "STOP_DEMON_stop3"]
                            /\ UNCHANGED << null, rootModule, uuid, mRefTable, processesLinked, processesMonitored, calls, results, timers, clientUp, openConnection, wasStopped, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

STOP_DEMON_stop3(self) == /\ pc[self] = "STOP_DEMON_stop3"
                          /\ receivedDown(self, mRefRes[self]) \/ \E s \in signals[self]: s[1] = "ACK" /\ s[2] = mRefRes[self]
                          /\ IF ~openConnection[unalias(mRefRes[self])]
                                THEN /\ queues' = [queues EXCEPT ![self] =             deleteFromTuple(queues[self], LAMBDA head:
                                                                           (isSeq(head) /\ head[1] = "DOWN" /\ head[2] = mRefRes[self])
                                                                               \/ (isRecord(head) /\ head(.)<<"requestId", mRefRes[self]>>), <<>>)]
                                     /\ UNCHANGED signals
                                ELSE /\ signals' = [signals EXCEPT ![self] = signals[self] \ {<<"ACK", mRefRes[self]>>}]
                                     /\ queues' = [queues EXCEPT ![self] =             deleteFromTuple(queues[self], LAMBDA head:
                                                                           (isRecord(head) /\ head(.)<<"requestId", mRefRes[self]>>), <<>>)]
                          /\ monitorSuccess' = [monitorSuccess EXCEPT ![self] = FALSE]
                          /\ clientUp' = [clientUp EXCEPT ![self] = FALSE]
                          /\ pc' = [pc EXCEPT ![self] = "Done"]
                          /\ UNCHANGED << null, rootModule, uuid, mRefTable, processesLinked, processesMonitored, calls, results, timers, openConnection, wasStopped, rVal, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, mRef, mRefRes >>

controlClient(self) == W_START(self)
                          \/ C_WAIT_ACK_genStart6_genStart6(self)
                          \/ C_WAIT_ACK_LINK_genStart6_genStart6(self)
                          \/ C_WAIT_ACK_MON_genStart6_genStart6(self)
                          \/ W_SYNC(self) \/ W_STOP(self)
                          \/ MONITOR_WAIT_FOR_ACK_stop3(self)
                          \/ STOP_SEND_stop3(self)
                          \/ STOP_RECEIVE_stop3(self)
                          \/ STOP_DEMON_stop3(self)

TIMER_TICK(self) == /\ pc[self] = "TIMER_TICK"
                    /\ IF (\E c \in AllClients: clientUp[c])
                          THEN /\ (\A c \in AllClients: ~(clientUp[c])) \/ anyRunning(timers)
                               /\ timers' = tickTimers(timers)
                               /\ pc' = [pc EXCEPT ![self] = "TIMER_TICK"]
                          ELSE /\ pc' = [pc EXCEPT ![self] = "Done"]
                               /\ UNCHANGED timers
                    /\ UNCHANGED << null, rootModule, uuid, queues, signals, mRefTable, processesLinked, processesMonitored, calls, results, clientUp, openConnection, wasStopped, rVal, rValTemp, rValTemp1, rValTemp2, rValTemp3, trace, callTrace, replyTrace, globalClock, reqStart, reqTerminate, canStart, smsg, raw_msg, serverUp, serverState, timeout_, hibernate, continue, hibernateAfterTimeout, skipLoop, skipLoopHibernate, skipWait, parent, name, initResult, module_, mRef_, mRefRes_, mRefs, z_, iterations, z_w, module_w, monitorSuccess_, mRef_w, mRefRes_w, reqId, previousReqId, timeout, callMsg, z, module, monitorSuccess, mRef, mRefRes >>

timer(self) == TIMER_TICK(self)

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == (\E self \in ServerSet: genServer(self))
           \/ (\E self \in WorkerClients: workerClient(self))
           \/ (\E self \in ControlClients: controlClient(self))
           \/ (\E self \in TimerSet: timer(self))
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in ServerSet : WF_vars(genServer(self))
        /\ \A self \in WorkerClients : WF_vars(workerClient(self))
        /\ \A self \in ControlClients : WF_vars(controlClient(self))
        /\ \A self \in TimerSet : WF_vars(timer(self))

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION
(***************************************************************************)
(* PROPERTIES                                                              *)
(***************************************************************************)

(***************************************************************************)
(* Correctness                                                             *)
(***************************************************************************)
Mrefs == DOMAIN mRefTable

NotTermination == ~(<>(\A self \in ProcSet: pc[self] = "Done"))

NeverPendingRequests == [][~(Len(queues[SERVERID]) = 6 /\ Len(queues'[SERVERID]) = 5)]_queues

P0_MailboxTypeInvariant ==
    /\ \A p \in DOMAIN queues: isSeq(queues[p]) 
    /\ \A msgs \in DOMAIN queues[SERVERID]: isSeq(queues[SERVERID][msgs])
    
P1_Assumption(c)                == serverUp[SERVERID] /\ isSeq(rValTemp1[c]) /\ pc[c] = "C_LOOP"
P1_OrderReflected(order, queue) == \E indexSet \in SUBSET DOMAIN queue: 
    /\ Cardinality(indexSet) = Len(order) 
    /\ isOrdered(toTuple(indexSet))
    /\ mapTuple(LAMBDA i: queue[i], toTuple(indexSet)) = order 
P1_RelativeOrderPreservation    == \A c \in WorkerClients: 
    P1_Assumption(c) ~> (P1_OrderReflected(trace[c] (+) rValTemp1[c], queues[SERVERID]))

P2_Assumption(c)                               == reqStart[c]
P2_PerformedCallInfinitly(c)                   == 
    /\ isSeq(rValTemp1[c]) /\ rValTemp1[c][1] = "$gen_call" /\ ~timeout[c]
P2_ExpectedResultEitherDownOrReply(c)          == 
    /\ isRecord(rVal[c]) /\ isSeq(rValTemp1[c]) /\ Len(rValTemp1[c]) >= 4 
    /\ rVal[c](.)<<"requestId", rValTemp1[c][4]>>
    /\ ((isRecord(rVal[c]) /\ rVal[c](.)<<"result", "#CALL INF">>)   
        \/ (isRecord(rVal[c]) /\ rVal[c](.)<<"reason", "DOWN">>))        (* Path for one state not present *)
P2_PerformedCallTimeout(c)                     == 
    /\ isSeq(rValTemp1[c]) /\ rValTemp1[c][1] = "$gen_call" /\ timeout[c]
P2_ExpectedResultEitherDownOrReplyOrTimeout(c) ==
    /\ isRecord(rVal[c]) /\ isSeq(rValTemp1[c]) /\ Len(rValTemp1[c]) >= 4 
    /\ rVal[c](.)<<"requestId", rValTemp1[c][4]>>
    /\ ((rVal[c](.)<<"result", "#CALL TMO">>) 
        \/ (rVal[c](.)<<"reason", "DOWN">>)
        \/ (rVal[c](.)<<"result", "timeout">>))
P2_ReliableCall                                == \A c \in WorkerClients:
    /\ ((P2_Assumption(c) /\ P2_PerformedCallTimeout(c)) 
        ~> P2_ExpectedResultEitherDownOrReplyOrTimeout(c))
    /\ ((P2_Assumption(c) /\ P2_PerformedCallInfinitly(c)) 
        ~> P2_ExpectedResultEitherDownOrReply(c))
    
P3_Assumption         == serverUp[SERVERID]
P3_StateGrowOnly      == (Len(serverState'[SERVERID]) >= Len(serverState[SERVERID]))
P3_PersistentRequests == [][P3_Assumption => P3_StateGrowOnly]_serverState
    
P4_MailboxAtMostExceptDownSignals(c) == Len(filterTuple(LAMBDA head: ~(isSeq(head) /\ head[1] = "DOWN"), queues[c])) <= 1
P4_MailboxIntegrity                  == [](\A c \in WorkerClients: P4_MailboxAtMostExceptDownSignals(c))

S1_Assumption(c)    == reqStart[c]
S1_PerformedCall(c) ==
    /\ isSeq(rValTemp1[c]) 
    /\ Len(rValTemp1[c]) >= 1
    /\ rValTemp1[c][1] = "$gen_call"
    /\ pc[c] = "C_LOOP"
S1_RequestFailed(c) == isRecord(rVal[c]) /\ rVal[c](.)<<"result", "error">>
S1_StrictLifecycle == \A c \in WorkerClients:
    /\ ((S1_PerformedCall(c) /\ ~serverUp[SERVERID])
        ~> S1_RequestFailed(c))
        
S2_MailboxStepBound(s) == Len(queues'[s]) \in {Len(queues[s]) - 1, Len(queues[s]) + 1, Len(queues[s])}
S2_MessageChanges(s) == pc[s] = "S_PROCESS" => smsg[s] # smsg'[s]
S2_ProcessingExactlyOnce == [][\A s \in ServerSet: S2_MessageChanges(s)]_smsg
    /\ [][\A s \in ServerSet: S2_MailboxStepBound(s)]_queues
    
S3_MessageConstraint(m) == isSeq(m) /\ Len(m) = 3 /\ isString(m[1]) /\ m[1] = "system" /\ m[3][1] = "terminate"
S3_Termination == \A s \in ServerSet: []((S3_MessageConstraint(smsg[s]) /\ serverUp[s]) 
    ~> ~serverUp[s])
        
S4_MutualExclusion == [](isOrdered(replyTrace))

SE1_EventualProgress == [][\E s \in ServerSet: pc'[s] # pc[s] 
    \/ \E c \in (NodeSet \ ServerSet): pc'[c] # pc[c]
    \/ \A n \in NodeSet: pc[n] = "Done"]_pc
    
SE2_NoStaleMessages == \A s \in ServerSet: Len(queues[s]) >= 1 
    ~> (Len(queues[s]) = 0 \/ \E c \in AllClients: reqTerminate[c])
    
SE3_NoDuplicateDelivery == [][\A n \in NodeSet: 
    (queues'[n] # queues[n] => (Len(queues'[n]) = Len(queues[n]) + 1))
    \/ ((n \in ServerSet /\ Len(queues'[n]) = Len(queues[n]) - 1) => raw_msg[n] = Head(queues[n]))
    \/ ((n \in WorkerClients /\ Len(queues'[n]) = Len(queues[n]) - 1) => rVal[n] = Head(queues[n]))]_queues

SE4_CallWithResult(t) == isRecord(t) /\ hasKey("requestId", t) /\ (t(.)<<"result", "#CALL TMO">> \/ t(.)<<"result", "#CALL INF">>)
SE4_CallsSetReductionUnique(c) == LET filterCalls == filterTuple(LAMBDA t: SE4_CallWithResult(t), callTrace[c]) 
    IN \A call \in toSetOfRecs(filterCalls): \E i \in DOMAIN filterCalls: filterCalls[i] = call /\ ~(\E j \in DOMAIN filterCalls: i # j /\ filterCalls[j] = call)
SE4_MonitorUniqueness == [](\A c \in WorkerClients: SE4_CallsSetReductionUnique(c))

\* Invariants
====
