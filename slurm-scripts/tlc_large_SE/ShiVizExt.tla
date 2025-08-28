------------------------------- MODULE ShiVizExt -------------------------------
LOCAL INSTANCE GenServerUtil
LOCAL INSTANCE Integers
LOCAL INSTANCE Json
LOCAL INSTANCE TLC
LOCAL INSTANCE Sequences
LOCAL INSTANCE FiniteSets
LOCAL INSTANCE Typecheck

INSTANCE Toolbox

CONSTANTS A_0

-----------------------------------------------------------------------------

\* Host below is different to the action predicate 
\*   CHOOSE p \in DOMAIN pc : pc[p] # pc'[p]
\* since Host evaluates for states n and n - 1
\* whereas the CHOOSE evaluates for states n and n + 1.
\* This difference is most easily observed by looking
\* at off-by-one difference of the initial state.
LOCAL FnHost ==
    LET host[i \in DOMAIN _TETrace] ==
        (*************************************************************************)
        (* The pc value that has been modified in (the current) state n compared *)
        (* to the predecessor state n-1.                                         *)
        (*************************************************************************)
        IF i = 1 THEN A_0
        ELSE IF _TETrace[i-1].pc = _TETrace[i].pc
                THEN host[i-1] \* pc variable has not changed.
                ELSE CHOOSE p \in DOMAIN _TETrace[i-1].pc :
                            _TETrace[i-1].pc[p] # _TETrace[i].pc[p]
    IN TLCEval(host)

Host == FnHost[_TEPosition]
    
-----------------------------------------------------------------------------

LOCAL clock(n) == 
   IF n = 1 THEN [p \in DOMAIN _TETrace[n].pc |-> 0] \* In the init state, all clocks are 0.
   ELSE [p \in DOMAIN _TETrace[n].pc |-> IF p = FnHost[n] 
                                    THEN 1
                                    ELSE 0]

LOCAL sum(F, G) == 
   [d \in DOMAIN F |-> F[d] + G[d]]
   
LOCAL choice(l, i, host) == CHOOSE p \in DOMAIN _TETrace: 
    /\ l < i
    /\ p > l 
    /\ p < i 
    /\ FnHost[p] = host 
    /\ Cardinality({o \in DOMAIN _TETrace: o > 1 /\ o < i /\ FnHost[o] = host /\ o > p}) = 0
    
RECURSIVE queueMatcher(_)
LOCAL queueMatcher(queue) == 
    IF Len(queue) = 0 
    THEN <<>>
    ELSE
        LET head == Head(queue) 
        IN 
            IF isString(head[1]) /\ head[1] \in {"$sys", "DOWN"} 
            THEN queueMatcher(Tail(queue))
            ELSE <<head>> \o queueMatcher(Tail(queue)) 
            
RECURSIVE historyBacktrack(_,_,_,_,_)
LOCAL historyBacktrack(l, u, currHost, vclock, hosts) ==
    IF Len(hosts) = 0 THEN
        clock(1)
    ELSE
        LET nextHost == Head(hosts)
        IN LET aPrevious == \E p \in DOMAIN _TETrace: p > l /\ p < u /\ FnHost[p] = nextHost
        IN IF aPrevious THEN
                LET max_i == choice(l, u, nextHost) 
                IN IF Len(queueMatcher(_TETrace[max_i].queues[currHost])) 
                    > Len(queueMatcher(_TETrace[l].queues[currHost])) THEN
                    sum(TLCEval(vclock[max_i]),
                        historyBacktrack(l, u, currHost, vclock, Tail(hosts)))
                ELSE
                    historyBacktrack(l, u, currHost, vclock, Tail(hosts)) 
            ELSE
                clock(1)

LOCAL FnClock == 
    LET vclock[i \in DOMAIN _TETrace] == 
        IF i = 1
        THEN clock(i)
        ELSE 
            LET host == FnHost[i] 
            IN
                LET aPrevious == \E p \in DOMAIN _TETrace: p > 1 /\ p < i /\ FnHost[p] = host
                IN 
                    IF aPrevious /\ Len(queueMatcher(_TETrace[i].queues[host])) > Len(queueMatcher(_TETrace[choice(1, i, host)].queues[host])) 
                    THEN 
                        LET max_i == choice(1, i, host) IN
                        LET aggregatedHistory == historyBacktrack(max_i, i, host, vclock, toTuple({FnHost[ii]: ii \in max_i+1..i-1})) IN 
                            sum(aggregatedHistory, sum(TLCEval(vclock[choice(1, i, host)]), clock(i)))  
                    ELSE 
                        IF aPrevious
                        THEN sum(TLCEval(vclock[choice(1, i, host)]), clock(i))
                        ELSE clock(i)
    IN TLCEval(vclock)

Clock ==
   (*************************************************************************)
   (* The pc variable formatted as a Json object as required by ShiViz.     *)
   (*************************************************************************)
   ToJsonObject(FnClock[_TEPosition])

=============================================================================
\* Modification History
\* Last modified Sun Jul 06 01:19:34 CEST 2025 by arnes
\* Last modified Tue Sep 25 17:20:37 PDT 2019 by markus
\* Created Tue Aug 27 13:04:16 PDT 2019 by markus
