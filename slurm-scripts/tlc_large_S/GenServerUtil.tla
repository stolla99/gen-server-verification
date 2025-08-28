------------------------------- MODULE GenServerUtil -------------------------------
LOCAL INSTANCE Integers
LOCAL INSTANCE Json
LOCAL INSTANCE TLC
LOCAL INSTANCE Sequences
LOCAL INSTANCE FiniteSets
LOCAL INSTANCE Typecheck

(***************************************************************************)
(* TLA+ helper functions                                                   *)
(***************************************************************************)
    
RECURSIVE deleteFromTuple(_, _, _)    
RECURSIVE deleteOnceFromTuple(_, _, _)  
RECURSIVE filterTuple(_, _)
RECURSIVE mapTuple(_, _)    
RECURSIVE toTuple(_)
RECURSIVE isOrdered(_)
RECURSIVE isPrefix(_, _)

-----------------------------------------------------------------------------
isOrdered(T) == Len(T) <= 1 \/ (Head(T) < Head(Tail(T)) /\ isOrdered(Tail(T)))
-----------------------------------------------------------------------------
isPrefix(Str, Target) == Len(Str) <= Len(Target) 
    /\ (Len(Str) = 0 
        \/ (Head(Str) = Head(Target) /\ isPrefix(Tail(Str), Tail(Target))))
-----------------------------------------------------------------------------
toTuple(S) ==
  IF S = {} THEN
    <<>>
  ELSE
    LET elem == CHOOSE e \in S : TRUE IN
      <<elem>> \o toTuple(S \ {elem})
-----------------------------------------------------------------------------

set ++ ee ==
    set \union ee
-----------------------------------------------------------------------------

seq (+) elem ==
    Append(seq, elem)
-----------------------------------------------------------------------------

A (/) B == (A /\ ~B) \/ (~A /\ B)
-----------------------------------------------------------------------------

hasKey(key, structure) == isRecord(structure) /\ key \in (DOMAIN structure)
-----------------------------------------------------------------------------    

toSetOfStrings(tuple) == UNION {x : x \in {IF isString(tuple[i]) THEN {tuple[i]} ELSE {}: i \in DOMAIN tuple}}       
toSetOfSeqs(tuple) == UNION {x : x \in {IF isSeq(tuple[i]) THEN {tuple[i]} ELSE {}: i \in DOMAIN tuple}} 
toSetOfRecs(tuple) == {IF isRecord(tuple[i]) THEN tuple[i] ELSE [label |-> ""]: i \in DOMAIN tuple}
-----------------------------------------------------------------------------

\* Map seq accord. Op into out *\
mapTuple(Op(_), seq) ==
    IF Len(seq) = 0 THEN 
        <<>>
    ELSE
        <<Op(Head(seq))>> \o mapTuple(Op, Tail(seq))
-----------------------------------------------------------------------------

\* Filter accord. Op from seq into out *\
filterTuple(Op(_), seq) ==
    IF Len(seq) = 0 THEN 
        <<>>
    ELSE IF Op(Head(seq)) THEN
            <<Head(seq)>> \o filterTuple(Op, Tail(seq))
        ELSE
            filterTuple(Op, Tail(seq))
-----------------------------------------------------------------------------

\* Takes seq[m:n] if empty then <<>> *\
subSeq(seq, m, n) ==
    IF Len(seq) = 0 THEN <<>>
    ELSE SubSeq(seq, m, n)
-----------------------------------------------------------------------------

\* e \in queue: e.requestId \in reqIds *\
choosePossible(queue, reqIds) ==
    LET temp == {q \in queue: hasKey("requestId", q)}
    IN  Cardinality({q \in temp: q.requestId \in reqIds}) # 0
-----------------------------------------------------------------------------

\* reply.requestId = id *\     
reqIdMatch(reply, id) == hasKey("requestId", reply) /\ reply.requestId = id
-----------------------------------------------------------------------------

\* deleteOnceFromTuple(<<1,2,3,3>>, 3, <<>>) == <<1,2,3>> *\
deleteOnceFromTuple(tpl, elem, res) ==
    CASE Len(tpl) = 0 -> res
      [] OTHER        -> IF isRecord(Head(tpl)) /\ isRecord(elem) /\ Head(tpl) = elem THEN
            res \o Tail(tpl)
        ELSE 
            deleteOnceFromTuple(Tail(tpl), elem, res (+) Head(tpl))    

-----------------------------------------------------------------------------
\* deleteFromTuple(<<1,2,3,3>>, 3, <<>>) == <<1,2>> *\
deleteFromTuple(tpl, matcher(_), res) ==
    CASE Len(tpl) = 0 -> res
      [] OTHER        -> IF matcher(Head(tpl)) THEN
            deleteFromTuple(Tail(tpl), matcher, res)
        ELSE 
            deleteFromTuple(Tail(tpl), matcher, res (+) Head(tpl))
-----------------------------------------------------------------------------
\* Perform hasKey(key, struct, value) /\ struct.key = val*\
struct(.)key == Len(key) = 2 /\ hasKey(key[1], struct) /\ struct[key[1]] = key[2]
-----------------------------------------------------------------------------
\* Test if this is a string which an atom in erlang *\
isAtom(e) == isString(e)
isTuple(e) == isSeq(e)
isTimeout(e) == isString(e) /\ e = "1"
isExported(m, f, ar) == TRUE
-----------------------------------------------------------------------------

(***************************************************************************)
(* Timer helpers                                                           *)
(***************************************************************************)  
         
-----------------------------------------------------------------------------
tickTimers(timers) == 
    [timerId \in DOMAIN timers |-> 
        LET randId == CHOOSE any \in DOMAIN timers: timers[any].running IN 
        IF randId = timerId THEN
            timers[timerId]
        ELSE
        [
            isReached |-> timers[timerId].running,
            running   |-> timers[timerId].running            
        ]]
-----------------------------------------------------------------------------
anyRunning(timers) == 
    Cardinality({timer \in {timers[e]: e \in DOMAIN timers}: timer.running}) >= 1
-----------------------------------------------------------------------------

=============================================================================