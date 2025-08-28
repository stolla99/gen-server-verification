------------------------------- MODULE test_bug -------------------------------

EXTENDS Naturals, Sequences, TLC, FiniteSets
    
THEOREM OnePlusOne == 1 + 1 = 2
PROOF OBVIOUS

THEOREM Transitive == 
    ASSUME 
        NEW X \in Nat,
        NEW Y \in Nat,
        NEW Z \in Nat,
        X > Y,
        Y > Z
        PROVE X > Z + 1
PROOF OBVIOUS

HasSqrt(Y) == \E k \in 1..Y : k * k = Y
THEOREM TheseHaveSqrt ==
    ASSUME
        NEW X \in {4, 9}
    PROVE HasSqrt(X)
PROOF 
    <1>1 HasSqrt(4) BY DEF HasSqrt
    <1>2 QED BY <1>1

\* Invariants
====