---- MODULE SymmConfig ----
EXTENDS TLC

CONSTANTS
A_2, A_3, A_4
----

symmClients == {A_2, A_3, A_4}
WorkerClientsSymmetric == Permutations(symmClients)
=============================================================================

