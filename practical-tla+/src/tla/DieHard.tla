------------------------------ MODULE DieHard ------------------------------

EXTENDS Integers

VARIABLES small, big 

TypeCheck == small \in 0..3 /\ big \in 0..5

EmptySmall == small' = 0 /\ big' = big
EmptyBig   == big' = 0   /\ small' = small
FillSmall  == small' = 3 /\ big' = big
FillBig    == big' = 5   /\ small' = small

SmallToBig == IF big + small =< 5 
                THEN /\ big' = big + small
                     /\ small' = 0
                ELSE /\ big' = 5
                     /\ small' = small - (5 - big)
BigToSmall == IF big + small =< 3
                THEN /\ small' = small + big
                     /\ big' = 0
                ELSE /\ small' = 3
                     /\ big' = small - (3 - big)

Init == big = 0 /\ small = 0                 
                 
Next == \/ FillSmall
        \/ FillBig
        \/ EmptySmall
        \/ EmptyBig
        \/ SmallToBig
        \/ BigToSmall

=============================================================================
