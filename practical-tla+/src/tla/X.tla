------------------------------ MODULE Untitled ----------------------------

EXTENDS Integers

VARIABLES services, serviceRequests, clock

CONSTANTS maxResponseTime

Init == 
  /\ services = {"S1"}
  /\ clock = 0
  /\ serviceRequests = [ s \in services |-> [active |-> FALSE, responseTime |-> 0] ]

SVCMakeRequest(s) == 
  LET mrt == clock + 10 IN
  /\ UNCHANGED services
  /\ UNCHANGED clock
  /\ serviceRequests[s].active = FALSE
  /\ serviceRequests' = [serviceRequests EXCEPT ![s] = [active |-> TRUE, responseTime |-> mrt]]

SVCReceiveResponse(s, c) == 
  /\ UNCHANGED services
  /\ UNCHANGED clock
  /\ serviceRequests[s].active = TRUE
  /\ serviceRequests[s].responseTime >= c
  /\ serviceRequests' = [serviceRequests EXCEPT ![s] = [active |-> FALSE, responseTime |-> 0]]

NextTick ==
  /\ UNCHANGED services
  /\ UNCHANGED serviceRequests
  /\ clock' = clock + 1

Next == 
  \/ SVCMakeRequest("S1") \/ NextTick
  \/ SVCReceiveResponse("S1", clock) \/ NextTick

=============================================================================
