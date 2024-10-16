// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBdispatcher
/*
  This component includes two gates (stops), each activated by a cylinder. The
  dispatcher considers three positions numbered 1, 2 and 3. The closest position
  is numbered 1. The next position is numbered 2. The farthest position is
  numbered 3. The dispatcher is controlled to select one of these three positions.
*/

SETS
/*
  seq states:
    g1og2o := both gates are opened
    g1cg2o := the first gate is closed and the second gate are open
    g2cg1o := the second gate is closed and the first gate are open
    g1cg2c := the first gate has been closed before the second gate
    g2cg1c := the second gate has been closed before the first gate
  first gate states:
     G1e := first gate completely extended
    G1ie := first gate in extension
    G1ir := first gate in retraction
     G1r := first gate completely retracted
  second gate states:
     G2e := second gate completely extended
    G2ie := second gate in extension
    G2ir := second gate in retraction
     G2r := second gate completely retracted

  dispatcher states (interface):
      Dp1 := first position selected
      Dp2 := second position selected
      Dp3 := third position selected (default)
    Dp3p1 := change from third position selection to first position selection
    Dp3p2 := change from third position selection to second position selection
    Dpxp3 := change from first or second position selection to third position selection
*/
 StatesG_A = {g1og2o_G1r_G2r,
              g1og2o_G1ie_G2r, g1cg2o_G1e_G2r, g1cg2o_G1ir_G2r,
              g1og2o_G1r_G2ie, g2cg1o_G1r_G2e, g2cg1o_G1r_G2ir,
              g1cg2o_G1e_G2ie, g1cg2c_G1e_G2e, g1cg2c_G1ir_G2e,
              g2cg1o_G1ie_G2e, g2cg1c_G1e_G2e, g2cg1c_G1e_G2ir };
 StatesG_I = {Dp1, Dp2, Dp3, Dp3p1, Dp3p2, Dpxp3};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <seqD, gate1, gate2>

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(G1_close) = G1r & (G2e | G2r)
      ControlLaw(G1_open) = G1e & (g1cg2c | (g1cg2o & G2r))
      ControlLaw(G2_close) = G2r & (G1e | G1r)
      ControlLaw(G2_open) = G2e & (g2cg1c | (g2cg1o & G1r))

    Observer:
      Project_G1_close() := { return Func_select1(); }
      Project_G1_eoc() := {
        if ( G2r ) then return Func_p1s()
                   else return true; }
      Project_G1_eoo() := {
        if ( G2r ) then return Func_p3s()
                   else return Func_p2s(); }
      Project_G1_open() := {
        if ( G2r ) then return Func_select3)
                   else return true; }
      Project_G2_close() := { return Func_select2(); }
      Project_G2_eoc() := {
        if ( G1r ) then return Func_p2s()
                   else return true; }
      Project_G2_eoo() := {
        if ( G1r ) then return Func_p3s()
                   else return Func_p1s(); }
      Project_G2_open() := {
        if ( G1r ) then return Func_select3()
                   else return true; }
*/

/*
  Commands and signals:
    first gate events:                 dispatcher events (interface):
      G1_close := close first gate       select1 := select first position
        G1_eoc := end of closing             p1s := first position selected
        G1_eoo := end of opening         p2s/p3s := second or third position selected
       G1_open := open first gate        select3 := select third position
    second gate events:
      G2_close := close second gate      select2 := select second position
        G2_eoc := end of closing             p2s := second position selected
        G2_eoo := end of opening         p1s/p3s := first or third position selected
       G2_open := open second gate       select3 := select third position
*/
 Input = {G1_close, G1_eoc, G1_eoo, G1_open,
          G2_close, G2_eoc, G2_eoo, G2_open };
 Output = {p1s, p2s, p3s, select1, select2, select3, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 //G_FB,         Deterministic automaton exhaustively modeling the device behavior
 //              under control (output in file SBdispatcher)
 G_ts,        // G_FB under control of a state feedback function (solution of a control problem)
 bound_G_ts,  // Bound on the trace length of the controlled device behavior
 G_A,         // Mealy machine modeling the device implementation (agent)
 bound_G_A,   // Bound on the trace length of the agent
 G_I,         // (quotient deterministic) automaton modeling a device abstraction (interface)
 bound_G_I,   // Bound on the trace length of the interface

 agentStatesOnLoops,         // The set states not to be repeated in traces
 interfaceStatesOnLoops,     // The set of states not to be repeated in traces

 agentIOonLoops,             // The set of i/o not to be repeated in traces
 agentEventsOnLoops,         // The set events (inputs) not to be repeated in traces
 interfaceEventsOnLoops      // The set of events (outputs) not to be repeated in traces

PROPERTIES
 VocalOutput = Output - {silentEvent}
 &
 G_ts = rec(states: StatesG_A,
            events: Input,
            tf:{(g1og2o_G1r_G2r,G1_close,g1og2o_G1ie_G2r),
                (g1og2o_G1ie_G2r,G1_eoc,g1cg2o_G1e_G2r),
                (g1cg2o_G1e_G2r,G1_open,g1cg2o_G1ir_G2r),
                (g1cg2o_G1ir_G2r,G1_eoo,g1og2o_G1r_G2r),

                (g1og2o_G1r_G2r,G2_close,g1og2o_G1r_G2ie),
                (g1og2o_G1r_G2ie,G2_eoc,g2cg1o_G1r_G2e),
                (g2cg1o_G1r_G2e,G2_open,g2cg1o_G1r_G2ir),
                (g2cg1o_G1r_G2ir,G2_eoo,g1og2o_G1r_G2r),

                (g1cg2o_G1e_G2r,G2_close,g1cg2o_G1e_G2ie),
                (g1cg2o_G1e_G2ie,G2_eoc,g1cg2c_G1e_G2e),
                (g1cg2c_G1e_G2e,G1_open,g1cg2c_G1ir_G2e),
                (g1cg2c_G1ir_G2e,G1_eoo,g2cg1o_G1r_G2e),

                (g2cg1o_G1r_G2e,G1_close,g2cg1o_G1ie_G2e),
                (g2cg1o_G1ie_G2e,G1_eoc,g2cg1c_G1e_G2e),
                (g2cg1c_G1e_G2e,G2_open,g2cg1c_G1e_G2ir),
                (g2cg1c_G1e_G2ir,G2_eoo,g1cg2o_G1e_G2r) },
            x_0: g1og2o_G1r_G2r,
            Xm: {g1og2o_G1r_G2r,g1cg2o_G1e_G2r,g2cg1o_G1r_G2e},
            ctrlEvents: {G1_close, G1_open, G2_close, G2_open} ) &
 bound_G_ts = 8 // this bound agrees with all configuration of selected positions
                // by the interface (maximum three positions without cycles):
                // p1s-p2s-p3s or p2s-p1s-p3s,

 & 
 G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,
           tf:{(g1og2o_G1r_G2r,(G1_close,select1),g1og2o_G1ie_G2r),
               (g1og2o_G1ie_G2r,(G1_eoc,p1s),g1cg2o_G1e_G2r),
               (g1cg2o_G1e_G2r,(G1_open,select3),g1cg2o_G1ir_G2r),
               (g1cg2o_G1ir_G2r,(G1_eoo,p3s),g1og2o_G1r_G2r),

               (g1og2o_G1r_G2r,(G2_close,select2),g1og2o_G1r_G2ie),
               (g1og2o_G1r_G2ie,(G2_eoc,p2s),g2cg1o_G1r_G2e),
               (g2cg1o_G1r_G2e,(G2_open,select3),g2cg1o_G1r_G2ir),
               (g2cg1o_G1r_G2ir,(G2_eoo,p3s),g1og2o_G1r_G2r),

               (g1cg2o_G1e_G2r,(G2_close,select2),g1cg2o_G1e_G2ie),
               (g1cg2o_G1e_G2ie,(G2_eoc,silentEvent),g1cg2c_G1e_G2e),
               (g1cg2c_G1e_G2e,(G1_open,silentEvent),g1cg2c_G1ir_G2e),
               (g1cg2c_G1ir_G2e,(G1_eoo,p2s),g2cg1o_G1r_G2e),

               (g2cg1o_G1r_G2e,(G1_close,select1),g2cg1o_G1ie_G2e),
               (g2cg1o_G1ie_G2e,(G1_eoc,silentEvent),g2cg1c_G1e_G2e),
               (g2cg1c_G1e_G2e,(G2_open,silentEvent),g2cg1c_G1e_G2ir),
               (g2cg1c_G1e_G2ir,(G2_eoo,p1s),g1cg2o_G1e_G2r) },
           x_0: g1og2o_G1r_G2r,
           Xm: {g1og2o_G1r_G2r,g1cg2o_G1e_G2r,g2cg1o_G1r_G2e},
           ctrlEvents: {G1_close, G1_open, G2_close, G2_open} ) &
 bound_G_A = 8
 &
 G_I = rec(states: StatesG_I,
           events: Output - {silentEvent},
           tf: {(Dp3,select1,Dp3p1), (Dp3p1,p1s,Dp1), (Dp1,select2,Dp3p2),
                                                      (Dp1,select3,Dpxp3),
                (Dp3,select2,Dp3p2), (Dp3p2,p2s,Dp2), (Dp2,select1,Dp3p1),
                                                      (Dp2,select3,Dpxp3),
                (Dpxp3,p3s,Dp3) },
           x_0: Dp3,
           Xm: {Dp1,Dp2,Dp3},
           ctrlEvents: {select1, select2, select3} ) &
 bound_G_I = 8
 &
 agentStatesOnLoops = {g1og2o_G1r_G2r} &
 interfaceStatesOnLoops = {Dp3} &

 agentIOonLoops = {(G1_close,select1)} - {(G1_close,select1)} &
 agentEventsOnLoops = G_ts'events - G_ts'events &
 interfaceEventsOnLoops = G_I'events
END

/*
OUTPUT:
% Runtime for SOLUTION for SETUP_CONSTANTS: 260 ms (walltime: 260 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2210 ms
Number of states of the transition system 13
Number of transitions of the transition system: 16
Size of the prefix-closed language of the transition system: 25
Size of the marked language of the transition system: 9
Number of states of the agent: 13
Number of transitions of the agent: 16
Size of the prefix-closed language of the agent: 25
Size of the marked language of the agent: 9
Number of states of the interface: 6
Number of transitions the interface: 9
Size of the prefix-closed language of the interface: 17
Size of the marked language of the interface: 9
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2220 ms
"Marking is consistent."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2230 ms
"The causal map is an observer."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2630 ms
"Control coincidence property is satisfied."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2650 ms
"The interface is nonblocking."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2650 ms
Deadlock reached after 7 steps (after Check_NonBlocking_Interface).

% Finished processing file after 850 ms walltime (750 ms runtime), since start: 2670 ms
*/