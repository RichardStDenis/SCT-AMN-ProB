// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBcrane_br_w
/*
  This crane is just a wrapper for the crane_br used to give it more state information.
  There is no additional control required compared to crane_br and the projection is
  exhaustive. All transitions are projected although the additional state information
  allows for meaningful event names and a state space allowing the control required
  for the intended use by an handling station.
*/

SETS
/*
  seq states (MW, a memory word):
    p1 := picking-up a piece at the first position
    p2 := picking-up a piece at the second position
    p3 := picking-up a piece at the third position
     w := waiting
  crane-br states:
    Cbusy := busy
    Cidle := idle
*/
 StatesG_A = {p1_Cbusy, p2_Cbusy, p3_Cbusy, w_Cidle};
 StatesG_I = {CBRWp1, CBRWp2, CBRWp3, CBRWw};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <seqC, crane>

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(C_pck1) = w_Cidle
      ControlLaw(C_pck2) = w_Cidle
      ControlLaw(C_pck2) = w_Cidle

    Observer:
      Project_C_eot() := {
        if ( p1 ) then return Func_eop1()
                  else if ( p2 ) then return Func_eop2()
                                 else return Func_eop3(); }
      Project_C_pck1() := { return Func_pck1); }
      Project_C_pck2() := { return Func_pck2); }
      Project_C_pck2() := { return Func_pck3); }
*/

/*
  Commands and signals:
    crane-br  events:
      C_eop1 := end of pick up at position 1
      C_eop2 := end of pick up at position 2
      C_eop3 := end of pick up at position 3
      C_pck1 := pick up a piece at position 1
      C_pck2 := pick up a piece at position 2
      C_pck3 := pick up a piece at position 3
*/
 Input = {C_eot ,C_pck1, C_pck2, C_pck3};
 Output = {eop1, eop2, eop3, pck1, pck2, pck3, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 //G_FB,         Deterministic automaton exhaustively modeling the device behavior
 //              (defined in file SBcrane_br_w)
 G_ts,        // G_FB under control of a state feedback function (solution of a control problem)
 bound_G_ts,  // Bound on the trace length of the controlled device behavior
 G_A,         // Mealy machine modeling the device implementation (agent)
 bound_G_A,   // Bound on the trace length of the agent
 G_I,         // (quotient deterministic) automaton modeling a device abstraction (interface)
 bound_G_I    // Bound on the trace length of the interface

PROPERTIES
 VocalOutput = Output - {silentEvent}
 &
 G_ts = rec(states: StatesG_A,
            events: Input,
            tf:{(w_Cidle,C_pck1,p1_Cbusy), (p1_Cbusy,C_eot,w_Cidle),
                (w_Cidle,C_pck2,p2_Cbusy), (p2_Cbusy,C_eot,w_Cidle),
                (w_Cidle,C_pck3,p3_Cbusy), (p3_Cbusy,C_eot,w_Cidle) },
            x_0: w_Cidle,
            Xm: {w_Cidle},
            ctrlEvents: {C_pck1, C_pck2, C_pck3} ) &
 bound_G_ts = 2
 & 
 G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,
           tf:{(w_Cidle,(C_pck1,pck1),p1_Cbusy), (p1_Cbusy,(C_eot,eop1),w_Cidle),
               (w_Cidle,(C_pck2,pck2),p2_Cbusy), (p2_Cbusy,(C_eot,eop2),w_Cidle),
               (w_Cidle,(C_pck3,pck3),p3_Cbusy), (p3_Cbusy,(C_eot,eop3),w_Cidle) },
           x_0: w_Cidle,
           Xm: {w_Cidle},
           ctrlEvents: {C_pck1, C_pck2, C_pck3} ) &
 bound_G_A = 2
 &
 G_I = rec(states: StatesG_I,
           events: Output - {silentEvent},
           tf:{(CBRWw,pck1,CBRWp1), (CBRWp1,eop1,CBRWw),
               (CBRWw,pck2,CBRWp2), (CBRWp2,eop2,CBRWw),
               (CBRWw,pck3,CBRWp3), (CBRWp3,eop3,CBRWw) },
           x_0: CBRWw,
           Xm: {CBRWw},
           ctrlEvents: {pck1,pck2,pck3} ) &
 bound_G_I = 2
END

/*
OUTPUT:
% Runtime for SOLUTION for SETUP_CONSTANTS: 170 ms (walltime: 170 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2070 ms
Number of states of the transition system 4
Number of transitions of the transition system: 6
Size of the prefix-closed language of the transition system: 7
Size of the marked language of the transition system: 4
Number of states of the agent: 4
Number of transitions of the agent: 6
Size of the prefix-closed language of the agent: 7
Size of the marked language of the agent: 4
Number of states of the interface: 4
Number of transitions the interface: 6
Size of the prefix-closed language of the interface: 7
Size of the marked language of the interface: 4
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2080 ms
"Marking is consistent."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2080 ms
"The causal map is an observer."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2100 ms
"Control coincidence property is satisfied."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2110 ms
"The interface is nonblocking."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2110 ms
Deadlock reached after 7 steps (after Check_NonBlocking_Interface).

% Finished processing file after 340 ms walltime (290 ms runtime), since start: 2120 ms
*/