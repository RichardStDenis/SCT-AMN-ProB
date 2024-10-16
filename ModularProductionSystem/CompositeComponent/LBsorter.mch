// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBsorter
/*
  The sorter includes a three-position dispatcher through which a belt conveyor
  passes to carry a piece from an entry position to one of the three eligible
  exit positions. The sorter proceeds as soon as a position is chosen by an
  external device and terminates when the piece leaves the conveyor.
*/

SETS
/*
  seq states:
    p1 := transport of a piece to the first postion
    p2 := transport of a piece to the second postion
    p3 := transport of a piece to the third postion
     w := waiting transportation
  conveyor:
    Cnm := no motion
    Cim := in motion
  dispatcher:
      Dp1 := first position selected
      Dp2 := second position selected
      Dp3 := third position selected (default)
    Dp3p1 := change from third position selection to first position selection
    Dp3p2 := change from third position selection to second position selection
    Dpxp3 := change from first or second position selection to third position selection

  sorter states (interface):
    npt := no position targeted
    p1t := first position targeted
    p2t := second position targeted
    p3t := third position targeted
*/
 StatesG_A = {w_Cnm_Dp3,
              p1_Cnm_Dp3, p1_Cnm_Dp3p1, p1_Cnm_Dp1, p1_Cim_Dp1, w_Cnm_Dp1,
              p2_Cnm_Dp1, p3_Cnm_Dp1,
              p2_Cnm_Dp3, p2_Cnm_Dp3p2, p2_Cnm_Dp2, p2_Cim_Dp2, w_Cnm_Dp2,
              p1_Cnm_Dp2, p3_Cnm_Dp2,
              p3_Cnm_Dp3, p3_Cim_Dp3, p3_Cnm_Dpxp3 };
 StatesG_I = {Snpt, Sp1t, Sp2t, Sp3t};
/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <seqS, conveyor, dispatcher>

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(cp1) = w
      ControlLaw(cp2) = w
      ControlLaw(cp3) = w
      ControlLaw(Cstart) = Cnm & ((p1 & Sp1) | (p2 & Dp2) | (p3 & Dp3))
      ControlLaw(Sselect1) = p1 & (Dp2 | Dp3)
      ControlLaw(Sselect2) = p2 & (Dp1 | Dp3)
      ControlLaw(Sselect3) = p3 & (Dp1 | Dp2)

    Observer:
      Project_cp1() := { return Func_mp1(); }
      Project_cp2() := { return Func_mp2(); }
      Project_cp3() := { return Func_mp3(); }
      Project_Cmso() := {
        if ( p1 ) then return Func_pap1()
                  else if ( p2 ) then return Func_pap2()
                                 else return Func_pap3(); }
*/

/*
  Commands and signals:
    seq:                                          sorter events (interface):
      cp1 := carry a piece to the first position    mp1 := move the piece at the first position
      cp2 := carry a piece to the second position   mp2 := move the piece at the second position
      cp3 := carry a piece to the third position    mp3 := move the piece at the third position
    conveyor:
      Cmso := (conveyor) motor switched off        pap1 := piece arrival at the first position
                                                   pap2 := piece arrival at the second position
                                                   pap3 := piece arrival at the third position
      Cstart := start the conveyor belt
    disptacher:
      Dp1s := first position selected
      Dp2s := second position selected
      Dp3s := third position selected
      Dselect1 := select first position
      Dselect2 := select second position
      Dselect3 := select third position
*/
 Input = {cp1, cp2, cp3, Cmso, Cstart, Dp1s, Dp2s, Dp3s, Dselect1, Dselect2, Dselect3};
 Output = {mp1, mp2, mp3, pap1, pap2, pap3, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 //G_FB,         Deterministic automaton exhaustively modeling the device behavior
 //              under control (output in file SBsorter)
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
            tf:{(w_Cnm_Dp3,cp1,p1_Cnm_Dp3),
                (p1_Cnm_Dp3,Dselect1,p1_Cnm_Dp3p1),
                (p1_Cnm_Dp3p1,Dp1s,p1_Cnm_Dp1),
                (p1_Cnm_Dp1,Cstart,p1_Cim_Dp1),
                (p1_Cim_Dp1,Cmso,w_Cnm_Dp1),
                (w_Cnm_Dp1,cp1,p1_Cnm_Dp1),
                (w_Cnm_Dp1,cp2,p2_Cnm_Dp1), (p2_Cnm_Dp1,Dselect2,p2_Cnm_Dp3p2),
                (w_Cnm_Dp1,cp3,p3_Cnm_Dp1), (p3_Cnm_Dp1,Dselect3,p3_Cnm_Dpxp3),

                (w_Cnm_Dp3,cp2,p2_Cnm_Dp3),
                (p2_Cnm_Dp3,Dselect2,p2_Cnm_Dp3p2),
                (p2_Cnm_Dp3p2,Dp2s,p2_Cnm_Dp2),
                (p2_Cnm_Dp2,Cstart,p2_Cim_Dp2),
                (p2_Cim_Dp2,Cmso,w_Cnm_Dp2),
                (w_Cnm_Dp2,cp2,p2_Cnm_Dp2),
                (w_Cnm_Dp2,cp1,p1_Cnm_Dp2), (p1_Cnm_Dp2,Dselect1,p1_Cnm_Dp3p1),
                (w_Cnm_Dp2,cp3,p3_Cnm_Dp2), (p3_Cnm_Dp2,Dselect3,p3_Cnm_Dpxp3),

                (w_Cnm_Dp3,cp3,p3_Cnm_Dp3),
                (p3_Cnm_Dp3,Cstart,p3_Cim_Dp3),
                (p3_Cim_Dp3,Cmso,w_Cnm_Dp3),

                (p3_Cnm_Dpxp3,Dp3s,p3_Cnm_Dp3) },
            x_0: w_Cnm_Dp3,
            Xm: {w_Cnm_Dp1, w_Cnm_Dp2, w_Cnm_Dp3},
            ctrlEvents: {cp1, cp2, cp3, Cstart, Dselect1, Dselect2, Dselect3} ) &
 bound_G_ts = 10
 & 
 G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,
           tf:{(w_Cnm_Dp3,(cp1,mp1),p1_Cnm_Dp3),
               (p1_Cnm_Dp3,(Dselect1,silentEvent),p1_Cnm_Dp3p1),
               (p1_Cnm_Dp3p1,(Dp1s,silentEvent),p1_Cnm_Dp1),
               (p1_Cnm_Dp1,(Cstart,silentEvent),p1_Cim_Dp1),
               (p1_Cim_Dp1,(Cmso,pap1),w_Cnm_Dp1),
               (w_Cnm_Dp1,(cp1,mp1),p1_Cnm_Dp1),
               (w_Cnm_Dp1,(cp2,mp2),p2_Cnm_Dp1), (p2_Cnm_Dp1,(Dselect2,silentEvent),p2_Cnm_Dp3p2),
               (w_Cnm_Dp1,(cp3,mp3),p3_Cnm_Dp1), (p3_Cnm_Dp1,(Dselect3,silentEvent),p3_Cnm_Dpxp3),

               (w_Cnm_Dp3,(cp2,mp2),p2_Cnm_Dp3),
               (p2_Cnm_Dp3,(Dselect2,silentEvent),p2_Cnm_Dp3p2),
               (p2_Cnm_Dp3p2,(Dp2s,silentEvent),p2_Cnm_Dp2),
               (p2_Cnm_Dp2,(Cstart,silentEvent),p2_Cim_Dp2),
               (p2_Cim_Dp2,(Cmso,pap2),w_Cnm_Dp2),
               (w_Cnm_Dp2,(cp2,mp2),p2_Cnm_Dp2),
               (w_Cnm_Dp2,(cp1,mp1),p1_Cnm_Dp2), (p1_Cnm_Dp2,(Dselect1,silentEvent),p1_Cnm_Dp3p1),
               (w_Cnm_Dp2,(cp3,mp3),p3_Cnm_Dp2), (p3_Cnm_Dp2,(Dselect3,silentEvent),p3_Cnm_Dpxp3),

               (w_Cnm_Dp3,(cp3,mp3),p3_Cnm_Dp3),
               (p3_Cnm_Dp3,(Cstart,silentEvent),p3_Cim_Dp3),
               (p3_Cim_Dp3,(Cmso,pap3),w_Cnm_Dp3),

               (p3_Cnm_Dpxp3,(Dp3s,silentEvent),p3_Cnm_Dp3) },
            x_0: w_Cnm_Dp3,
            Xm: {w_Cnm_Dp1, w_Cnm_Dp2, w_Cnm_Dp3},
            ctrlEvents: {cp1, cp2, cp3, Cstart, Dselect1, Dselect2, Dselect3} ) &
 bound_G_A = 10
 &
 G_I = rec(states: StatesG_I,
           events: Output - {silentEvent},
           tf: {(Snpt,mp1,Sp1t), (Sp1t,pap1,Snpt),
                (Snpt,mp2,Sp2t), (Sp2t,pap2,Snpt),
                (Snpt,mp3,Sp3t), (Sp3t,pap3,Snpt) },
           x_0: Snpt,
           Xm: {Snpt},
           ctrlEvents: {mp1, mp2, mp3} ) &
 bound_G_I = 4
 &
 agentStatesOnLoops = G_A'states - G_A'states &
 interfaceStatesOnLoops = G_I'states - G_I'states &

 agentIOonLoops = {(cp3,mp3)} &
 agentEventsOnLoops = {cp3} &
 interfaceEventsOnLoops = {mp3}
END

/*
OUTPUT:
% Runtime for SOLUTION for SETUP_CONSTANTS: 340 ms (walltime: 360 ms)
Finished Invariant Checking 10 ms walltime (0 ms runtime), since start: 2380 ms
Number of states of the transition system 18
Number of transitions of the transition system: 24
Size of the prefix-closed language of the transition system: 70
Size of the marked language of the transition system: 12
Number of states of the agent: 18
Number of transitions of the agent: 24
Size of the prefix-closed language of the agent: 70
Size of the marked language of the agent: 12
Number of states of the interface: 4
Number of transitions the interface: 6
Size of the prefix-closed language of the interface: 23
Size of the marked language of the interface: 12
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2390 ms
"Marking is consistent."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2390 ms
"The causal map is an observer."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 3970 ms
"Control coincidence property is satisfied."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 3990 ms
"The interface is nonblocking."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 4000 ms
Deadlock reached after 7 steps (after Check_NonBlocking_Interface).

% Runtime: 1810 ms (total: 1810 ms, walltime: 2120 ms)
% Finished execute (errors=0, warnings=0)
% Finished processing file after 2210 ms walltime (1860 ms runtime), since start: 4020 ms

% Finished process_file (errors=0, warnings=0)
*/