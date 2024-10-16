// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBservomotor
/*
  The servo motor is a positioning mechanism driven by a rod drive servo motor used
  in a predetermined fixed position mode to move a device mounted on the rod (e.g.,
  the components of a crane). In this configuration, the servo motor provides four
  fixed positions (in the form of synthetic events) for the operation of the device
  in a specific sequence.

  The sequencer provides the available positioning sequence. Information on a 
  given position is not present in the automaton. It can be added later when
  using the component. The device can select any position (e.g., 1, 2, and 3)
  from position 0. From positions 1, 2, and 3, only position 0 is accessible.
  Position 0 is therefore a pivot through which any other positioning command
  must pass. State SMatpos0 of the interface corresponds to position 0 of the
  device (near-end position) and state SMatpos123 of the interface corresponds
  to the three other positions (1, 2, and 3) of the device (far-end position).
  The device does not have a position sensor and therefore a timer is required
  to infer that the last positioning command has been completed.
*/

SETS
/*
  sequencer states (MW, a memory word):
       im0 := in motion for position 0
     im123 := in motion for postion 1, 2 or 3
      rfm0 := ready for moving to position 0
    rfm123 := ready for moving to position 1, 2, or 3
  timer states:
      Ti := timer idle
     Tip := timer in progress
    Trtc := timer ready to clear

  servo motor states (interface):
       SMatpos0 := at position 0
     SMatpos123 := at position 1, 2, or 3
          SMnr0 := near position 0
        SMnr123 := near position 1, 2, or 3

*/
 StatesG_A = {im0_Ti, im0_Tip, im123_Ti, im123_Tip,
              rfm0_Ti, rfm0_Trtc, rfm123_Ti, rfm123_Trtc};
 StatesG_I = {SMatpos0, SMatpos123, SMnr0, SMnr123};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <seqM, timer>

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(mt0) = rfm0 & Ti
      ControlLaw(mt1) = rfm123 & Ti
      ControlLaw(mt2) = rfm123 & Ti
      ControlLaw(mt3) = rfm123 & Ti
      ControlLaw(T_clr) = Trtc
      ControlLaw(T_set) = (im0|im123) & Ti

    Observer:
      Project_mt0() := { return Func_gt0(); }
      Project_mt1() := { return Func_gt1(); }
      Project_mt2() := { return Func_gt2(); }
      Project_mt3() := { return Func_gt3(); }
      Project_T_clr) := {
        if ( rfm0) then return Func_eog123()
                   else return Func_eog0(); }

    Lactions_mt0() := { seq := im0; servomotor_register := pos0; }
    Lactions_mt1() := { seq := im123; servomotor_register := pos1; }
    Lactions_mt2() := { seq := im123; servomotor_register := pos2; }
    Lactions_mt3() := { seq := im123; servomotor_register := pos3; }

    Cycle() := Cycle() + {
      Func_mt0();
      Func_mt1();
      Func_mt2();
      Func_mt3(); }
*/

/*
  Commands and signals:
    seq (synthetic) events:            servo motor events:
      mt0 := move to position 0          gt0 := go to position 0
      mt1 := move to position 1          gt1 := go to position 1
      mt2 := move to position 2          gt2 := go to position 2
      mt3 := move to position 3          gt3 := go to position 3
    timer events :
      T_clr := clear timer               eog0/eog123 := end of going to position 0/1,2,or 3
      T_exp := time interval expired
      T_set := set (enable) the timer
*/
 Input = {mt0, mt1, mt2, mt3, T_clr, T_exp, T_set};
 Output = {eog0, eog123, gt0, gt1, gt2, gt3, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 //G_FB,         Deterministic automaton exhaustively modeling the device behavior
 //              (defined in file SBservomotor)
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
            tf:{(rfm123_Ti,mt1,im123_Ti),    (rfm123_Ti,mt2,im123_Ti),
                (rfm123_Ti,mt3,im123_Ti),    (im123_Ti,T_set,im123_Tip),
                (im123_Tip,T_exp,rfm0_Trtc), (rfm0_Trtc,T_clr,rfm0_Ti),
                (rfm0_Ti,mt0,im0_Ti),        (im0_Ti,T_set,im0_Tip),
                (im0_Tip,T_exp,rfm123_Trtc), (rfm123_Trtc,T_clr,rfm123_Ti)},
            x_0: rfm123_Ti,
            Xm: {rfm123_Ti},
            ctrlEvents: {mt0, mt1, mt2, mt3, T_clr, T_set} ) &
 bound_G_ts = 8
 & 
 G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,
           tf:{(rfm123_Ti,(mt1,gt1),im123_Ti),            (rfm123_Ti,(mt2,gt2),im123_Ti),
               (rfm123_Ti,(mt3,gt3),im123_Ti),            (im123_Ti,(T_set,silentEvent),im123_Tip),
               (im123_Tip,(T_exp,silentEvent),rfm0_Trtc), (rfm0_Trtc,(T_clr,eog123),rfm0_Ti),
               (rfm0_Ti,(mt0,gt0),im0_Ti),                (im0_Ti,(T_set,silentEvent),im0_Tip),
               (im0_Tip,(T_exp,silentEvent),rfm123_Trtc), (rfm123_Trtc,(T_clr,eog0),rfm123_Ti) },
           x_0: rfm123_Ti,
           Xm: {rfm123_Ti},
           ctrlEvents: {mt0, mt1, mt2, mt3, T_clr, T_set} ) &
 bound_G_A = 8
 &
 G_I = rec(states: StatesG_I,
           events: Output - {silentEvent},
           tf: {(SMatpos0,gt1,SMnr123),      (SMatpos0,gt2,SMnr123), (SMatpos0,gt3,SMnr123),
                (SMnr123,eog123,SMatpos123), (SMatpos123,gt0,SMnr0), (SMnr0,eog0,SMatpos0)},
           x_0: SMatpos0,
           Xm: {SMatpos0},
           ctrlEvents: {gt0, gt1, gt2, gt3} ) &
 bound_G_I = 4
END

/*
OUTPUT:
% Runtime for SOLUTION for SETUP_CONSTANTS: 200 ms (walltime: 200 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2130 ms
Number of states of the transition system 8
Number of transitions of the transition system: 10
Size of the prefix-closed language of the transition system: 25
Size of the marked language of the transition system: 4
Number of states of the agent: 8
Number of transitions of the agent: 10
Size of the prefix-closed language of the agent: 25
Size of the marked language of the agent: 4
Number of states of the interface: 4
Number of transitions the interface: 6
Size of the prefix-closed language of the interface: 13
Size of the marked language of the interface: 4
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2140 ms
"Marking is consistent."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2150 ms
"The causal map is an observer."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2370 ms
"Control coincidence property is satisfied."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2380 ms
"The interface is nonblocking."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2380 ms
Deadlock reached after 7 steps (after Check_NonBlocking_Interface).

% Finished processing file after 600 ms walltime (530 ms runtime), since start: 2400 ms
*/