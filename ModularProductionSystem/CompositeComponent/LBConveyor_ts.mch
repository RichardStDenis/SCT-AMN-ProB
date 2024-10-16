// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBconveyor_ts
/*
  This component is rotary conveyor or a turntable with four steps
  (only one step at a time).
*/

SETS
/*
  sequencer states (MW, a memory word):
    p1 := first phase of the operation sequence (before the end of a rotation)
    p2 := second phase of the operation sequence (before the timer expires)
  motor states:
    Mim := in motion
    Mnm := no motion
  timer states:
      Ti := timer is idle
     Tip := timer is in progress
    Trtc := timer is ready to be cleared

  converyor_ts states (interface):
    Ciup := in unstable position
    Cisp := in stable position
*/
 StatesG_A = {p1_Mnm_Trtc, p1_Mim_Trtc, p1_Mnm_Ti, p1_Mim_Ti,
              p2_Mnm_Ti, p2_Mnm_Tip, p2_Mnm_Trtc };
 StatesG_I = {Cisp, Ciup};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <seqC, motor, timer>

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(Mstep) = p1 & Mnm
      ControlLaw(Tclr) = Trtc
      ControlLaw(Tset) = p2 & Ti

    Observer:
      Project_T_clr) := {
        if ( p1 & Mnm ) then return Func_rotate()
                        else return true(); }
      Project_M_step) := {
        if ( Trtc ) then return Func_rotate()
                    else return true; }
      Project_T_exp() := { return Func_eor(); }
*/

/*
  Commands and signals:
    driver (converyorRotary):          conveyor_ts events:
       M_eos := stop
      M_step := start                    rotate := make one-step advancement
    timer events :
       T_clr := clear                    rotate := make one-step advancement
       T_exp := time interval expired       eor := end of one-step rotation
       T_set := set
*/
 Input = {M_eos, M_step, T_clr, T_exp, T_set};
 Output = {eor, rotate, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 //G_FB,         Deterministic automaton exhaustively modeling the device behavior
 //              (defined in file SBconvetor_ts)
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
            tf:{(p1_Mnm_Trtc,M_step,p1_Mim_Trtc), (p1_Mim_Trtc,T_clr,p1_Mim_Ti),
                (p1_Mnm_Trtc,T_clr,p1_Mnm_Ti),    (p1_Mnm_Ti,M_step,p1_Mim_Ti),
                (p1_Mim_Trtc,M_eos,p2_Mnm_Trtc),  (p2_Mnm_Trtc,T_clr,p2_Mnm_Ti),
                (p1_Mim_Ti,M_eos,p2_Mnm_Ti),
                (p2_Mnm_Ti,T_set,p2_Mnm_Tip),     (p2_Mnm_Tip,T_exp,p1_Mnm_Trtc) },
            x_0: p1_Mnm_Trtc,
            Xm: {p1_Mnm_Trtc},
            ctrlEvents: {M_step, T_clr, T_set} ) &
 bound_G_ts = 5
 &
 G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,

           tf:{(p1_Mnm_Trtc,(M_step,rotate),p1_Mim_Trtc),     (p1_Mim_Trtc,(T_clr,silentEvent),p1_Mim_Ti),
               (p1_Mnm_Trtc,(T_clr,rotate),p1_Mnm_Ti),        (p1_Mnm_Ti,(M_step,silentEvent),p1_Mim_Ti),
               (p1_Mim_Trtc,(M_eos,silentEvent),p2_Mnm_Trtc), (p2_Mnm_Trtc,(T_clr,silentEvent),p2_Mnm_Ti),
               (p1_Mim_Ti,(M_eos,silentEvent),p2_Mnm_Ti),
               (p2_Mnm_Ti,(T_set,silentEvent),p2_Mnm_Tip),    (p2_Mnm_Tip,(T_exp,eor),p1_Mnm_Trtc) },
           x_0: p1_Mnm_Trtc,
           Xm: {p1_Mnm_Trtc},
           ctrlEvents: {M_step, T_clr, T_set} ) &
 bound_G_A = 5
 &
 G_I = rec(states: StatesG_I,
           events: Output - {silentEvent},
           tf: {(Cisp,rotate,Ciup), (Ciup,eor,Cisp)},
           x_0: Cisp,
           Xm: {Cisp},
           ctrlEvents: {rotate} ) &
 bound_G_I = 2
END

/*
OUTPUT:
% Runtime for SOLUTION for SETUP_CONSTANTS: 180 ms (walltime: 180 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2060 ms
Number of states of the transition system 7
Number of transitions of the transition system: 9
Size of the prefix-closed language of the transition system: 15
Size of the marked language of the transition system: 4
Number of states of the agent: 7
Number of transitions of the agent: 9
Size of the prefix-closed language of the agent: 15
Size of the marked language of the agent: 4
Number of states of the interface: 2
Number of transitions the interface: 2
Size of the prefix-closed language of the interface: 3
Size of the marked language of the interface: 2
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2070 ms
"Marking is consistent."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2080 ms
"The causal map is an observer."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2100 ms
"Control coincidence property is satisfied."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2100 ms
"The interface is nonblocking."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2100 ms
Deadlock reached after 7 steps (after Check_NonBlocking_Interface).

% Finished processing file after 360 ms walltime (320 ms runtime), since start: 2120 ms
*/