// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBmicrometer
/*
  Micrometer is a device used to get an accurate measurement of the thickness
  of a piece.
*/

SETS
/*
  sequencer states (MW, a memory word):
    bom := beginning of the measurement sequence
    mip := measurement in progress
    eom := end of the measurement sequence
  probe states:
     Pe := probe completely extended
    Pie := probe in extension
    Pir := probe in retraction
     Pr := probe completely retracted
  timer states:
      Ti := timer is idle
     Tip := timer is in progress
    Trtc := timer is ready to be cleared

  micrometer states (interface):
    Midl := the micrometer is idle
     Mim := in measurement sequence
     Mmc := measurement sequence completed
     Mrk := result known
*/
 StatesG_A = {bom_Pr_Trtc, bom_Pie_Trtc, bom_Pe_Trtc,  bom_Pe_Ti,
              bom_Pe_Tip,  mip_Pe_Trtc,  mip_Pir_Trtc, eom_Pr_Trtc };
 StatesG_I = {Midl, Mim, Mmc, Mrk};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <seqM, probe, timer>

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(rst) = eom
      ControlLaw(P_gd)= bom & Pr
      ControlLaw(P_gu) = mip & Pe
      ControlLaw(T_clr) = bom & Pe & Trtc
      ControlLaw(T_set) = Ti

    Observer:
      Project_rst() := { return Func_reset(); }
      Project_P_gd() := { return Func_get(); }
      Project_P_eoa() := { return Func_eom(); }
      Project_T_exp) := {
        if ( (XW0 < loLmt) or (xW0 > hiLmt) ) then return Func_failure()
                                              else return Func_success(); }

    Cycle() := Cycle() + {
      Func_rst(); }
*/

/*
  Commands and signals:
    sequencer events:                            micrometer events (nterface):
      rst:= reset the measurement sequence         reset := reset the micrometer
    probe events :
      P_eod := end of descent
      P_eoa := end of ascent                       eom := end of measurement
       P_gd := go down                             get := get thickness
       P_gu := go up
    timer events :
      T_clr := clear the timer (synthetic event)
      T_exp := timer interval expired              fORs := failure or success  // *** the causal map must be a function ***
      T_set := set (set the preset value)

*/
 Input = {rst, P_eoa, P_eod, P_gd, P_gu, T_clr, T_exp, T_set};
 Output = {eom, fORs, get, reset, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 //G_FB,         Deterministic automaton exhaustively modeling the device behavior
 //              (defined in file SBmicrometer)
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
            tf:{(bom_Pr_Trtc,P_gd,bom_Pie_Trtc),
                (bom_Pie_Trtc,P_eod,bom_Pe_Trtc),
                (bom_Pe_Trtc,T_clr,bom_Pe_Ti),
                (bom_Pe_Ti,T_set,bom_Pe_Tip),
                (bom_Pe_Tip,T_exp,mip_Pe_Trtc),
                (mip_Pe_Trtc,P_gu,mip_Pir_Trtc),
                (mip_Pir_Trtc,P_eoa,eom_Pr_Trtc),
                (eom_Pr_Trtc,rst,bom_Pr_Trtc) },
            x_0: bom_Pr_Trtc,
            Xm: {bom_Pr_Trtc},
            ctrlEvents: {rst, P_gd, P_gu, T_clr, T_set} ) &
 bound_G_ts = 8
 & 
 G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,
           tf:{(bom_Pr_Trtc,(P_gd,get),bom_Pie_Trtc),
               (bom_Pie_Trtc,(P_eod,silentEvent),bom_Pe_Trtc),
               (bom_Pe_Trtc,(T_clr,silentEvent),bom_Pe_Ti),
               (bom_Pe_Ti,(T_set,silentEvent),bom_Pe_Tip),
               (bom_Pe_Tip,(T_exp,fORs),mip_Pe_Trtc),
               (mip_Pe_Trtc,(P_gu,silentEvent),mip_Pir_Trtc),
               (mip_Pir_Trtc,(P_eoa,eom),eom_Pr_Trtc),
               (eom_Pr_Trtc,(rst,reset),bom_Pr_Trtc) },
           x_0: bom_Pr_Trtc,
           Xm: {bom_Pr_Trtc},
           ctrlEvents: {rst, P_gd, P_gu, T_clr, T_set} ) &
 bound_G_A = 8
 &
 G_I = rec(states: StatesG_I,
           events: Output - {silentEvent},
           tf: {(Midl,get,Mim), (Mim,fORs,Mrk), (Mrk,eom,Mmc), (Mmc,reset,Midl)},
           x_0: Midl,
           Xm: {Midl},
           ctrlEvents: {get, reset} ) &
 bound_G_I = 4
END

/*
OUTPUT:
% Runtime for SOLUTION for SETUP_CONSTANTS: 180 ms (walltime: 190 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2100 ms
Number of states of the transition system 8
Number of transitions of the transition system: 8
Size of the prefix-closed language of the transition system: 9
Size of the marked language of the transition system: 2
Number of states of the agent: 8
Number of transitions of the agent: 8
Size of the prefix-closed language of the agent: 9
Size of the marked language of the agent: 2
Number of states of the interface: 4
Number of transitions the interface: 4
Size of the prefix-closed language of the interface: 5
Size of the marked language of the interface: 2
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2110 ms
"Marking is consistent."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2110 ms
"The causal map is an observer."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2150 ms
"Control coincidence property is satisfied."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2150 ms
"The interface is nonblocking."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2150 ms
Deadlock reached after 7 steps (after Check_NonBlocking_Interface).

% Finished processing file after 370 ms walltime (340 ms runtime), since start: 2170 ms
*/