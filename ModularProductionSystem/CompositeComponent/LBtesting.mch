// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBtesting
/*
  The handling station consists of lift, a micrometer, and an evacuator. It accepts
  a workpiece from the distribution station for a quality control test concerning its
  thickness. The piece with a thickness that lies in a tolerance interval is transferred
  into a workpiece retainer fixed on the rotating plate at the entry of the processing
  station. Otherwise, it is rejected.
*/

SETS
/*
  sequencer states:
       idle := the testing station is idle
     failm1 := the result of the thickness measure was fail
     failm2 := the measurement sequence is completed
     failm3 := the lift is down
     failm4 := the piece has been evacuated (rejected)
     passm1 := the result of the thickness measure was pass
     passm2 := the measurement sequence is completed
     passm3 := the piece has been evacuated (transferred)
     passm4 := the lift is down
    waiting := the testing station is waiting the result of the thickness measure
  lift states:
     Le := completely extended (the lift is up)
    Lie := in extension (the lift is lifting up)
    Lir := in retraction (the lift is lifting down)
     Lr := completely retracted (the lift is down)
  micrometer states:
    Midl := the micrometer is idle
     Mim := in measurement sequence
     Mmc := measurement sequence completed
     Mrk := result of the thickness measure known
  evacuator states:
     Enr := nor completely retracted (i.e., completely extended, in extension, in retraction)
      Er := completely retracted

  testing station states (interface):
       idle := the testing station is idle
         it := workpiece in transfer
      pause := waiting the result of the thickness measure
         rt := workpiece rejected or transferred
         wt := waiting a transfer
*/
 StatesG_A = {idle_Lr_Midl_Er,
              waiting_Lr_Midl_Er, waiting_Lie_Midl_Er,
              waiting_Le_Midl_Er, waiting_Le_Mim_Er,
              failm1_Le_Mrk_Er, failm2_Le_Mmc_Er,  failm2_Lir_Mmc_Er,
              failm3_Lr_Mmc_Er, failm3_Lr_Mmc_Enr, failm4_Lr_Mmc_Er,
              passm1_Le_Mrk_Er, passm2_Le_Mmc_Er,  passm2_Le_Mmc_Enr,
              passm3_Le_Mmc_Er, passm3_Lir_Mmc_Er, passm4_Lr_Mmc_Er };
 StatesG_I = {Tidle, Tit, Tpause, Trt, Twt};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <seqT, lift, micometer, evacuator>

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(start) = idle
      ControlLaw(L_liftdown) = (failm2 | passm3) & Le
      ControlLaw(L_liftup) = waiting & Lr
      ControlLaw(M_reset) = (failm4 | passm4) & Mmc
      ControlLaw(M_test) = Le & Midl
      ControlLaw(E_evacuate) = (failm3 | passm2) & Er

    Observer:
      Project_start() := { return Func_test(); }
      Project_M_fail) := { return Func_fail(); }
      Project_M_pass() := { return Func_pass(); }
      Project_M_reset() := { return Func_eot(); }
      Project_E_eoe() := { 
        if ( passm2 ) then return Func_eotwp()
                      else return; }
      Project_E_evacuate() := { 
        if ( passm2 ) then return Func_twp()
                      else return; }
*/

/*
  Commands and signals:
    sequencer events:                            testing station events (interface):
      start := start testing                       test := test the thickness of a workpiece
    lift events:
          L_eold := end of lift down
          L_eolu := endof lift up
      L_liftdown := lift down
        L_liftup := lift up
    micrometer events:
       M_fail := fail result                       fail
       M_pass := pass result                       pass
      M_reset := reset the measurement sequence     eot := end of testing
       M_test := test the thickness of a workpiece
    evacuator events:
           E_eoe := end of evacuation              eotwp := end of transfer
      E_evacuate := evacuate                         twp := transfer a workpiece
*/
 Input = {start, L_eold, L_eolu, L_liftdown, L_liftup,
          M_eot, M_fail, M_pass, M_reset, M_test, E_eoe, E_evacuate };
 Output = {eot, eotwp, fail, pass, test, twp, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 //G_FB,         Deterministic automaton exhaustively modeling the device behavior
 //              (defined in file SBtesting)
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
            tf:{(idle_Lr_Midl_Er,start,waiting_Lr_Midl_Er),
                (waiting_Lr_Midl_Er,L_liftup,waiting_Lie_Midl_Er),
                (waiting_Lie_Midl_Er,L_eolu,waiting_Le_Midl_Er),
                (waiting_Le_Midl_Er,M_test,waiting_Le_Mim_Er),
                (waiting_Le_Mim_Er,M_fail,failm1_Le_Mrk_Er),
                (waiting_Le_Mim_Er,M_pass,passm1_Le_Mrk_Er),

                (failm1_Le_Mrk_Er,M_eot,failm2_Le_Mmc_Er),
                (failm2_Le_Mmc_Er,L_liftdown,failm2_Lir_Mmc_Er),
                (failm2_Lir_Mmc_Er,L_eold,failm3_Lr_Mmc_Er),
                (failm3_Lr_Mmc_Er,E_evacuate,failm3_Lr_Mmc_Enr),
                (failm3_Lr_Mmc_Enr,E_eoe,failm4_Lr_Mmc_Er),
                (failm4_Lr_Mmc_Er,M_reset,idle_Lr_Midl_Er),

                (passm1_Le_Mrk_Er,M_eot,passm2_Le_Mmc_Er),
                (passm2_Le_Mmc_Er,E_evacuate,passm2_Le_Mmc_Enr),
                (passm2_Le_Mmc_Enr,E_eoe,passm3_Le_Mmc_Er),
                (passm3_Le_Mmc_Er,L_liftdown,passm3_Lir_Mmc_Er),
                (passm3_Lir_Mmc_Er,L_eold,passm4_Lr_Mmc_Er),
                (passm4_Lr_Mmc_Er,M_reset,idle_Lr_Midl_Er) },
            x_0: idle_Lr_Midl_Er,
            Xm: {idle_Lr_Midl_Er},
            ctrlEvents: {start, L_liftdown, L_liftup, M_reset, M_test, E_evacuate} ) &
 bound_G_ts = 11
 & 
 G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,
           tf:{(idle_Lr_Midl_Er,(start,test),waiting_Lr_Midl_Er),
               (waiting_Lr_Midl_Er,(L_liftup,silentEvent),waiting_Lie_Midl_Er),
               (waiting_Lie_Midl_Er,(L_eolu,silentEvent),waiting_Le_Midl_Er),
               (waiting_Le_Midl_Er,(M_test,silentEvent),waiting_Le_Mim_Er),
               (waiting_Le_Mim_Er,(M_fail,fail),failm1_Le_Mrk_Er),
               (waiting_Le_Mim_Er,(M_pass,pass),passm1_Le_Mrk_Er),

               (failm1_Le_Mrk_Er,(M_eot,silentEvent),failm2_Le_Mmc_Er),
               (failm2_Le_Mmc_Er,(L_liftdown,silentEvent),failm2_Lir_Mmc_Er),
               (failm2_Lir_Mmc_Er,(L_eold,silentEvent),failm3_Lr_Mmc_Er),
               (failm3_Lr_Mmc_Er,(E_evacuate,silentEvent),failm3_Lr_Mmc_Enr),
               (failm3_Lr_Mmc_Enr,(E_eoe,silentEvent),failm4_Lr_Mmc_Er),
               (failm4_Lr_Mmc_Er,(M_reset,eot),idle_Lr_Midl_Er),

               (passm1_Le_Mrk_Er,(M_eot,silentEvent),passm2_Le_Mmc_Er),
               (passm2_Le_Mmc_Er,(E_evacuate,twp),passm2_Le_Mmc_Enr),
               (passm2_Le_Mmc_Enr,(E_eoe,eotwp),passm3_Le_Mmc_Er),
               (passm3_Le_Mmc_Er,(L_liftdown,silentEvent),passm3_Lir_Mmc_Er),
               (passm3_Lir_Mmc_Er,(L_eold,silentEvent),passm4_Lr_Mmc_Er),
               (passm4_Lr_Mmc_Er,(M_reset,eot),idle_Lr_Midl_Er) },
           x_0: idle_Lr_Midl_Er,
           Xm: {idle_Lr_Midl_Er},
           ctrlEvents: {start, L_liftdown, L_liftup, M_reset, M_test, E_evacuate} ) &
 bound_G_A = 11
 &
 G_I = rec(states: StatesG_I,
           events: Output - {silentEvent},
           tf: {(Tidle,test,Tpause),
                (Tpause,fail,Trt), (Trt,eot,Tidle),
                (Tpause,pass,Twt), (Twt,twp,Tit), (Tit,eotwp,Trt) },
           x_0: Tidle,
           Xm: {Tidle},
           ctrlEvents: {test, twp} ) &
 bound_G_I = 5
END

/*
OUTPUT:
% Runtime for SOLUTION for SETUP_CONSTANTS: 230 ms (walltime: 240 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2150 ms
Number of states of the transition system 17
Number of transitions of the transition system: 18
Size of the prefix-closed language of the transition system: 19
Size of the marked language of the transition system: 3
Number of states of the agent: 17
Number of transitions of the agent: 18
Size of the prefix-closed language of the agent: 19
Size of the marked language of the agent: 3
Number of states of the interface: 5
Number of transitions the interface: 6
Size of the prefix-closed language of the interface: 8
Size of the marked language of the interface: 3
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2160 ms
"Marking is consistent."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2160 ms
"The causal map is an observer."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2300 ms
"Control coincidence property is satisfied."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2310 ms
"The interface is nonblocking."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2310 ms
Deadlock reached after 7 steps (after Check_NonBlocking_Interface).

% Runtime: 420 ms (total: 420 ms, walltime: 460 ms)
% Finished execute (errors=0, warnings=0)
% Finished processing file after 550 ms walltime (490 ms runtime), since start: 2330 ms

% Finished process_file (errors=0, warnings=0)
*/