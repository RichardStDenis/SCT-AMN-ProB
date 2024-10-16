// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBdrill
/*
  This component includes an electric motor fixed at a vertical arm to vertically
  move the drill to perform a cylindrical hole in a piece. A clamp ensures (by
  tightening) the stability of a piece during the drilling operation. The motor,
  controlled by a relay, activates or deactivates the drilling operation. The drill
  also includes two memories. One acts as a switch to start and stop the drill and
  another divides the drilling operation into two phases once it is running.
*/

SETS
/*
  switch states  (MW, a memory word):
    isOff := the drill is off
     isOn := the drill is on
  operation phases (MW, a memory word):
    dp := in drilling phase
    fp := in final phase
  clamp states:
     Ce := clamp completely extended
    Cie := clamp in extension
    Cir := clamp in retraction
     Cr := clamp completely retracted
   motor states
     Mnv := motor with no velocity
     Mfv := motor running at full velocity
     Mdv := motor decreasing its velocity
     Miv := motor increasing its velocity
   arm states:
     Anr := arm not completely retracted
      Ar := arm completely retracted
*/
 StatesG_A = {isOff_dp_Cr_Mnv_Ar, isOn_dp_Cr_Mnv_Ar, 
              isOn_dp_Cie_Mnv_Ar, isOn_dp_Ce_Mnv_Ar,  isOn_dp_Ce_Miv_Ar,
              isOn_dp_Cr_Miv_Ar,  isOn_dp_Cr_Mfv_Ar,  isOn_dp_Cie_Mfv_Ar,
              isOn_dp_Cie_Miv_Ar,
              isOn_dp_Ce_Mfv_Ar,  isOn_dp_Ce_Mfv_Anr, isOn_fp_Ce_Mfv_Ar,
              isOn_fp_Cir_Mfv_Ar, isOn_fp_Cr_Mfv_Ar,  isOn_fp_Cr_Mdv_Ar,
              isOn_fp_Ce_Mdv_Ar,  isOn_fp_Ce_Mnv_Ar,  isOn_fp_Cir_Mnv_Ar,
              isOn_fp_Cir_Mdv_Ar,
              isOn_fp_Cr_Mnv_Ar };
 StatesG_I = {Dbusy, Didle};
/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <switch, operation, clamp, motor, arm>

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(start) = isOff
      ControlLaw(stop) = fp & Cr & Mnv
      ControlLaw(C_clamp) = isOn & dp
      ControlLaw(C_unclamp) = isOn & fp
      ControlLaw(M_tOn) = isOn & dp
      ControlLaw(M_tOff) = isOn & fp
      ControlLaw(A_gd) = isOn & dp & Ce & Mfv

    Observer:
      Project_SW_start() := { return Func_drill(); }
      Project_SW_stop() := { return Func_eod(); }

   Command to be sent to an actuator to reflect the occurrence an event:
      Lactions_start() := { SW := isOn; }
      Lactions_stop() := { SW := isOff; PHS = dp; }

    Cycle() := Cycle() + {
      Func_start();
      Func_stop(); }
*/

/*
  Commands and signals:
  switch events:                                 drill events (interface):
    start := start the drilling operation          drill
     stop := stop the drilling operation             eod := end of drilling
  clamp events:
      C_clamp := clamp a piece
        C_eoc := end of clamping
        C_eou := end of unclamping
    C_unclamp := unclamp a piece
   motor events
     M_tOff := turn off
      M_tOn := turn on
       M_fs := full speed
       M_ns := no speed
   arm events:
     A_eoa := end of ascent
      A_gd := go down
*/
 Input = {start, stop, C_clamp, C_eoc, C_eou, C_unclamp,
          M_fs, M_ns, M_tOff, M_tOn, A_eoa, A_gd};
 Output = {drill, eod, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 //G_FB,         Deterministic automaton exhaustively modeling the device behavior
 //              (defined in file SBdrill)
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
            tf:{(isOff_dp_Cr_Mnv_Ar,start,isOn_dp_Cr_Mnv_Ar),

                (isOn_dp_Cr_Mnv_Ar,C_clamp,isOn_dp_Cie_Mnv_Ar),
                (isOn_dp_Cie_Mnv_Ar,C_eoc,isOn_dp_Ce_Mnv_Ar),
                (isOn_dp_Ce_Mnv_Ar,M_tOn,isOn_dp_Ce_Miv_Ar),
                (isOn_dp_Ce_Miv_Ar,M_fs,isOn_dp_Ce_Mfv_Ar),

                (isOn_dp_Cr_Mnv_Ar,M_tOn,isOn_dp_Cr_Miv_Ar),
                (isOn_dp_Cr_Miv_Ar,M_fs,isOn_dp_Cr_Mfv_Ar),
                (isOn_dp_Cr_Mfv_Ar,C_clamp,isOn_dp_Cie_Mfv_Ar),
                (isOn_dp_Cie_Mfv_Ar,C_eoc,isOn_dp_Ce_Mfv_Ar),

                (isOn_dp_Cie_Mnv_Ar,M_tOn,isOn_dp_Cie_Miv_Ar),
                (isOn_dp_Cr_Miv_Ar,C_clamp,isOn_dp_Cie_Miv_Ar),
                (isOn_dp_Cie_Miv_Ar,C_eoc,isOn_dp_Ce_Miv_Ar),
                (isOn_dp_Cie_Miv_Ar,M_fs,isOn_dp_Cie_Mfv_Ar),

                (isOn_dp_Ce_Mfv_Ar,A_gd,isOn_dp_Ce_Mfv_Anr),
                (isOn_dp_Ce_Mfv_Anr,A_eoa,isOn_fp_Ce_Mfv_Ar),

                (isOn_fp_Ce_Mfv_Ar,C_unclamp,isOn_fp_Cir_Mfv_Ar),
                (isOn_fp_Cir_Mfv_Ar,C_eou,isOn_fp_Cr_Mfv_Ar),
                (isOn_fp_Cr_Mfv_Ar,M_tOff,isOn_fp_Cr_Mdv_Ar),
                (isOn_fp_Cr_Mdv_Ar,M_ns,isOn_fp_Cr_Mnv_Ar),

                (isOn_fp_Ce_Mfv_Ar,M_tOff,isOn_fp_Ce_Mdv_Ar),
                (isOn_fp_Ce_Mdv_Ar,M_ns,isOn_fp_Ce_Mnv_Ar),
                (isOn_fp_Ce_Mnv_Ar,C_unclamp,isOn_fp_Cir_Mnv_Ar),
                (isOn_fp_Cir_Mnv_Ar,C_eou,isOn_fp_Cr_Mnv_Ar),

                (isOn_fp_Cir_Mfv_Ar,M_tOff,isOn_fp_Cir_Mdv_Ar),
                (isOn_fp_Ce_Mdv_Ar,C_unclamp,isOn_fp_Cir_Mdv_Ar),
                (isOn_fp_Cir_Mdv_Ar,C_eou,isOn_fp_Cr_Mdv_Ar),
                (isOn_fp_Cir_Mdv_Ar,M_ns,isOn_fp_Cir_Mnv_Ar),

                (isOn_fp_Cr_Mnv_Ar,stop,isOff_dp_Cr_Mnv_Ar) },
            x_0: isOff_dp_Cr_Mnv_Ar,
            Xm: {isOff_dp_Cr_Mnv_Ar},
            ctrlEvents: {start, stop, C_clamp, C_unclamp ,M_tOff ,M_tOn ,A_gd} ) &
 bound_G_ts = 12
 & 
 G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,
            tf:{(isOff_dp_Cr_Mnv_Ar,(start,drill),isOn_dp_Cr_Mnv_Ar),

                (isOn_dp_Cr_Mnv_Ar,(C_clamp,silentEvent),isOn_dp_Cie_Mnv_Ar),
                (isOn_dp_Cie_Mnv_Ar,(C_eoc,silentEvent),isOn_dp_Ce_Mnv_Ar),
                (isOn_dp_Ce_Mnv_Ar,(M_tOn,silentEvent),isOn_dp_Ce_Miv_Ar),
                (isOn_dp_Ce_Miv_Ar,(M_fs,silentEvent),isOn_dp_Ce_Mfv_Ar),

                (isOn_dp_Cr_Mnv_Ar,(M_tOn,silentEvent),isOn_dp_Cr_Miv_Ar),
                (isOn_dp_Cr_Miv_Ar,(M_fs,silentEvent),isOn_dp_Cr_Mfv_Ar),
                (isOn_dp_Cr_Mfv_Ar,(C_clamp,silentEvent),isOn_dp_Cie_Mfv_Ar),
                (isOn_dp_Cie_Mfv_Ar,(C_eoc,silentEvent),isOn_dp_Ce_Mfv_Ar),

                (isOn_dp_Cie_Mnv_Ar,(M_tOn,silentEvent),isOn_dp_Cie_Miv_Ar),
                (isOn_dp_Cr_Miv_Ar,(C_clamp,silentEvent),isOn_dp_Cie_Miv_Ar),
                (isOn_dp_Cie_Miv_Ar,(C_eoc,silentEvent),isOn_dp_Ce_Miv_Ar),
                (isOn_dp_Cie_Miv_Ar,(M_fs,silentEvent),isOn_dp_Cie_Mfv_Ar),

                (isOn_dp_Ce_Mfv_Ar,(A_gd,silentEvent),isOn_dp_Ce_Mfv_Anr),
                (isOn_dp_Ce_Mfv_Anr,(A_eoa,silentEvent),isOn_fp_Ce_Mfv_Ar),

                (isOn_fp_Ce_Mfv_Ar,(C_unclamp,silentEvent),isOn_fp_Cir_Mfv_Ar),
                (isOn_fp_Cir_Mfv_Ar,(C_eou,silentEvent),isOn_fp_Cr_Mfv_Ar),
                (isOn_fp_Cr_Mfv_Ar,(M_tOff,silentEvent),isOn_fp_Cr_Mdv_Ar),
                (isOn_fp_Cr_Mdv_Ar,(M_ns,silentEvent),isOn_fp_Cr_Mnv_Ar),

                (isOn_fp_Ce_Mfv_Ar,(M_tOff,silentEvent),isOn_fp_Ce_Mdv_Ar),
                (isOn_fp_Ce_Mdv_Ar,(M_ns,silentEvent),isOn_fp_Ce_Mnv_Ar),
                (isOn_fp_Ce_Mnv_Ar,(C_unclamp,silentEvent),isOn_fp_Cir_Mnv_Ar),
                (isOn_fp_Cir_Mnv_Ar,(C_eou,silentEvent),isOn_fp_Cr_Mnv_Ar),

                (isOn_fp_Cir_Mfv_Ar,(M_tOff,silentEvent),isOn_fp_Cir_Mdv_Ar),
                (isOn_fp_Ce_Mdv_Ar,(C_unclamp,silentEvent),isOn_fp_Cir_Mdv_Ar),
                (isOn_fp_Cir_Mdv_Ar,(C_eou,silentEvent),isOn_fp_Cr_Mdv_Ar),
                (isOn_fp_Cir_Mdv_Ar,(M_ns,silentEvent),isOn_fp_Cir_Mnv_Ar),

                (isOn_fp_Cr_Mnv_Ar,(stop,eod),isOff_dp_Cr_Mnv_Ar) },
           x_0: isOff_dp_Cr_Mnv_Ar,
           Xm: {isOff_dp_Cr_Mnv_Ar},
           ctrlEvents: {start, stop, C_clamp, C_unclamp, M_tOff, M_tOn, A_gd} ) &
 bound_G_A = 12
 &
 G_I = rec(states: StatesG_I,
           events: Output - {silentEvent},
           tf: {(Didle,drill,Dbusy), (Dbusy,eod,Didle)},
           x_0: Didle,
           Xm: {Didle},
         ctrlEvents: {drill} ) &
 bound_G_I = 2
END

/*
OUTPUT:
% Runtime for SOLUTION for SETUP_CONSTANTS: 470 ms (walltime: 500 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2630 ms
Number of states of the transition system 20
Number of transitions of the transition system: 28
Size of the prefix-closed language of the transition system: 176
Size of the marked language of the transition system: 37
Number of states of the agent: 20
Number of transitions of the agent: 28
Size of the prefix-closed language of the agent: 176
Size of the marked language of the agent: 37
Number of states of the interface: 2
Number of transitions the interface: 2
Size of the prefix-closed language of the interface: 3
Size of the marked language of the interface: 2
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2640 ms
"Marking is consistent."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2640 ms
"The causal map is an observer."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2910 ms
"Control coincidence property is satisfied."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2920 ms
"The interface is nonblocking."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2930 ms
Deadlock reached after 7 steps (after Check_NonBlocking_Interface).

% Finished processing file after 1150 ms walltime (1020 ms runtime), since start: 2940 ms
*/