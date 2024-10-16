// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBcrane_lr2_f_w
/*
  This crane wraps the interface of "crane_lr2_f" for specific usages by adding more
  state information defined by a memory. it is not really reusable, but it has been
  added to the repository to show how to create other components that model similar
  cranes, but with a simpler interface. Furthermore, it takes into account that, in
  a subsequent problem, there is no possibility of harmful interactions at the exit
  positions but that there is a potentially harmful interaction at the entry position
  while the hoist is grabbing a piece. It is therefore enough to know when this
  operation ends to avoid problems.
*/

SETS
/*
  sequencer states:
    idle := the crane is idle
     rjt := rejecting a piece
     trf := transferring a piece
  crane states:
        CatLa := crane at left during aborting
      CatLaEp := crane at left above the entry position
      CatLnWp := crane at left with no workpiece
    CatLaEpWo := crane at left above the entry position waiting an option
      CatRaRp := crane at right above the rejection (exit) position
      CatRaTp := crane at right above the transfer (exit) position
      CatRaWp := crane at right with a workpiece

  crane_lr2_w states (interface):
    idle
    exec: executing a command
*/
 StatesG_A = {idle_CatLnWp,
              rjt_CatLnWp, rjt_CatLaEpWo, rjt_CatLaEp, rjt_CatRaWp, rjt_CatRaRp,
              trf_CatLnWp, trf_CatLaEpWo, trf_CatLaEp, trf_CatRaWp, trf_CatRaTp,
              rjt_CatLa, trf_CatLa };
 StatesG_I = {Cexec, Cidle};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <seqC, crane>

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(reject) = idle
      ControlLaw(transfert) = idle
      ControlLaw(C_confirm) = (rjt | trf) & CatLaEpWo
      ControlLaw(C_grab) = (rjt | trf) & CatLnWp
      ControlLaw(C_reject) = rjt & CatRaWp
      ControlLaw(C_transfer) = trf & CatRaWp

    Observer:
      Project_reject() := { return Func_rjct(); }
      Project_transfer() := { return Func_trsf(); }
      Project_C_eoa() := { return Func_eoo(); }
      Project_C_eog() := { return Func_eoo(); }
*/

/*
  Commands and signals:
    sequencer events:                       crane_lr2_w_f events (interface)
        reject := reject a pice
      transfer := transfer a piece
    crane events :
         C_abort := abort rejection or transfer
       C_confirm := confirm rejection or transfer
           C_eoa := end of abortion           eoo : end of operation (abortion)
           C_eog := end of grabbing
           C_eor := end of rejecting          eoo : end of operation (rejection or transfer)
           C_eot := end of transferring       eoo : end of operation (rejection or transfer)
          C_grab := grab a piece
        C_reject := reject a piece            rjct
      C_transfer := transfer a piece          trsf
*/
 Input = {reject, transfer, C_abort, C_confirm, C_eoa, C_eog, C_eor ,C_eot,
          C_grab, C_reject, C_transfer};
 Output = {eoo, rjct, trsf, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 //G_FB,         Deterministic automaton exhaustively modeling the device behavior
 //              (defined in file SBcrane_lr2_f_w)
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
            tf: {(idle_CatLnWp,reject,rjt_CatLnWp),
                 (rjt_CatLnWp,C_grab,rjt_CatLaEpWo),
                 (rjt_CatLaEpWo,C_confirm,rjt_CatLaEp),
                 (rjt_CatLaEp,C_eog,rjt_CatRaWp),
                 (rjt_CatRaWp,C_reject,rjt_CatRaRp),
                 (rjt_CatRaRp,C_eor,idle_CatLnWp),

                 (idle_CatLnWp,transfer,trf_CatLnWp),
                 (trf_CatLnWp,C_grab,trf_CatLaEpWo),
                 (trf_CatLaEpWo,C_confirm,trf_CatLaEp),
                 (trf_CatLaEp,C_eog,trf_CatRaWp),
                 (trf_CatRaWp,C_transfer,trf_CatRaTp),
                 (trf_CatRaTp,C_eot,idle_CatLnWp),

                 (rjt_CatLaEpWo,C_abort,rjt_CatLa),
                 (rjt_CatLa,C_eoa,idle_CatLnWp),

                 (trf_CatLaEpWo,C_abort,trf_CatLa),
                 (trf_CatLa,C_eoa,idle_CatLnWp) },
            x_0: idle_CatLnWp,
            Xm: {idle_CatLnWp},
            ctrlEvents: {reject,transfer,C_confirm,C_grab,C_reject,C_transfer} ) &
 bound_G_ts = 6
 & 
 G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,
           tf: {(idle_CatLnWp,(reject,rjct),rjt_CatLnWp),
                (rjt_CatLnWp,(C_grab,silentEvent),rjt_CatLaEpWo),
                (rjt_CatLaEpWo,(C_confirm,silentEvent),rjt_CatLaEp),
                (rjt_CatLaEp,(C_eog,eoo),rjt_CatRaWp),
                (rjt_CatRaWp,(C_reject,silentEvent),rjt_CatRaRp),
                (rjt_CatRaRp,(C_eor,silentEvent),idle_CatLnWp),

                (idle_CatLnWp,(transfer,trsf),trf_CatLnWp),
                (trf_CatLnWp,(C_grab,silentEvent),trf_CatLaEpWo),
                (trf_CatLaEpWo,(C_confirm,silentEvent),trf_CatLaEp),
                (trf_CatLaEp,(C_eog,eoo),trf_CatRaWp),
                (trf_CatRaWp,(C_transfer,silentEvent),trf_CatRaTp),
                (trf_CatRaTp,(C_eot,silentEvent),idle_CatLnWp),

                (rjt_CatLaEpWo,(C_abort,silentEvent),rjt_CatLa),
                (rjt_CatLa,(C_eoa,eoo),idle_CatLnWp),

                (trf_CatLaEpWo,(C_abort,silentEvent),trf_CatLa),
                (trf_CatLa,(C_eoa,eoo),idle_CatLnWp) },
           x_0: idle_CatLnWp,
           Xm: {idle_CatLnWp},
           ctrlEvents: {reject, transfer, C_confirm, C_grab, C_reject, C_transfer} ) &
 bound_G_A = 6
 &
 G_I = rec(states: StatesG_I,
           events: Output - {silentEvent},
           tf: {(Cidle,rjct,Cexec), (Cidle,trsf,Cexec), (Cexec,eoo,Cidle)},
           x_0: Cidle,
           Xm: {Cidle},
           ctrlEvents: {rjct, trsf} ) &
 bound_G_I = 2
END

/*
OUTPUT:
% Runtime for SOLUTION for SETUP_CONSTANTS: 190 ms (walltime: 190 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2130 ms
Number of states of the transition system 13
Number of transitions of the transition system: 16
Size of the prefix-closed language of the transition system: 17
Size of the marked language of the transition system: 5
Number of states of the agent: 13
Number of transitions of the agent: 16
Size of the prefix-closed language of the agent: 17
Size of the marked language of the agent: 5
Number of states of the interface: 2
Number of transitions the interface: 3
Size of the prefix-closed language of the interface: 5
Size of the marked language of the interface: 3
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2140 ms
"Marking is weakly consistent."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2210 ms
"The causal map is an observer."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2240 ms
"Control coincidence property is satisfied."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2250 ms
"The interface is nonblocking."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2250 ms
Deadlock reached after 7 steps (after Check_NonBlocking_Interface).

% Finished processing file after 450 ms walltime (410 ms runtime), since start: 2260 ms
*/