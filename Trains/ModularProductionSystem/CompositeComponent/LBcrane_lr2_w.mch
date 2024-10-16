// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBcrane_lr2_w
/*
  This crane wraps the interface of "crane_lr2" for specific usages by adding more
  state information defined by a memory. It is not really reusable, but it has been
  added to the repository to show how to create other components that model similar
  cranes, but with a simpler interface. Furthermore, it takes into account that, in
  a subsequent problem, there is no possibility of harmful interactions at the exit
  positions but that there is a potentially harmful interaction at the entry position
  while the hoist is grabbing a piece. It is therefore enough to know when this
  operation ends to avoid problems.
*/

SETS
/*
  sequencer states (MW, a memory word):
    idle := the crane is idle
     rjt := rejecting a piece
     trf := transferring a piece
  crane states:
    CatLaEp := crane at left above the entry position
    CatLnWp := crane at left with no workpiece
    CatRaRp := crane at right above the rejection (exit) position
    CatRaTp := crane at right above the transfer (exit) position
    CatRaWp := crane at right with a workpiece

  crane_lr2_w states (interface):
    idle
    exec: executing a command

*/
 StatesG_A = {idle_CatLnWp,
              rjt_CatLnWp, rjt_CatLaEp, rjt_CatRaWp, rjt_CatRaRp,
              trf_CatLnWp, trf_CatLaEp, trf_CatRaWp, trf_CatRaTp };
 StatesG_I = {Cexec, Cidle};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <seqC, crane>

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(reject) = idle
      ControlLaw(transfert) = idle
      ControlLaw(C_grab) = (rjt | trf) & CatLnWp
      ControlLaw(C_reject) = rjt & CatRaWp
      ControlLaw(C_transfer) = trf & CatRaWp

    Observer:
      Project_reject() := { return Func_rjct(); }
      Project_transfer() := { return Func_trsf(); }
      Project_C_eog() := { return Func_eop(); }
*/

/*
  Commands and signals:
    sequencer events:                       crane_lr2_w events (interface)
        reject := reject a piece
      transfer := transfer a piece
    crane events :
           C_eog := end of grabbing           eop := end of picking up
           C_eor := end of rejecting
           C_eot := end of transferring
          C_grab := grab a piece
        C_reject := reject a piece            rjct
      C_transfer := transfer a piece          trsf

  The event ``eoo'' (end of operation) could be replaced by the event ``eop'' (end of picking
  up a piece) for consistency of marking. Then theta(C_eor) = theta(C_eot) = eoo and
  theta(C_eog) = silentEvent.
*/
 Input = {reject, transfer, C_eog, C_eor, C_eot, C_grab, C_reject, C_transfer};
 Output = {eop, rjct, trsf, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 //G_FB,         Deterministic automaton exhaustively modeling the device behavior
 //              (defined in file SBcrane_lr2_w)
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
                 (rjt_CatLnWp,C_grab,rjt_CatLaEp),
                 (rjt_CatLaEp,C_eog,rjt_CatRaWp),
                 (rjt_CatRaWp,C_reject,rjt_CatRaRp),
                 (rjt_CatRaRp,C_eor,idle_CatLnWp),
                 
                 (idle_CatLnWp,transfer,trf_CatLnWp),
                 (trf_CatLnWp,C_grab,trf_CatLaEp),
                 (trf_CatLaEp,C_eog,trf_CatRaWp),
                 (trf_CatRaWp,C_transfer,trf_CatRaTp),
                 (trf_CatRaTp,C_eot,idle_CatLnWp) },
            x_0: idle_CatLnWp,
            Xm: {idle_CatLnWp},
            ctrlEvents: {reject, transfer, C_grab, C_reject, C_transfer} ) &
 bound_G_ts = 5
 & 
 G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,
           tf: {(idle_CatLnWp,(reject,rjct),rjt_CatLnWp),
                (rjt_CatLnWp,(C_grab,silentEvent),rjt_CatLaEp),
                (rjt_CatLaEp,(C_eog,eop),rjt_CatRaWp),
                (rjt_CatRaWp,(C_reject,silentEvent),rjt_CatRaRp),
                (rjt_CatRaRp,(C_eor,silentEvent),idle_CatLnWp),

                (idle_CatLnWp,(transfer,trsf),trf_CatLnWp),
                (trf_CatLnWp,(C_grab,silentEvent),trf_CatLaEp),
                (trf_CatLaEp,(C_eog,eop),trf_CatRaWp),
                (trf_CatRaWp,(C_transfer,silentEvent),trf_CatRaTp),
                (trf_CatRaTp,(C_eot,silentEvent),idle_CatLnWp) },
           x_0: idle_CatLnWp,
           Xm: {idle_CatLnWp},
           ctrlEvents: {reject, transfer, C_grab, C_reject, C_transfer} ) &
 bound_G_A = 5
 &
 G_I = rec(states: StatesG_I,
           events: Output - {silentEvent},
           tf: {(Cidle,rjct,Cexec), (Cidle,trsf,Cexec), (Cexec,eop,Cidle)},
           x_0: Cidle,
           Xm: {Cidle},
           ctrlEvents: {rjct, trsf} ) &
 bound_G_I = 2
END

/*
OUTPUT:
% Runtime for SOLUTION for SETUP_CONSTANTS: 180 ms (walltime: 180 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2110 ms
Number of states of the transition system 9
Number of transitions of the transition system: 10
Size of the prefix-closed language of the transition system: 11
Size of the marked language of the transition system: 3
Number of states of the agent: 9
Number of transitions of the agent: 10
Size of the prefix-closed language of the agent: 11
Size of the marked language of the agent: 3
Number of states of the interface: 2
Number of transitions the interface: 3
Size of the prefix-closed language of the interface: 5
Size of the marked language of the interface: 3
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2120 ms
"Marking is weakly consistent."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2180 ms
"The causal map is an observer."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2200 ms
"Control coincidence property is satisfied."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2210 ms
"The interface is nonblocking."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2210 ms
Deadlock reached after 7 steps (after Check_NonBlocking_Interface).

% Finished processing file after 390 ms walltime (360 ms runtime), since start: 2220 ms
*/