// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBcrane_lr2
/*
  This crane is a crane with three axis-motions. It includes a boom, a bridge, and
  a hoist. The boom moves the bridge from left to right and vice versa. The bridge
  positions the hoist above the unique entry position (end position of the processing
  station at the left) or above one of two exit positions: one for rejection and one
  for the transfer of a piece to the sorting station at right. The hoist grabs
  a piece at the entry position and drop the piece at one of exit positions.
*/

SETS
/*
  boom states:                                   crane states:
     BMe := boom completely extended               at right
    BMie := boom in extension
    BMir := boom in retraction
     BMr := boom completely retracted              at left
  bridge states:
     BRe := bridge completely extended
    BRie := bridge in extension                    near the entry or transfer position
    BRir := bridge in retraction
     BRr := bridge completely retracted            above the rejection position
  hook states:
    HaWp := a workpiece                            a workpiece
    HdWp := dropping a workpiece
    HgWp := grabbing a workpiece
    HnWp := no workpiece                           no workpiece

  crane states Interface):
    CatLaEp := crane at left above the entry position
    CatLnWp := crane at left with no workpiece
    CatRaRp := crane at right above the rejection (exit) position
    CatRaTp := crane at right above the transfer (exit) position
    CatRaWp := crane at right with a workpiece
*/
 StatesG_A = {BMr_BRr_HnWp,  BMr_BRie_HnWp, BMr_BRe_HnWp,  BMr_BRe_HgWp, BMr_BRe_HaWp,
              BMr_BRir_HaWp, BMr_BRr_HaWp,  BMie_BRr_HaWp, BMe_BRr_HaWp, BMe_BRr_HdWp,
              BMe_BRr_HnWp,  BMir_BRr_HnWp, 
              BMe_BRie_HaWp, BMe_BRe_HaWp,  BMe_BRe_HdWp,  BMe_BRe_HnWp, BMe_BRir_HnWp};
 StatesG_I = {CatLaEp, CatLnWp, CatRaRp, CatRaTp, CatRaWp};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <boom, bridge, hoist>

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(BM_gl) = BMe & BRr & HnWp
      ControlLaw(BM_gr) = BMr & BRr & HaWp
      ControlLaw(BR_extend) = BRr & ((BMr & HnWp)| (BMe & HaWp))
      ControlLaw(BR_retract) = BRe & ((BMr & HaWp)| (BMe & HnWp))
      ControlLaw(H_drop) = BMe & (BRe | BRr) & HaWp
      ControlLaw(H_grab) = BMr & BRe & HnWp

    Observer:
      Project_BR_extend() := {
        if ( BMr ) then return Func_grab()
                   else return Func_transfer(); }
      Project_BR_eor() :=
        if ( BMe ) then return Func_eot()
                   else return true; }
      Project_H_drop() := {
        if ( BRr ) then return Func_reject()
                   else return true; }
      Project_H_eod() := {
        if ( BRr ) then return Func_eor()
                   else return true; }
      Project_H_eog() := {  return Func_eog(); }
*/

/*
  Commands and signals:
    boom events :                                crane events (interface):
      BM_eogl := end of going to the left
      BM_eogr := end of going to the right
        BM_gl := go to the left
        BM_gr := go to the right
    bridge events :
       BR_extend := extend the hoist               grab/transfer := grab/transfer a piece
          BR_eoe := end of extension
          BR_eor := end of retraction                        eot := end of transfer
      BR_retract := retract the hoist
    hook events :
      H_drop := drop                                      reject := reject a piece
      H_eod := end of dropping                               eor := end of reject
      H_eog := end of grabbing                               eog := end of grabbing
      H_grab := grab

*/
 Input = {BM_eogl, BM_eogr, BM_gl, BM_gr, BR_extend, BR_eoe, BR_eor, BR_retract,
          H_drop, H_eod, H_eog, H_grab};
 Output = {eog, eor, eot, grab, reject, transfer, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 //G_FB,         Deterministic automaton exhaustively modeling the device behavior
 //              (defined in file SBcrane_lr2)
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
            tf:{(BMr_BRr_HnWp,BR_extend,BMr_BRie_HnWp),
                (BMr_BRie_HnWp,BR_eoe,BMr_BRe_HnWp),
                (BMr_BRe_HnWp,H_grab,BMr_BRe_HgWp),
                (BMr_BRe_HgWp,H_eog,BMr_BRe_HaWp),
                (BMr_BRe_HaWp,BR_retract,BMr_BRir_HaWp),
                (BMr_BRir_HaWp,BR_eor,BMr_BRr_HaWp),
                (BMr_BRr_HaWp,BM_gr,BMie_BRr_HaWp),
                (BMie_BRr_HaWp,BM_eogr,BMe_BRr_HaWp),
                (BMe_BRr_HaWp,H_drop,BMe_BRr_HdWp),
                (BMe_BRr_HdWp,H_eod,BMe_BRr_HnWp),
                (BMe_BRr_HnWp,BM_gl,BMir_BRr_HnWp),
                (BMir_BRr_HnWp,BM_eogl,BMr_BRr_HnWp),

                (BMe_BRr_HaWp,BR_extend,BMe_BRie_HaWp),
                (BMe_BRie_HaWp,BR_eoe,BMe_BRe_HaWp),
                (BMe_BRe_HaWp,H_drop,BMe_BRe_HdWp),
                (BMe_BRe_HdWp,H_eod,BMe_BRe_HnWp),
                (BMe_BRe_HnWp,BR_retract,BMe_BRir_HnWp),
                (BMe_BRir_HnWp,BR_eor,BMe_BRr_HnWp) },
            x_0:BMr_BRr_HnWp,
            Xm:{BMr_BRr_HnWp},
            ctrlEvents: {BM_gl, BM_gr, BR_extend, BR_retract, H_drop, H_grab} ) &
 bound_G_ts = 16
 & 
 G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,
           tf:{(BMr_BRr_HnWp,(BR_extend,grab),BMr_BRie_HnWp),
               (BMr_BRie_HnWp,(BR_eoe,silentEvent),BMr_BRe_HnWp),
               (BMr_BRe_HnWp,(H_grab,silentEvent),BMr_BRe_HgWp),
               (BMr_BRe_HgWp,(H_eog,eog),BMr_BRe_HaWp),
               (BMr_BRe_HaWp,(BR_retract,silentEvent),BMr_BRir_HaWp),
               (BMr_BRir_HaWp,(BR_eor,silentEvent),BMr_BRr_HaWp),
               (BMr_BRr_HaWp,(BM_gr,silentEvent),BMie_BRr_HaWp),
               (BMie_BRr_HaWp,(BM_eogr,silentEvent),BMe_BRr_HaWp),
               (BMe_BRr_HaWp,(H_drop,reject),BMe_BRr_HdWp),
               (BMe_BRr_HdWp,(H_eod,eor),BMe_BRr_HnWp),
               (BMe_BRr_HnWp,(BM_gl,silentEvent),BMir_BRr_HnWp),
               (BMir_BRr_HnWp,(BM_eogl,silentEvent),BMr_BRr_HnWp),

               (BMe_BRr_HaWp,(BR_extend,transfer),BMe_BRie_HaWp),
               (BMe_BRie_HaWp,(BR_eoe,silentEvent),BMe_BRe_HaWp),
               (BMe_BRe_HaWp,(H_drop,silentEvent),BMe_BRe_HdWp),
               (BMe_BRe_HdWp,(H_eod,silentEvent),BMe_BRe_HnWp),
               (BMe_BRe_HnWp,(BR_retract,silentEvent),BMe_BRir_HnWp),
               (BMe_BRir_HnWp,(BR_eor,eot),BMe_BRr_HnWp) },
           x_0:BMr_BRr_HnWp,
           Xm:{BMr_BRr_HnWp},     // BMe_BRr_HnWp and BMir_BRr_HnWp to ensure marking consistency
           ctrlEvents: {BM_gl, BM_gr, BR_extend, BR_retract, H_drop, H_grab} ) &
 bound_G_A = 16
 &
 G_I = rec(states: StatesG_I,
           events: Output - {silentEvent},
           tf: {(CatLnWp,grab,CatLaEp),     (CatLaEp,eog,CatRaWp),
                (CatRaWp,reject,CatRaRp),   (CatRaRp,eor,CatLnWp),
                (CatRaWp,transfer,CatRaTp), (CatRaTp,eot,CatLnWp) },
           x_0: CatLnWp,
           Xm: {CatLnWp},
           ctrlEvents: {grab, reject, transfer} ) &
 bound_G_I = 4
END

/*
OUTPUT:
% Runtime for SOLUTION for SETUP_CONSTANTS: 270 ms (walltime: 270 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2220 ms
Number of states of the transition system 17
Number of transitions of the transition system: 18
Size of the prefix-closed language of the transition system: 21
Size of the marked language of the transition system: 3
Number of states of the agent: 17
Number of transitions of the agent: 18
Size of the prefix-closed language of the agent: 21
Size of the marked language of the agent: 3
Number of states of the interface: 5
Number of transitions the interface: 6
Size of the prefix-closed language of the interface: 7
Size of the marked language of the interface: 3
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2230 ms
"Marking is weakly consistent."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2410 ms
"The causal map is an observer."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2550 ms
"Control coincidence property is satisfied."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2560 ms
"The interface is nonblocking."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2560 ms
Deadlock reached after 7 steps (after Check_NonBlocking_Interface).

% Finished processing file after 750 ms walltime (690 ms runtime), since start: 2570 ms
*/