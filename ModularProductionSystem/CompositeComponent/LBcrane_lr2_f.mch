// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBcrane_lr2_f
/*
  This crane is a crane with three axis-motions. It includes a boom, a bridge, and
  a hoist. The boom moves the bridge from left to right and vice versa. The bridge
  positions the hoist above the unique entry position (end position of the processing
  station at the left) or above one of two exit positions: one for rejection and one
  for the transfer of a piece to the sorting station at right. The hoist grabs
  a piece at the entry position and drop the piece at one of exit positions.
  With respect to "crane_lr2", the hoist includes a failure/recovery mode.
*/

SETS
/*
  sequencer states:
    fm := failure mode
    nm := normal mode
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
     Hrp := in recovery process
     Hwo := waiting a decision

  crane states Interface):
    CatLa := crane at left and abortion
    CatLaEp := crane at left above the entry position
    CatLnWp := crane at left with no workpiece
    CatLaEpWo := crane at left waiting option
    CatRaRp := crane at right above the rejection (exit) position
    CatRaTp := crane at right above the transfer (exit) position
    CatRaWp := crane at right with a workpiece
*/
 StatesG_A = {nm_BMr_BRr_HnWp,  nm_BMr_BRie_HnWp, nm_BMr_BRe_HnWp,  nm_BMr_BRe_Hwo,
              nm_BMr_BRe_HgWp,  nm_BMr_BRe_HaWp,  nm_BMr_BRir_HaWp, nm_BMr_BRr_HaWp,
              nm_BMie_BRr_HaWp, nm_BMe_BRr_HaWp,  nm_BMe_BRr_HdWp,  nm_BMe_BRr_HnWp,
              nm_BMir_BRr_HnWp,
              nm_BMe_BRie_HaWp, nm_BMe_BRe_HaWp,  nm_BMe_BRe_HdWp,  nm_BMe_BRe_HnWp,
              nm_BMe_BRir_HnWp,
              fm_BMr_BRe_Hrp,   fm_BMr_BRe_HnWp,  fm_BMr_BRir_HnWp };
 StatesG_I = {CatLa, CatLaEp, CatLnWp, CatLaEpWo, CatRaRp, CatRaTp, CatRaWp};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <seqC, boom, bridge, hoist>

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(BM_gl) = BMe & BRr & HnWp
      ControlLaw(BM_gr) = BMr & BRr & HaWp
      ControlLaw(BR_extend) = BRr & ((BMr & HnWp)| (BMe & HaWp))
      ControlLaw(BR_retract) = BRe & ((fm & HnWp) | (nm & BMr & HaWp)| (nm & BMe & HnWp))
      ControlLaw(H_drop) = BMe & (BRe | BRr) & HaWp
      ControlLaw(H_grab) = nm & BMr & BRe & HnWp

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
          BR_eor := end of retraction                    eoa/eot := end of abortion/transfer
      BR_retract := retract the hoist
    hook events :
        H_abort := abort the grabbing of a piece             abort
      H_confirm := confirm normal process                    confirm
         H_drop := drop                                      reject := reject a piece
          H_eod := end of dropping                              eor := end of reject
          H_eog := end of grabbing                              eog := end of grabbing
         H_grab := grab
        H_reset := reset (the bridge must be retracted)

*/
 Input = {BM_eogl, BM_eogr, BM_gl, BM_gr, BR_extend, BR_eoe, BR_eor, BR_retract,
          H_abort, H_confirm, H_drop, H_eod, H_eog, H_grab, H_reset};
 Output = {abort, confirm, eoa, eog, eor, eot, grab, reject, transfer, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 //G_FB,         Deterministic automaton exhaustively modeling the device behavior
 //              (defined in file SBcrane_lr2_f)
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
            tf: {(nm_BMr_BRr_HnWp,BR_extend,nm_BMr_BRie_HnWp),
                 (nm_BMr_BRie_HnWp,BR_eoe,nm_BMr_BRe_HnWp),
                 (nm_BMr_BRe_HnWp,H_grab,nm_BMr_BRe_Hwo),
                 (nm_BMr_BRe_Hwo,H_confirm,nm_BMr_BRe_HgWp),
                 (nm_BMr_BRe_HgWp,H_eog,nm_BMr_BRe_HaWp),
                 (nm_BMr_BRe_HaWp,BR_retract,nm_BMr_BRir_HaWp),
                 (nm_BMr_BRir_HaWp,BR_eor,nm_BMr_BRr_HaWp),
                 (nm_BMr_BRr_HaWp,BM_gr,nm_BMie_BRr_HaWp),
                 (nm_BMie_BRr_HaWp,BM_eogr,nm_BMe_BRr_HaWp),
                 (nm_BMe_BRr_HaWp,H_drop,nm_BMe_BRr_HdWp),
                 (nm_BMe_BRr_HdWp,H_eod,nm_BMe_BRr_HnWp),
                 (nm_BMe_BRr_HnWp,BM_gl,nm_BMir_BRr_HnWp),
                 (nm_BMir_BRr_HnWp,BM_eogl,nm_BMr_BRr_HnWp),

                 (nm_BMe_BRr_HaWp,BR_extend,nm_BMe_BRie_HaWp),
                 (nm_BMe_BRie_HaWp,BR_eoe,nm_BMe_BRe_HaWp),
                 (nm_BMe_BRe_HaWp,H_drop,nm_BMe_BRe_HdWp),
                 (nm_BMe_BRe_HdWp,H_eod,nm_BMe_BRe_HnWp),
                 (nm_BMe_BRe_HnWp,BR_retract,nm_BMe_BRir_HnWp),
                 (nm_BMe_BRir_HnWp,BR_eor,nm_BMe_BRr_HnWp),

                 (nm_BMr_BRe_Hwo,H_abort,fm_BMr_BRe_Hrp),
                 (fm_BMr_BRe_Hrp,H_reset,fm_BMr_BRe_HnWp),
                 (fm_BMr_BRe_HnWp,BR_retract,fm_BMr_BRir_HnWp),
                 (fm_BMr_BRir_HnWp,BR_eor,nm_BMr_BRr_HnWp) },
            x_0:nm_BMr_BRr_HnWp,
            Xm:{nm_BMr_BRr_HnWp},
            ctrlEvents: {BM_gl, BM_gr, BR_extend, BR_retract,
                         H_confirm, H_drop, H_grab, H_reset } ) &
 bound_G_ts = 17
 & 
 G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,
           tf: {(nm_BMr_BRr_HnWp,(BR_extend,grab),nm_BMr_BRie_HnWp),
                (nm_BMr_BRie_HnWp,(BR_eoe,silentEvent),nm_BMr_BRe_HnWp),
                (nm_BMr_BRe_HnWp,(H_grab,silentEvent),nm_BMr_BRe_Hwo),
                (nm_BMr_BRe_Hwo,(H_confirm,confirm),nm_BMr_BRe_HgWp),
                (nm_BMr_BRe_HgWp,(H_eog,eog),nm_BMr_BRe_HaWp),
                (nm_BMr_BRe_HaWp,(BR_retract,silentEvent),nm_BMr_BRir_HaWp),
                (nm_BMr_BRir_HaWp,(BR_eor,silentEvent),nm_BMr_BRr_HaWp),
                (nm_BMr_BRr_HaWp,(BM_gr,silentEvent),nm_BMie_BRr_HaWp),
                (nm_BMie_BRr_HaWp,(BM_eogr,silentEvent),nm_BMe_BRr_HaWp),
                (nm_BMe_BRr_HaWp,(H_drop,reject),nm_BMe_BRr_HdWp),
                (nm_BMe_BRr_HdWp,(H_eod,eor),nm_BMe_BRr_HnWp),
                (nm_BMe_BRr_HnWp,(BM_gl,silentEvent),nm_BMir_BRr_HnWp),
                (nm_BMir_BRr_HnWp,(BM_eogl,silentEvent),nm_BMr_BRr_HnWp),

                (nm_BMe_BRr_HaWp,(BR_extend,transfer),nm_BMe_BRie_HaWp),
                (nm_BMe_BRie_HaWp,(BR_eoe,silentEvent),nm_BMe_BRe_HaWp),
                (nm_BMe_BRe_HaWp,(H_drop,silentEvent),nm_BMe_BRe_HdWp),
                (nm_BMe_BRe_HdWp,(H_eod,silentEvent),nm_BMe_BRe_HnWp),
                (nm_BMe_BRe_HnWp,(BR_retract,silentEvent),nm_BMe_BRir_HnWp),
                (nm_BMe_BRir_HnWp,(BR_eor,eot),nm_BMe_BRr_HnWp),

                (nm_BMr_BRe_Hwo,(H_abort,abort),fm_BMr_BRe_Hrp),
                (fm_BMr_BRe_Hrp,(H_reset,silentEvent),fm_BMr_BRe_HnWp),
                (fm_BMr_BRe_HnWp,(BR_retract,silentEvent),fm_BMr_BRir_HnWp),
                (fm_BMr_BRir_HnWp,(BR_eor,eoa),nm_BMr_BRr_HnWp) },
           x_0:nm_BMr_BRr_HnWp,
           Xm:{nm_BMr_BRr_HnWp},  // nm_BMe_BRr_HnWp, nm_BMir_BRr_HnWp to ensure marking consistency
           ctrlEvents: {BM_gl, BM_gr, BR_extend, BR_retract,
                        H_confirm, H_drop, H_grab, H_reset } ) &
 bound_G_A = 17
 &
 G_I = rec(states: StatesG_I,
           events: Output - {silentEvent},
           tf: {(CatLnWp,grab,CatLaEpWo),   (CatLaEpWo,confirm,CatLaEp), (CatLaEp,eog,CatRaWp),
                (CatRaWp,reject,CatRaRp),   (CatRaRp,eor,CatLnWp),
                (CatRaWp,transfer,CatRaTp), (CatRaTp,eot,CatLnWp),
                (CatLaEpWo,abort,CatLa),     (CatLa,eoa,CatLnWp)},
           x_0: CatLnWp,
           Xm: {CatLnWp},
           ctrlEvents: {grab, reject, transfer} ) &
 bound_G_I = 5
END

/*
OUTPUT:
% Runtime for SOLUTION for SETUP_CONSTANTS: 300 ms (walltime: 320 ms)
Finished Invariant Checking 10 ms walltime (0 ms runtime), since start: 2300 ms
Number of states of the transition system 21
Number of transitions of the transition system: 23
Size of the prefix-closed language of the transition system: 26
Size of the marked language of the transition system: 4
Number of states of the agent: 21
Number of transitions of the agent: 23
Size of the prefix-closed language of the agent: 26
Size of the marked language of the agent: 4
Number of states of the interface: 7
Number of transitions the interface: 9
Size of the prefix-closed language of the interface: 10
Size of the marked language of the interface: 4
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2310 ms
"Marking is weakly consistent."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2510 ms
"The causal map is an observer."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2820 ms
"Control coincidence property is satisfied."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2830 ms
"The interface is nonblocking."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2830 ms
Deadlock reached after 7 steps (after Check_NonBlocking_Interface).

% Finished processing file after 1020 ms walltime (910 ms runtime), since start: 2850 ms
*/