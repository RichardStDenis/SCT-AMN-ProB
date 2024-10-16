// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBcrane_lr_all
/*
  This crane is a unidirectional crane (one axis-motion) which moves a piece
  from left to right (lr). The projection is exhaustive.
*/

SETS
/*
  boom states:                                   crane states (interface):
     BMe := boom completely extended                  atR := at right without a workpiece
                                                   atRaWp := at right with a workpiece
    BMie := boom in extension                         nrR := near right without a workpiece
                                                   nrRaWp := near right with a workpiece
    BMir := boom in retraction                        nrL := near left
     BMr := boom completely retracted                 atL := at left

  hook states:
     Hl := hook loosing air                        dWp := dropping a workpiece
    Hnv := no vacuum
     Hp := hook pumpimg air                        gWp := grabbing a workpiece
     Hv := vacuum                                  aWp := a workpiece
*/
 StatesG_A = {BMe_Hnv, BMir_Hnv, BMr_Hnv, BMie_Hnv, 
              BMr_Hp, BMr_Hv, BMie_Hv, BMe_Hv, BMe_Hl};
 StatesG_I = {CatL, CatR, CatRaWp, CaWp, CdWp, CgWp, CnrL, CnrR, CnrRaWp};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <boom, hook>

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(BM_gl) = BMe & Hnv
      ControlLaw(BM_gr) = BMr & (Hnv | Hv)
      ControlLaw(H_drop) = BMe & Hv
      ControlLaw(H_grab) = BMr & Hnv

    Observer:
      Project_BM_eogl() := { return Func_eoml(); }
      Project_BM_eogr() := { return Func_eomr(); }
      Project_BM_gl() := { return Func_ml(); }
      Project_BM_gr() := { return Func_mr(); }
      Project_H_grab() := { return Func_grab(); }
      Project_H_eod() := { return Func_eod(); }
      Project_H_eog() := { return Func_eog(); }
      Project_H_grab() := { return Func_grab(); }
*/

/*
  Commands and signals:
    boom events:                                 crane events:
      BM_eogl := end of going to the left          eoml := end of moving to the left
      BM_eogr := end of going to the right         eomr := end of moving to the right
        BM_gl := go to the left                      ml := move to the left 
        BM_gr := go to the right                     mr := move to the right
    hook events:
      H_drop := drop                               drop := drop a workpiece
      H_eod := end of dropping                      eod := end of dropping a workpiece
      H_eog := end of grabbing                      eog := end of grabbing a workpiece
      H_grab := grab                               grab := grab a workpiece

*/
 Input = {BM_eogl, BM_eogr, BM_gl, BM_gr, H_drop, H_eod, H_eog, H_grab};
 Output = {drop, eod, eog, eoml, eomr, grab, ml, mr, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 //G_FB,         Deterministic automaton exhaustively modeling the device behavior
 //              (defined in file SBcrane_lr)
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
            tf:{(BMe_Hnv,BM_gl,BMir_Hnv), (BMir_Hnv,BM_eogl,BMr_Hnv),
                (BMr_Hnv,BM_gr,BMie_Hnv), (BMie_Hnv,BM_eogr,BMe_Hnv),
                (BMr_Hnv,H_grab,BMr_Hp),  (BMr_Hp,H_eog,BMr_Hv),
                (BMr_Hv,BM_gr,BMie_Hv),   (BMie_Hv,BM_eogr,BMe_Hv),
                (BMe_Hv,H_drop,BMe_Hl),   (BMe_Hl,H_eod,BMe_Hnv) },
            x_0: BMe_Hnv,
            Xm: {BMe_Hnv},
            ctrlEvents: {BM_gl, BM_gr, H_drop, H_grab} ) &
 bound_G_ts = 8
 & 
 G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,
           tf:{(BMe_Hnv,(BM_gl,ml),BMir_Hnv),  (BMir_Hnv,(BM_eogl,eoml),BMr_Hnv),
               (BMr_Hnv,(BM_gr,mr),BMie_Hnv),  (BMie_Hnv,(BM_eogr,eomr),BMe_Hnv),
               (BMr_Hnv,(H_grab,grab),BMr_Hp), (BMr_Hp,(H_eog,eog),BMr_Hv),
               (BMr_Hv,(BM_gr,mr),BMie_Hv),    (BMie_Hv,(BM_eogr,eomr),BMe_Hv),
               (BMe_Hv,(H_drop,drop),BMe_Hl),  (BMe_Hl,(H_eod,eod),BMe_Hnv) },
           x_0: BMe_Hnv,
           Xm: {BMe_Hnv},
           ctrlEvents: {BM_gl, BM_gr, H_drop, H_grab} ) &
 bound_G_A = 8
 &
 G_I = rec(states: StatesG_I,
           events: Output - {silentEvent},
           tf: {(CatR,ml,CnrL),      (CnrL,eoml,CatL),
                (CatL,mr,CnrR),      (CnrR,eomr,CatR),
                (CatL,grab,CgWp),    (CgWp,eog,CaWp),
                (CaWp,mr,CnrRaWp),   (CnrRaWp,eomr,CatRaWp),
                (CatRaWp,drop,CdWp), (CdWp,eod,CatR) },
           x_0: CatR,
           Xm: {CatR},
           ctrlEvents: {drop, grab, ml, mr} ) &
 bound_G_I = 8
END

/*
OUTPUT:
% Runtime for SOLUTION for SETUP_CONSTANTS: 200 ms (walltime: 200 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2090 ms
Number of states of the transition system 9
Number of transitions of the transition system: 10
Size of the prefix-closed language of the transition system: 11
Size of the marked language of the transition system: 3
Number of states of the agent: 9
Number of transitions of the agent: 10
Size of the prefix-closed language of the agent: 11
Size of the marked language of the agent: 3
Number of states of the interface: 9
Number of transitions the interface: 10
Size of the prefix-closed language of the interface: 11
Size of the marked language of the interface: 3
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2100 ms
"Marking is consistent."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2110 ms
"The causal map is an observer."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2130 ms
"Control coincidence property is satisfied."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2150 ms
"The interface is nonblocking."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2150 ms
Deadlock reached after 7 steps (after Check_NonBlocking_Interface).

% Finished processing file after 390 ms walltime (350 ms runtime), since start: 2160 ms
*/