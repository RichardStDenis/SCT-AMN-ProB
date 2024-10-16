// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBcrane_lr
/*
  This crane is a unidirectional crane (one axis-motion) which moves a piece
  from left to right (lr).
*/

SETS
/*
  boom states:                                   crane states (interface):
     BMe := boom completely extended               atR := at right
    BMie := boom in extension                      nrR := near right
    BMir := boom in retraction                     nrL := near left
     BMr := boom completely retracted              atL := at left
  hook states:
     Hl := hook loosing air
    Hnv := no vacuum
     Hp := hook pumpimg air                        gWp := grabbing a workpiece
     Hv := vacuum                                  aWp := a workpiece
*/
 StatesG_A = {BMe_Hnv, BMir_Hnv, BMr_Hnv, BMie_Hnv, 
              BMr_Hp, BMr_Hv, BMie_Hv, BMe_Hv, BMe_Hl};
 StatesG_I = {CatL, CatR, CaWp, CgWp, CnrL, CnrR};

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
      Project_H_eog() := { return Func_eog(); }
      Project_H_grab() := { return Func_grab(); }
*/

/*
  Commands and signals:
    boom events:                                 crane events (interface):
      BM_eogl := end of going to the left          eoml := end of moving to the left
      BM_eogr := end of going to the right         eomr := end of moving to the right
        BM_gl := go to the left                      ml := move to the left 
        BM_gr := go to the right                     mr := move to the right
    hook events:
      H_drop := drop
      H_eod := end of dropping                     eomr := end of moving to the right
      H_eog := end of grabbing                      eog := end of grabbing a piece
      H_grab := grab                               grab := grab a piece

*/
 Input = {BM_eogl, BM_eogr, BM_gl, BM_gr, H_drop, H_eod, H_eog, H_grab};
 Output = {eog, eoml, eomr, grab, ml, mr, silentEvent}

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
           tf:{(BMe_Hnv,(BM_gl,ml),BMir_Hnv),        (BMir_Hnv,(BM_eogl,eoml),BMr_Hnv),
               (BMr_Hnv,(BM_gr,mr),BMie_Hnv),        (BMie_Hnv,(BM_eogr,eomr),BMe_Hnv),
               (BMr_Hnv,(H_grab,grab),BMr_Hp),       (BMr_Hp,(H_eog,eog),BMr_Hv),
               (BMr_Hv,(BM_gr,mr),BMie_Hv),          (BMie_Hv,(BM_eogr,eomr),BMe_Hv),
               (BMe_Hv,(H_drop,silentEvent),BMe_Hl), (BMe_Hl,(H_eod,silentEvent),BMe_Hnv) },
           x_0: BMe_Hnv,
           Xm: {BMe_Hnv},
           ctrlEvents: {BM_gl, BM_gr, H_drop, H_grab} ) &
 bound_G_A = 8
 &
 G_I = rec(states: StatesG_I,
           events: Output - {silentEvent},
           tf: {(CatR,ml,CnrL),   (CnrL,eoml,CatL),
                (CatL,mr,CnrR),   (CnrR,eomr,CatR),
                (CatL,grab,CgWp), (CgWp,eog,CaWp), (CaWp,mr,CnrR) },
           x_0: CatR,
           Xm: {CatR},
           ctrlEvents: {grab, ml, mr} ) &
 bound_G_I = 6
END

/*
OUTPUT:
% Runtime for SOLUTION for SETUP_CONSTANTS: 190 ms (walltime: 190 ms)
Finished Invariant Checking 0 ms walltime (10 ms runtime), since start: 2120 ms
Number of states of the transition system 9
Number of transitions of the transition system: 10
Size of the prefix-closed language of the transition system: 11
Size of the marked language of the transition system: 3
Number of states of the agent: 9
Number of transitions of the agent: 10
Size of the prefix-closed language of the agent: 11
Size of the marked language of the agent: 3
Number of states of the interface: 6
Number of transitions the interface: 7
Size of the prefix-closed language of the interface: 9
Size of the marked language of the interface: 3
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2130 ms
"Marking is weakly consistent."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2190 ms
"The causal map is an observer."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2270 ms
"Control coincidence property is satisfied."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2280 ms
"The interface is nonblocking."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2280 ms
Deadlock reached after 7 steps (after Check_NonBlocking_Interface).

% Finished processing file after 480 ms walltime (440 ms runtime), since start: 2290 ms
*/