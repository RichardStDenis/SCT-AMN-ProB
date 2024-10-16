// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBdistribution
/*
  The distribution station consists of an injector, a crane, and a memory. Its magazine
  is filled by the handling station. The crane transfers the workpiece in the external
  stop of the injector to the testing station after the injector has been pushed the
  workpiece out of the magazine.
*/

SETS
/*
  sequencer states (MW, a memory word):
    s0 := no workpiece in the injector and crane at right
    s1 := first workpiece pushed out of the magazine and crane at right
    s2 := first workpiece at the external stop and crane moving to the left
    s3 := crane at left for grabbing a first workpiece
    s4 := crane at left with a workpiece
    s5 := injector freeing the basis of the magazine and crane moving to the right with a workpiece
    s6 := workpiece pushed out of the magazine and crane moving to the right with a workpiece
    s7 := crane at right with a workpiece and no workpiece in the injector
    s8 := workpiece pushed out of the magazine and crane at right with a workpiece
    s9 := workpiece at the external stop and crane moving to the left
   s10 := crane at left for grabbing a workpiece
   s11 := crane moving to the left and no workpiece in the injector
   s12 := workpiece pushed out of the magazine and crane moving to the left
   s13 := crane at left and no workpiece in the injector
   s14 := workpiece pushed out of the magazine and crane at left
   s15 := workpiece pushed out of the magazine and crane moving to the right
   s16 := no workpiece in the injector and crane moving to the right

  injector states:
      Ie := completely extended (holding a workpiece)
     Iie := in extension (pushing a workpiece)
      Ir := completely retracted
    Iwim := workpiece in magazine
  crane states:
      CatL := at left
      CatR := at right without a workpiece
   CatRaWp := at right with a workpiece
      CaWp := holding a workpiece
      CdWp := dropping a workpiece
      CgWp := grabbing a workpiece
      CnrL := near to left
      CnrR := near to right without a workpiece
   CnrRaWp := near to right with a workpiece

  distributiong station states (interface):
      DcrR := crane at the right
      DcrL := crane at the left with one workpiece in the distribution station
     DcrL1 := crane at the left with two workpieces in the distribution station
   Dm1stwp := moving a first piecework
    D2ndwp := a second workpiece in the distribution station
*/
 StatesG_A = {s0_Ir_CatR,      s1_Iwim_CatR,    s1_Iie_CatR,    s2_Ie_CatR,   s2_Ie_CnrL,
              s3_Ie_CatL,      s3_Ie_CgWp,      s4_Ie_CaWp,     s5_Ir_CaWp,   s5_Ir_CnrRaWp,
              s6_Iwim_CaWp,    s6_Iwim_CnrRaWp, s7_Ir_CatRaWp,  s7_Ir_CdWp,   s7_Ir_CatR,
              s8_Iwim_CatRaWp, s8_Iwim_CdWp,    s8_Iie_CatRaWp, s8_Iwim_CatR, s8_Iie_CdWp,
              s8_Iie_CatR,     s9_Ie_CatRaWp,   s9_Ie_CdWp,     s9_Ie_CatR,   s9_Ie_CnrL,
              s10_Ie_CatL,     s11_Ir_CnrL,     s12_Iwim_CnrL,  s13_Ir_CatL,  s14_Iwim_CatL,
              s15_Iwim_CatL,   s15_Iwim_CnrR,   s16_Ir_CatL,     s16_Ir_CnrR };
 StatesG_I = {DcrR, Dm1stwp, D2ndwp, DcrL, DcrL1};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <seqD, injector, crane>

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(reset) = (s10 | s13 | s14)
      ControlLaw(I_inject) = (s1 | s8) & Iwin
      ControlLaw(I_rearm) = s4 & Ie
      ControlLaw(C_drop) = CatRaWp
      ControlLaw(C_gl) = (s2 | s7 | s9) & CatR
      ControlLaw(C_grab) = s3 & CatL
      ControlLaw(C_gr) = ((s5 | s6) & CaWp) | ((s15 | s16) & CatL)

    Observer:
      Project_reset() := { return Func_release(); }
      Project_M_wpa) := { return Func_wpa(); }
      Project_E_eogl() := { 
        if ( s2 ) then return
                  else Func_creogl() }
*/

/*
  Commands and signals:
    sequencer events:                                         distribution station events (interface):
      reset := reset                                           release := release the testing station
    injector events:
         I_eoi := end of injection
      I_inject := inject a workpiece from the magazine
       I_rearm := rearm the injector
         I_wpa := workpiece arrival at the basis of the injector   wpa := workpiece arrival
    crane events:
      C_drop := drop a workpiece
       C_eod := end of dropping a workpice
       C_eog := end of grabbing a workpiece
      C_eogl := end of going to the left                        creogl := crane end of going to the left
      C_eogr := end of going to the right
        C_gl := go to the left
        C_gr := go to the right
      C_grab := grab a workpiece
*/



 Input = {reset, I_eoi, I_inject, I_rearm, I_wpa,
          C_drop, C_eod, C_eog, C_eogl, C_eogr, C_gl, C_gr, C_grab };
 Output = {wpa, release, creogl, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 //G_FB,         Deterministic automaton exhaustively modeling the device behavior
 //              (defined in file SBdistribution)
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
            tf:{(s0_Ir_CatR,I_wpa,s1_Iwim_CatR),
                (s1_Iwim_CatR,I_inject,s1_Iie_CatR),
                (s1_Iie_CatR,I_eoi,s2_Ie_CatR),
                (s2_Ie_CatR,C_gl,s2_Ie_CnrL),
                (s2_Ie_CnrL,C_eogl,s3_Ie_CatL),
                (s3_Ie_CatL,C_grab,s3_Ie_CgWp),
                (s3_Ie_CgWp,C_eog,s4_Ie_CaWp),
                (s4_Ie_CaWp,I_rearm,s5_Ir_CaWp),

                (s5_Ir_CaWp,C_gr,s5_Ir_CnrRaWp),
                (s5_Ir_CnrRaWp,I_wpa,s6_Iwim_CnrRaWp),    (s5_Ir_CnrRaWp,C_eogr,s7_Ir_CatRaWp),
                                                          (s7_Ir_CatRaWp,I_wpa,s8_Iwim_CatRaWp),
                (s5_Ir_CaWp,I_wpa,s6_Iwim_CaWp),
                (s6_Iwim_CaWp,C_gr,s6_Iwim_CnrRaWp),
                (s6_Iwim_CnrRaWp,C_eogr,s8_Iwim_CatRaWp),

                (s8_Iwim_CatRaWp,C_drop,s8_Iwim_CdWp),
                (s8_Iwim_CdWp,C_eod,s8_Iwim_CatR),        (s8_Iwim_CatR,I_inject,s8_Iie_CatR),
                (s8_Iwim_CdWp,I_inject,s8_Iie_CdWp),

                (s8_Iwim_CatRaWp,I_inject,s8_Iie_CatRaWp),
                (s8_Iie_CatRaWp,C_drop,s8_Iie_CdWp),
                (s8_Iie_CatRaWp,I_eoi,s9_Ie_CatRaWp),     (s9_Ie_CatRaWp,C_drop,s9_Ie_CdWp),

                (s8_Iie_CdWp,C_eod,s8_Iie_CatR),
                (s8_Iie_CatR,I_eoi,s9_Ie_CatR),
                (s8_Iie_CdWp,I_eoi,s9_Ie_CdWp),
                (s9_Ie_CdWp,C_eod,s9_Ie_CatR),

                (s9_Ie_CatR,C_gl,s9_Ie_CnrL),
                (s9_Ie_CnrL,C_eogl,s10_Ie_CatL),
                (s10_Ie_CatL,reset,s3_Ie_CatL),

                (s7_Ir_CatRaWp,C_drop,s7_Ir_CdWp),
                (s7_Ir_CdWp,C_eod,s7_Ir_CatR),            (s7_Ir_CdWp,I_wpa,s8_Iwim_CdWp),
                (s7_Ir_CatR,C_gl,s11_Ir_CnrL),            (s7_Ir_CatR,I_wpa,s8_Iwim_CatR),
                (s11_Ir_CnrL,C_eogl,s13_Ir_CatL),         (s13_Ir_CatL,I_wpa,s14_Iwim_CatL),
                (s13_Ir_CatL,reset,s16_Ir_CatL),
                (s16_Ir_CatL,C_gr,s16_Ir_CnrR),           (s16_Ir_CatL,I_wpa,s15_Iwim_CatL),
                (s16_Ir_CnrR,C_eogr,s0_Ir_CatR),          (s16_Ir_CnrR,I_wpa,s15_Iwim_CnrR),

                (s11_Ir_CnrL,I_wpa,s12_Iwim_CnrL),
                (s12_Iwim_CnrL,C_eogl,s14_Iwim_CatL),
                (s14_Iwim_CatL,reset,s15_Iwim_CatL),
                (s15_Iwim_CatL,C_gr,s15_Iwim_CnrR),
                (s15_Iwim_CnrR,C_eogr,s1_Iwim_CatR) },
            x_0: s0_Ir_CatR,
            Xm: {s0_Ir_CatR, s16_Ir_CatL, s16_Ir_CnrR},
            ctrlEvents: {reset, I_inject, I_rearm, C_drop, C_gl, C_gr, C_grab} ) &
 bound_G_ts = 19
 & 
 G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,
           tf:{(s0_Ir_CatR,(I_wpa,wpa),s1_Iwim_CatR),
               (s1_Iwim_CatR,(I_inject,silentEvent),s1_Iie_CatR),
               (s1_Iie_CatR,(I_eoi,silentEvent),s2_Ie_CatR),
               (s2_Ie_CatR,(C_gl,silentEvent),s2_Ie_CnrL),
               (s2_Ie_CnrL,(C_eogl,silentEvent),s3_Ie_CatL),
               (s3_Ie_CatL,(C_grab,silentEvent),s3_Ie_CgWp),
               (s3_Ie_CgWp,(C_eog,silentEvent),s4_Ie_CaWp),
               (s4_Ie_CaWp,(I_rearm,silentEvent),s5_Ir_CaWp),

               (s5_Ir_CaWp,(C_gr,silentEvent),s5_Ir_CnrRaWp),
               (s5_Ir_CnrRaWp,(I_wpa,wpa),s6_Iwim_CnrRaWp),            (s5_Ir_CnrRaWp,(C_eogr,silentEvent),s7_Ir_CatRaWp),
                                                                       (s7_Ir_CatRaWp,(I_wpa,wpa),s8_Iwim_CatRaWp),
               (s5_Ir_CaWp,(I_wpa,wpa),s6_Iwim_CaWp),
               (s6_Iwim_CaWp,(C_gr,silentEvent),s6_Iwim_CnrRaWp),
               (s6_Iwim_CnrRaWp,(C_eogr,silentEvent),s8_Iwim_CatRaWp),

               (s8_Iwim_CatRaWp,(C_drop,silentEvent),s8_Iwim_CdWp),
               (s8_Iwim_CdWp,(C_eod,silentEvent),s8_Iwim_CatR),        (s8_Iwim_CatR,(I_inject,silentEvent),s8_Iie_CatR),
               (s8_Iwim_CdWp,(I_inject,silentEvent),s8_Iie_CdWp),

               (s8_Iwim_CatRaWp,(I_inject,silentEvent),s8_Iie_CatRaWp),
               (s8_Iie_CatRaWp,(I_eoi,silentEvent),s9_Ie_CatRaWp),     (s9_Ie_CatRaWp,(C_drop,silentEvent),s9_Ie_CdWp),
               (s8_Iie_CatRaWp,(C_drop,silentEvent),s8_Iie_CdWp),

               (s8_Iie_CdWp,(C_eod,silentEvent),s8_Iie_CatR),
               (s8_Iie_CatR,(I_eoi,silentEvent),s9_Ie_CatR),
               (s8_Iie_CdWp,(I_eoi,silentEvent),s9_Ie_CdWp),
               (s9_Ie_CdWp,(C_eod,silentEvent),s9_Ie_CatR),

               (s9_Ie_CatR,(C_gl,silentEvent),s9_Ie_CnrL),
               (s9_Ie_CnrL,(C_eogl,creogl),s10_Ie_CatL),
               (s10_Ie_CatL,(reset,release),s3_Ie_CatL),

               (s7_Ir_CatRaWp,(C_drop,silentEvent),s7_Ir_CdWp),
               (s7_Ir_CdWp,(C_eod,silentEvent),s7_Ir_CatR),            (s7_Ir_CdWp,(I_wpa,wpa),s8_Iwim_CdWp),
               (s7_Ir_CatR,(C_gl,silentEvent),s11_Ir_CnrL),            (s7_Ir_CatR,(I_wpa,wpa),s8_Iwim_CatR),
               (s11_Ir_CnrL,(C_eogl,creogl),s13_Ir_CatL),              (s13_Ir_CatL,(I_wpa,wpa),s14_Iwim_CatL),
               (s13_Ir_CatL,(reset,release),s16_Ir_CatL),
               (s16_Ir_CatL,(C_gr,silentEvent),s16_Ir_CnrR),           (s16_Ir_CatL,(I_wpa,wpa),s15_Iwim_CatL),
               (s16_Ir_CnrR,(C_eogr,silentEvent),s0_Ir_CatR),          (s16_Ir_CnrR,(I_wpa,wpa),s15_Iwim_CnrR),

               (s11_Ir_CnrL,(I_wpa,wpa),s12_Iwim_CnrL),
               (s12_Iwim_CnrL,(C_eogl,creogl),s14_Iwim_CatL),
               (s14_Iwim_CatL,(reset,release),s15_Iwim_CatL),
               (s15_Iwim_CatL,(C_gr,silentEvent),s15_Iwim_CnrR),
               (s15_Iwim_CnrR,(C_eogr,silentEvent),s1_Iwim_CatR) },
            x_0: s0_Ir_CatR,
            Xm: {s0_Ir_CatR, s16_Ir_CatL, s16_Ir_CnrR},
            ctrlEvents: {reset, I_inject, I_rearm, C_drop, C_gl, C_gr, C_grab} ) &
 bound_G_A = 19
 &
 G_I = rec(states: StatesG_I,
           events: Output - {silentEvent},
           tf: {(DcrR,wpa,Dm1stwp), (Dm1stwp,creogl,DcrL), (DcrL,release,DcrR),
                                                           (DcrL,wpa,DcrL1),       (DcrL1,release,Dm1stwp),
                                    (Dm1stwp,wpa,D2ndwp),  (D2ndwp,creogl,DcrL1) },
           x_0: DcrR,
           Xm: {DcrR},
           ctrlEvents: {release} ) &
 bound_G_I = 4
END

/*
OUTPUT:
See the outputs of LBdistribution_1 and LBdistribution_2.
*/