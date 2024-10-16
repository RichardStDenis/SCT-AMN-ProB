// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBmps_dt
/*
  The subsystem distribution and testing.
*/

SETS
/*
  first buffer states):
    b10 := the first buffer is empty
    b11 := the first buffer is full (contains one workpiece)
  distribution subsystem states:
        DcrR := crane at the right
        DcrL := crane at the left with one workpiece in the distribution station
        DcrL1 := crane at the left with two workpieces in the distribution station
      Dm1stwp := moving a first workpiece
       D2ndwp := a second workpiece in the distribution station
  testing subsystem states:
     Tidle := the testing subsystem is idle
       Tit := workpiece in transfer
    Tpause := the testing subsystem waits the result
       Trt := workpiece rejected or transferred
       Twt :=  waiting a transfer
*/
 StatesG_ts = {DcrR_b10_Tidle,Dm1stwp_b10_Tidle,DcrL_b11_Tidle,D2ndwp_b10_Tidle,
               DcrL_b11_Tpause,DcrL1_b11_Tidle,DcrL_b11_Trt,DcrL_b11_Twt,
               DcrL1_b11_Tpause,DcrL_b10_Tidle,DcrL1_b11_Trt,DcrL_b11_Tit,
               DcrL1_b11_Twt,DcrL1_b10_Tidle,DcrL1_b11_Tit};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <buffer1, distribution, testing>

    ControlLaw : G_ts'events --> POW(StateValues):
      ControlLaw(D_release) = b10 & Tidle
      ControlLaw(T_test) = b11 & Tidle
      ControlLaw(T_twp) = b11 & Twt
*/

/*
  Commands and signals:
    distribution subsystem events:
          D_wpa := workpiece arrival
      D_release := release the testing subsystem
       D_creogl := crane end of going to the left
    testing subsystem events:
        T_eot := end of testing
      T_eotwp := end of transfer of a workpiece
       T_fail := fail
       T_pass := pass
       T_test := test
        T_twp := transfer a workpiece
*/
 EventsG_ts = {D_wpa,D_release,D_creogl,T_eot,T_eotwp,T_fail,T_pass,T_test,T_twp}

CONSTANTS
 G_ts,        // G_ts the subsystem under control of a state feedback function
              // (solution of a control problem)
 bound_G_ts,  // Bound on the trace length of the controlled device behavior

 T_G_ts,           // Traces of the transition system
 L_G_ts, Lm_G_ts   // Generated and marked languages of the transition system

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 10000000;
 SET_PREF_SMT == FALSE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../../Theories/SCT_core.def";
 "LibraryIO.def"

PROPERTIES
 G_ts = rec(states : StatesG_ts,
            events: EventsG_ts,
            tf: {((DcrR_b10_Tidle|->D_wpa)|->Dm1stwp_b10_Tidle),            ((Dm1stwp_b10_Tidle|->D_wpa)|->D2ndwp_b10_Tidle),
                   ((Dm1stwp_b10_Tidle|->D_creogl)|->DcrL_b11_Tidle),       ((DcrL_b11_Tidle|->D_wpa)|->DcrL1_b11_Tidle),
                     ((DcrL_b11_Tidle|->T_test)|->DcrL_b11_Tpause),         ((DcrL_b11_Tpause|->D_wpa)|->DcrL1_b11_Tpause),
                       ((DcrL_b11_Tpause|->T_fail)|->DcrL_b11_Trt),         ((DcrL_b11_Trt|->D_wpa)|->DcrL1_b11_Trt),
                         ((DcrL_b11_Trt|->T_eot)|->DcrL_b10_Tidle),         ((DcrL_b10_Tidle|->D_wpa)|->DcrL1_b10_Tidle),
                           ((DcrL_b10_Tidle|->D_release)|->DcrR_b10_Tidle),
                       ((DcrL_b11_Tpause|->T_pass)|->DcrL_b11_Twt),         ((DcrL_b11_Twt|->D_wpa)|->DcrL1_b11_Twt),
                         ((DcrL_b11_Twt|->T_twp)|->DcrL_b11_Tit),           ((DcrL_b11_Tit|->D_wpa)|->DcrL1_b11_Tit),
                           ((DcrL_b11_Tit|->T_eotwp)|->DcrL_b11_Trt),

                                                                         ((D2ndwp_b10_Tidle|->D_creogl)|->DcrL1_b11_Tidle),
                                                                          ((DcrL1_b11_Tidle|->T_test)|->DcrL1_b11_Tpause),
                                                                            ((DcrL1_b11_Tpause|->T_fail)|->DcrL1_b11_Trt),
                                                                              ((DcrL1_b11_Trt|->T_eot)|->DcrL1_b10_Tidle),
                                                                                ((DcrL1_b10_Tidle|->D_release)|->Dm1stwp_b10_Tidle),
                                                                            ((DcrL1_b11_Tpause|->T_pass)|->DcrL1_b11_Twt),
                                                                              ((DcrL1_b11_Twt|->T_twp)|->DcrL1_b11_Tit),
                                                                                ((DcrL1_b11_Tit|->T_eotwp)|->DcrL1_b11_Trt)},
            x_0: DcrR_b10_Tidle,
            Xm: {DcrR_b10_Tidle},
            ctrlEvents: {D_release,T_test,T_twp} ) &
 bound_G_ts = 10
 &
 T_G_ts = GetTracesNc1st(G_ts,bound_G_ts) &
 L_G_ts = Traces2Strings(T_G_ts) &
 Lm_G_ts = Traces2Tasks(T_G_ts,G_ts'Xm)
END