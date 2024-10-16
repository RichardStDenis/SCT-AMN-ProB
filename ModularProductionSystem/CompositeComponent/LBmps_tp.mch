// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBmps_tp
/*
  The subsystem testing and processing.
*/

SETS
/*
  second buffer states):
    b20 := the second buffer is empty
    b21 := the second buffer is full (contains one workpiece)
  testing subsystem states:
     Tidle := the testing subsystem is idle
       Tit := workpiece in transfer
    Tpause := the testing subsystem waits the result
       Trt := workpiece rejected or transferred
       Twt :=  waiting a transfer
  processing subsystem states:
      Pready := the processing subsystem is ready to accept a workpiece
    Punready := the processing subsystem is unready to accept a workpiece
*/
 StatesG_ts = {Tidle_b20_Pready,Tpause_b20_Pready,Trt_b20_Pready,
               Twt_b20_Pready,
               Tit_b20_Pready,Trt_b21_Pready,Trt_b21_Punready,Tidle_b21_Pready,
               Tidle_b21_Punready,Tpause_b21_Pready,Tpause_b21_Punready,Twt_b21_Pready,
               Twt_b21_Punready };

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <buffer2, testing, processing>

    ControlLaw : G_ts'events --> POW(StateValues):
      ControlLaw(P_open) = Pready & b21
      ControlLaw(T_test) = Tidle
      ControlLaw(T_twp) = Twt & b20 & Pready
*/

/*
  Commands and signals:
    testing subsystem events:
        T_eot := end of testing
      T_eotwp := end of transfer of a workpiece 
       T_fail := fail
       T_pass := pass
       T_test := test
        T_twp := transfer a workpiece
    processing subsystem events:
      P_eoc := end of closing the access to the entry position on the rotary table
      P_open := open the access to the entry position on the rotary table
*/
 EventsG_ts = {T_eot,T_eotwp,T_fail,T_pass,T_test,T_twp,P_eoc,P_open}

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
            tf: {((Tidle_b20_Pready|->T_test)|->Tpause_b20_Pready),
                   ((Tpause_b20_Pready|->T_fail)|->Trt_b20_Pready),
                     ((Trt_b20_Pready|->T_eot)|->Tidle_b20_Pready),
                   ((Tpause_b20_Pready|->T_pass)|->Twt_b20_Pready),
                    ((Twt_b20_Pready|->T_twp)|->Tit_b20_Pready),
                       ((Tit_b20_Pready|->T_eotwp)|->Trt_b21_Pready),
                         ((Trt_b21_Pready|->T_eot)|->Tidle_b21_Pready),
                           ((Tidle_b21_Pready|->P_open)|->Tidle_b21_Punready),
                             ((Tidle_b21_Punready|->P_eoc)|->Tidle_b20_Pready),

                             ((Tidle_b21_Punready|->T_test)|->Tpause_b21_Punready),
                               ((Tpause_b21_Punready|->P_eoc)|->Tpause_b20_Pready),
                               ((Tpause_b21_Punready|->T_fail)|->Trt_b21_Punready),
                               ((Tpause_b21_Punready|->T_pass)|->Twt_b21_Punready),
                                 ((Twt_b21_Punready|->P_eoc)|->Twt_b20_Pready),

                         ((Trt_b21_Pready|->P_open)|->Trt_b21_Punready),
                           ((Trt_b21_Punready|->P_eoc)|->Trt_b20_Pready),
                           ((Trt_b21_Punready|->T_eot)|->Tidle_b21_Punready), 

                           ((Tidle_b21_Pready|->T_test)|->Tpause_b21_Pready),
                             ((Tpause_b21_Pready|->P_open)|->Tpause_b21_Punready),
                             ((Tpause_b21_Pready|->T_fail)|->Trt_b21_Pready),
                             ((Tpause_b21_Pready|->T_pass)|->Twt_b21_Pready),
                              ((Twt_b21_Pready|->P_open)|->Twt_b21_Punready) },
            x_0: Tidle_b20_Pready,
            Xm: {Tidle_b20_Pready},
            ctrlEvents: {T_test,T_twp,P_open} ) &
 bound_G_ts = 9
 &
 T_G_ts = GetTracesNc1st(G_ts,bound_G_ts) &
 L_G_ts = Traces2Strings(T_G_ts) &
 Lm_G_ts = Traces2Tasks(T_G_ts,G_ts'Xm)
END