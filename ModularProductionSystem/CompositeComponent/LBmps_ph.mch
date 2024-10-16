// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBmps_ph
/*
  The subsystem processing and handling.
*/

SETS
/*
  third buffer states):
    b30 := the third buffer is empty
    b31 := the third buffer is full (contains one workpiece)
  processing subsystem states:
      Pready := the processing subsystem is ready to accept a workpiece
    Punready := the subsystem is unready to accept a workpiece
  handling subsystem states:
    Hidle := the handling subsystem is idle
    Hbusy := the handling subsystem is busy
*/
 StatesG_ts = {Pready_b30_Hidle,Punready_b30_Hidle,
               Pready_b31_Hidle,Pready_b31_Hbusy};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <buffer3, processing, handling>

    ControlLaw : G_ts'events --> POW(StateValues):
      ControlLaw(P_open) = Pready & b30 &_Hidle
      ControlLaw(H_start) = Pready & b31 & Hidle
*/

/*
  Commands and signals:
    processing subsystem events:
       P_eoc := end of closing the access to the entry position on the rotary table
      P_open := open the access to the entry position on the rotary table
    handling subsystem events:
      H_start := start the activities of the handling subsystem
       H_stop := stop the activities of the handling subsystem
*/
 EventsG_ts = {P_eoc,P_open,H_start,H_stop}

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
            tf: {((Pready_b30_Hidle|->P_open)|->Punready_b30_Hidle),
                 ((Punready_b30_Hidle|->P_eoc)|->Pready_b31_Hidle),
                 ((Pready_b31_Hidle|->H_start)|->Pready_b31_Hbusy),
                 ((Pready_b31_Hbusy|->H_stop)|->Pready_b30_Hidle) },
            x_0:Pready_b30_Hidle,
            Xm:{Pready_b30_Hidle},
            ctrlEvents:{P_open,H_start} ) &
 bound_G_ts = 4
 &
 T_G_ts = GetTracesNc1st(G_ts,bound_G_ts) &
 L_G_ts = Traces2Strings(T_G_ts) &
 Lm_G_ts = Traces2Tasks(T_G_ts,G_ts'Xm)
END