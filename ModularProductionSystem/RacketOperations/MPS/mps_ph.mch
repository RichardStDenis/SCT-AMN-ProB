// copyright Richard St_Denis, Universite de Sherbrooke, 2023.
MODEL SBmps_ph

SETS
 States = {Pready_b30_Hidle, Punready_b30_Hidle, Pready_b30_Hbusy,
           Pready_b31_Hidle, Punready_b30_Hbusy, Punready_b31_Hidle,
           Pready_b31_Hbusy, Punready_b31_Hbusy };
 Events = {P_open, P_eoc, H_stop, H_start};
 LocalStates = {Pready, Punready, b30, b31, Hbusy, Hidle}

CONSTANTS
 G_FB,
 processing, buffer3, handling,
 Qpred, BadStates

PROPERTIES
 G_FB = rec(       // Automaton modeling the free behavior
            states: States,
            events: Events,
            tf: {(Pready_b30_Hidle,P_open,Punready_b30_Hidle), (Pready_b30_Hidle,H_start,Pready_b30_Hbusy), (Punready_b30_Hidle,P_eoc,Pready_b31_Hidle),
                 (Punready_b30_Hidle,H_start,Punready_b30_Hbusy), (Pready_b30_Hbusy,P_open,Punready_b30_Hbusy), (Pready_b30_Hbusy,H_stop,Pready_b30_Hidle),
                 (Pready_b31_Hidle,P_open,Punready_b31_Hidle), (Pready_b31_Hidle,H_start,Pready_b31_Hbusy), (Punready_b30_Hbusy,P_eoc,Pready_b31_Hbusy),
                 (Punready_b30_Hbusy,H_stop,Punready_b30_Hidle), (Punready_b31_Hidle,H_start,Punready_b31_Hbusy), (Punready_b31_Hidle,P_eoc,Pready_b31_Hidle),
                 (Pready_b31_Hbusy,P_open,Punready_b31_Hbusy), (Pready_b31_Hbusy,H_stop,Pready_b30_Hidle), (Punready_b31_Hbusy,H_stop,Punready_b30_Hidle),
                 (Punready_b31_Hbusy,P_eoc,Pready_b31_Hbusy) },
            x_0: Pready_b30_Hidle,
            Xm: {Pready_b30_Hidle},
            ctrlEvents: {P_open, H_start} )
 &
 processing = {
      (Pready_b30_Hidle,Pready),(Punready_b30_Hidle,Punready),(Pready_b30_Hbusy,Pready),
      (Pready_b31_Hidle,Pready),(Punready_b30_Hbusy,Punready),(Punready_b31_Hidle,Punready),
      (Pready_b31_Hbusy,Pready),(Punready_b31_Hbusy,Punready) }
 &
 buffer3 = {
      (Pready_b30_Hidle,b30),(Punready_b30_Hidle,b30),(Pready_b30_Hbusy,b30),
      (Pready_b31_Hidle,b31),(Punready_b30_Hbusy,b30),(Punready_b31_Hidle,b31),
      (Pready_b31_Hbusy,b31),(Punready_b31_Hbusy,b31) }
 &
 handling = {
      (Pready_b30_Hidle,Hidle),(Punready_b30_Hidle,Hidle),(Pready_b30_Hbusy,Hbusy),
      (Pready_b31_Hidle,Hidle),(Punready_b30_Hbusy,Hbusy),(Punready_b31_Hidle,Hidle),
      (Pready_b31_Hbusy,Hbusy),(Punready_b31_Hbusy,Hbusy) }
 &
 BadStates =  States    // to be completed
 &
 Qpred = States - BadStates
END