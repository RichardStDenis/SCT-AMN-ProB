// copyright Richard St_Denis, Universite de Sherbrooke, 2023.
MODEL SBservomotor

SETS
 States = {rfm123_Ti, im123_Ti, rfm123_Tip,
           im123_Tip, rfm0_Trtc, im0_Trtc,
           rfm0_Ti, im0_Ti, rfm0_Tip,
           im0_Tip, rfm123_Trtc, im123_Trtc };
 Events = {mt0, mt1, mt2, mt3, T_exp, T_clr, T_set};
 LocalStates = {im0, im123, rfm0, rfm123, Ti, Tip, Trtc}

CONSTANTS
 G_FB,
 seqSM, timer,
 Qpred, BadStates

PROPERTIES
 G_FB = rec(       // Automaton modeling the free behavior
            states: States,
            events: Events,
            tf: {(rfm123_Ti,mt1,im123_Ti), (rfm123_Ti,T_set,rfm123_Tip), (rfm123_Ti,mt2,im123_Ti),
                 (rfm123_Ti,mt3,im123_Ti), (im123_Ti,T_set,im123_Tip), (rfm123_Tip,mt1,im123_Tip),
                 (rfm123_Tip,mt2,im123_Tip), (rfm123_Tip,mt3,im123_Tip), (im123_Tip,T_exp,rfm0_Trtc),
                 (rfm0_Trtc,mt0,im0_Trtc), (rfm0_Trtc,T_clr,rfm0_Ti), (rfm0_Ti,mt0,im0_Ti),
                 (rfm0_Ti,T_set,rfm0_Tip), (im0_Ti,T_set,im0_Tip), (rfm0_Tip,mt0,im0_Tip),
                 (im0_Tip,T_exp,rfm123_Trtc), (rfm123_Trtc,mt1,im123_Trtc), (rfm123_Trtc,mt2,im123_Trtc),
                 (rfm123_Trtc,mt3,im123_Trtc), (rfm123_Trtc,T_clr,rfm123_Ti) },
            x_0: rfm123_Ti,
            Xm: {rfm123_Ti},
            ctrlEvents: {mt0, mt1, mt2, mt3, T_clr, T_set} )
 &
 seqSM = {
      (rfm123_Ti,rfm123),(im123_Ti,im123),(rfm123_Tip,rfm123),
      (im123_Tip,im123),(rfm0_Trtc,rfm0),(im0_Trtc,im0),
      (rfm0_Ti,rfm0),(im0_Ti,im0),(rfm0_Tip,rfm0),
      (im0_Tip,im0),(rfm123_Trtc,rfm123),(im123_Trtc,im123) }
 &
 timer = {
      (rfm123_Ti,Ti),(im123_Ti,Ti),(rfm123_Tip,Tip),
      (im123_Tip,Tip),(rfm0_Trtc,Trtc),(im0_Trtc,Trtc),
      (rfm0_Ti,Ti),(im0_Ti,Ti),(rfm0_Tip,Tip),
      (im0_Tip,Tip),(rfm123_Trtc,Trtc),(im123_Trtc,Trtc) }
 &
 BadStates =  States    // to be completed
 &
 Qpred = States - BadStates
END