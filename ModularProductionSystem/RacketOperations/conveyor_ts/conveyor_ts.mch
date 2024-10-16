// copyright Richard St_Denis, Universite de Sherbrooke, 2023.
MODEL SBconveyor_ts

SETS
 States = {p1_Mnm_Trtc, p1_Mim_Trtc, p1_Mnm_Ti,
           p2_Mnm_Trtc, p1_Mim_Ti, p1_Mnm_Tip,
           p2_Mnm_Ti, p2_Mim_Trtc, p1_Mim_Tip,
           p2_Mnm_Tip, p2_Mim_Ti, p2_Mim_Tip };
 Events = {M_eos, T_exp, M_step, T_clr, T_set};
 LocalStates = {p1, p2, Mim, Mnm, Ti, Tip, Trtc}

CONSTANTS
 G_FB,
 seqC, motor, timer,
 Qpred, BadStates

PROPERTIES
 G_FB = rec(       // Automaton modeling the free behavior
            states: States,
            events: Events,
            tf: {(p1_Mnm_Trtc,M_step,p1_Mim_Trtc), (p1_Mnm_Trtc,T_clr,p1_Mnm_Ti), (p1_Mim_Trtc,M_eos,p2_Mnm_Trtc),
                 (p1_Mim_Trtc,T_clr,p1_Mim_Ti), (p1_Mnm_Ti,M_step,p1_Mim_Ti), (p1_Mnm_Ti,T_set,p1_Mnm_Tip),
                 (p2_Mnm_Trtc,T_clr,p2_Mnm_Ti), (p2_Mnm_Trtc,M_step,p2_Mim_Trtc), (p1_Mim_Ti,M_eos,p2_Mnm_Ti),
                 (p1_Mim_Ti,T_set,p1_Mim_Tip), (p1_Mnm_Tip,M_step,p1_Mim_Tip), (p1_Mnm_Tip,T_exp,p1_Mnm_Trtc),
                 (p2_Mnm_Ti,T_set,p2_Mnm_Tip), (p2_Mnm_Ti,M_step,p2_Mim_Ti), (p2_Mim_Trtc,T_clr,p2_Mim_Ti),
                 (p2_Mim_Trtc,M_eos,p2_Mnm_Trtc), (p1_Mim_Tip,M_eos,p2_Mnm_Tip), (p1_Mim_Tip,T_exp,p1_Mim_Trtc),
                 (p2_Mnm_Tip,T_exp,p1_Mnm_Trtc), (p2_Mnm_Tip,M_step,p2_Mim_Tip), (p2_Mim_Ti,T_set,p2_Mim_Tip),
                 (p2_Mim_Ti,M_eos,p2_Mnm_Ti), (p2_Mim_Tip,T_exp,p1_Mim_Trtc), (p2_Mim_Tip,M_eos,p2_Mnm_Tip) },
            x_0: p1_Mnm_Trtc,
            Xm: {p1_Mnm_Trtc},
            ctrlEvents: {M_step, T_clr, T_set} )
 &
 seqC = {
      (p1_Mnm_Trtc,p1),(p1_Mim_Trtc,p1),(p1_Mnm_Ti,p1),
      (p2_Mnm_Trtc,p2),(p1_Mim_Ti,p1),(p1_Mnm_Tip,p1),
      (p2_Mnm_Ti,p2),(p2_Mim_Trtc,p2),(p1_Mim_Tip,p1),
      (p2_Mnm_Tip,p2),(p2_Mim_Ti,p2),(p2_Mim_Tip,p2) }
 &
 motor = {
      (p1_Mnm_Trtc,Mnm),(p1_Mim_Trtc,Mim),(p1_Mnm_Ti,Mnm),
      (p2_Mnm_Trtc,Mnm),(p1_Mim_Ti,Mim),(p1_Mnm_Tip,Mnm),
      (p2_Mnm_Ti,Mnm),(p2_Mim_Trtc,Mim),(p1_Mim_Tip,Mim),
      (p2_Mnm_Tip,Mnm),(p2_Mim_Ti,Mim),(p2_Mim_Tip,Mim) }
 &
 timer = {
      (p1_Mnm_Trtc,Trtc),(p1_Mim_Trtc,Trtc),(p1_Mnm_Ti,Ti),
      (p2_Mnm_Trtc,Trtc),(p1_Mim_Ti,Ti),(p1_Mnm_Tip,Tip),
      (p2_Mnm_Ti,Ti),(p2_Mim_Trtc,Trtc),(p1_Mim_Tip,Tip),
      (p2_Mnm_Tip,Tip),(p2_Mim_Ti,Ti),(p2_Mim_Tip,Tip) }
 &
 BadStates =  States    // to be completed
 &
 Qpred = States - BadStates
END