// copyright Richard St_Denis, Universite de Sherbrooke, 2023.
MODEL SBtester

SETS
 States = {Pr_Ti, Pie_Ti, Pe_Ti,
           Pie_Tip, Pir_Ti, Pe_Tip,
           Pie_Trtc, Pir_Tip, Pe_Trtc,
           Pir_Trtc, Pr_Tip, Pr_Trtc };
 Events = {P_cd, P_eod, P_eoa, P_gd, P_gu, T_clr, T_rst, T_set, T_exp};
 LocalStates = {Pe, Pie, Pir, Pr, Ti, Tip, Trtc}

CONSTANTS
 G_FB,
 probe, timer,
 Qpred, BadStates

PROPERTIES
 G_FB = rec(       // Automaton modeling the free behavior
            states: States,
            events: Events,
            tf: {(Pr_Ti,P_gd,Pie_Ti), (Pie_Ti,P_eod,Pe_Ti), (Pie_Ti,T_set,Pie_Tip),
                 (Pe_Ti,P_gu,Pir_Ti), (Pie_Tip,P_eod,Pe_Tip), (Pie_Tip,T_exp,Pie_Trtc),
                 (Pir_Ti,P_eoa,Pr_Ti), (Pe_Tip,P_gu,Pir_Tip), (Pe_Tip,T_exp,Pe_Trtc),
                 (Pe_Tip,T_rst,Pe_Ti), (Pie_Trtc,P_eod,Pe_Trtc), (Pie_Trtc,P_cd,Pir_Trtc),
                 (Pir_Tip,P_eoa,Pr_Tip), (Pir_Tip,T_exp,Pir_Trtc), (Pe_Trtc,P_gu,Pir_Trtc),
                 (Pe_Trtc,T_clr,Pe_Ti), (Pir_Trtc,P_eoa,Pr_Trtc), (Pir_Trtc,T_clr,Pir_Ti),
                 (Pr_Tip,T_exp,Pr_Trtc), (Pr_Trtc,T_clr,Pr_Ti) },
            x_0: Pr_Ti,
            Xm: {Pr_Ti},
            ctrlEvents: {P_cd, P_gd, P_gu, T_clr, T_rst, T_set} )
 &
 probe = {
      (Pr_Ti,Pr),(Pie_Ti,Pie),(Pe_Ti,Pe),
      (Pie_Tip,Pie),(Pir_Ti,Pir),(Pe_Tip,Pe),
      (Pie_Trtc,Pie),(Pir_Tip,Pir),(Pe_Trtc,Pe),
      (Pir_Trtc,Pir),(Pr_Tip,Pr),(Pr_Trtc,Pr) }
 &
 timer = {
      (Pr_Ti,Ti),(Pie_Ti,Ti),(Pe_Ti,Ti),
      (Pie_Tip,Tip),(Pir_Ti,Ti),(Pe_Tip,Tip),
      (Pie_Trtc,Trtc),(Pir_Tip,Tip),(Pe_Trtc,Trtc),
      (Pir_Trtc,Trtc),(Pr_Tip,Tip),(Pr_Trtc,Trtc) }
 &
 BadStates =  States    // to be completed
 &
 Qpred = States - BadStates
END