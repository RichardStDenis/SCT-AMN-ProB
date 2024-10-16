// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBhandling

SETS
 States = {Snpt_nwp_nwp_nwp_Cw, Sp1t_nwp_nwp_nwp_Cw, Snpt_nwp_nwp_nwp_Cp3,
           Snpt_nwp_nwp_nwp_Cp2, Snpt_nwp_nwp_nwp_Cp1, Snpt_awp_nwp_nwp_Cw,
           Sp1t_nwp_nwp_nwp_Cp3, Sp1t_nwp_nwp_nwp_Cp2, Sp1t_nwp_nwp_nwp_Cp1,
           Sp1t_awp_nwp_nwp_Cw, Snpt_awp_nwp_nwp_Cp3, Snpt_awp_nwp_nwp_Cp2,
           Snpt_awp_nwp_nwp_Cp1, Sp2t_awp_nwp_nwp_Cw, Sp1t_awp_nwp_nwp_Cp3,
           Sp1t_awp_nwp_nwp_Cp2, Sp1t_awp_nwp_nwp_Cp1, Sp2t_awp_nwp_nwp_Cp3,
           Sp2t_awp_nwp_nwp_Cp2, Sp2t_awp_nwp_nwp_Cp1, Snpt_awp_awp_nwp_Cw,
           Snpt_awp_awp_nwp_Cp3, Snpt_awp_awp_nwp_Cp2, Snpt_awp_awp_nwp_Cp1,
           Sp2t_nwp_nwp_nwp_Cw, Sp1t_awp_awp_nwp_Cw, Sp2t_awp_awp_nwp_Cw,
           Sp3t_awp_awp_nwp_Cw, Sp1t_awp_awp_nwp_Cp3, Sp2t_awp_awp_nwp_Cp3,
           Sp3t_awp_awp_nwp_Cp3, Sp1t_awp_awp_nwp_Cp2, Sp2t_awp_awp_nwp_Cp2,
           Sp3t_awp_awp_nwp_Cp2, Sp1t_awp_awp_nwp_Cp1, Snpt_nwp_awp_nwp_Cw,
           Sp2t_awp_awp_nwp_Cp1, Sp3t_awp_awp_nwp_Cp1, Sp2t_nwp_nwp_nwp_Cp3,
           Sp2t_nwp_nwp_nwp_Cp2, Sp2t_nwp_nwp_nwp_Cp1, Snpt_awp_awp_awp_Cw,
           Snpt_awp_awp_awp_Cp3, Snpt_awp_awp_awp_Cp2, Sp3t_awp_nwp_nwp_Cw,
           Sp1t_nwp_awp_nwp_Cw, Snpt_nwp_awp_nwp_Cp3, Snpt_nwp_awp_nwp_Cp2,
           Sp2t_nwp_awp_nwp_Cw, Snpt_awp_awp_awp_Cp1, Sp3t_nwp_awp_nwp_Cw,
           Snpt_nwp_awp_nwp_Cp1, Sp1t_awp_awp_awp_Cw, Sp2t_awp_awp_awp_Cw,
           Sp3t_awp_awp_awp_Cw, Sp1t_awp_awp_awp_Cp3, Sp2t_awp_awp_awp_Cp3,
           Sp3t_awp_awp_awp_Cp3, Sp1t_awp_awp_awp_Cp2, Snpt_awp_nwp_awp_Cw,
           Sp2t_awp_awp_awp_Cp2, Sp3t_awp_awp_awp_Cp2, Sp3t_awp_nwp_nwp_Cp3,
           Sp3t_awp_nwp_nwp_Cp2, Sp3t_awp_nwp_nwp_Cp1, Sp1t_nwp_awp_nwp_Cp3,
           Sp1t_nwp_awp_nwp_Cp2, Sp2t_nwp_awp_nwp_Cp3, Sp2t_nwp_awp_nwp_Cp2,
           Sp1t_awp_awp_awp_Cp1, Snpt_nwp_awp_awp_Cw, Sp2t_awp_awp_awp_Cp1,
           Sp3t_awp_awp_awp_Cp1, Sp3t_nwp_awp_nwp_Cp3, Sp3t_nwp_awp_nwp_Cp2,
           Sp1t_nwp_awp_nwp_Cp1, Sp1t_awp_nwp_awp_Cw, Snpt_awp_nwp_awp_Cp3,
           Sp2t_awp_nwp_awp_Cw, Sp3t_awp_nwp_awp_Cw, Snpt_awp_nwp_awp_Cp2,
           Snpt_awp_nwp_awp_Cp1, Sp3t_nwp_nwp_nwp_Cw, Sp1t_nwp_awp_awp_Cw,
           Snpt_nwp_awp_awp_Cp3, Sp2t_nwp_awp_awp_Cw, Sp3t_nwp_awp_awp_Cw,
           Snpt_nwp_awp_awp_Cp2, Sp1t_awp_nwp_awp_Cp3, Sp2t_awp_nwp_awp_Cp3,
           Sp3t_awp_nwp_awp_Cp3, Sp1t_awp_nwp_awp_Cp2, Sp2t_awp_nwp_awp_Cp2,
           Sp1t_awp_nwp_awp_Cp1, Snpt_nwp_nwp_awp_Cw, Sp2t_awp_nwp_awp_Cp1,
           Sp3t_nwp_nwp_nwp_Cp3, Sp3t_nwp_nwp_nwp_Cp2, Sp3t_nwp_nwp_nwp_Cp1,
           Sp1t_nwp_awp_awp_Cp3, Sp2t_nwp_awp_awp_Cp3, Sp3t_nwp_awp_awp_Cp3,
           Sp1t_nwp_awp_awp_Cp2, Sp1t_nwp_nwp_awp_Cw, Snpt_nwp_nwp_awp_Cp3,
           Sp2t_nwp_nwp_awp_Cw, Snpt_nwp_nwp_awp_Cp2, Snpt_nwp_nwp_awp_Cp1,
           Sp1t_nwp_nwp_awp_Cp3, Sp2t_nwp_nwp_awp_Cp3, Sp1t_nwp_nwp_awp_Cp2,
           Sp1t_nwp_nwp_awp_Cp1 };
 Events = {S_mp1, S_mp2, S_mp3, S_pap1, S_pap2, S_pap3, C_eop1, C_eop2, C_pck1, C_eop3, C_pck2, C_pck3};
 LocalStates = {Snpt, Sp1t, Sp2t, Sp3t, nwp, awp, Cp1, Cp2, Cp3, Cw}

CONSTANTS
 G_FB,
 sorter, buffer1, buffer2, buffer3, crane,
 Qpred, BadStates

PROPERTIES
 G_FB =rec(   // Automaton modeling the free behavior
      states: States,
      events: Events,
      tf: {(Snpt_nwp_nwp_nwp_Cw,S_mp1,Sp1t_nwp_nwp_nwp_Cw), (Snpt_nwp_nwp_nwp_Cw,C_pck3,Snpt_nwp_nwp_nwp_Cp3), (Snpt_nwp_nwp_nwp_Cw,C_pck2,Snpt_nwp_nwp_nwp_Cp2),
           (Snpt_nwp_nwp_nwp_Cw,C_pck1,Snpt_nwp_nwp_nwp_Cp1), (Sp1t_nwp_nwp_nwp_Cw,S_pap1,Snpt_awp_nwp_nwp_Cw), (Sp1t_nwp_nwp_nwp_Cw,C_pck3,Sp1t_nwp_nwp_nwp_Cp3),
           (Sp1t_nwp_nwp_nwp_Cw,C_pck2,Sp1t_nwp_nwp_nwp_Cp2), (Sp1t_nwp_nwp_nwp_Cw,C_pck1,Sp1t_nwp_nwp_nwp_Cp1), (Snpt_nwp_nwp_nwp_Cp3,S_mp1,Sp1t_nwp_nwp_nwp_Cp3),
           (Snpt_nwp_nwp_nwp_Cp2,S_mp1,Sp1t_nwp_nwp_nwp_Cp2), (Snpt_nwp_nwp_nwp_Cp1,S_mp1,Sp1t_nwp_nwp_nwp_Cp1), (Snpt_awp_nwp_nwp_Cw,S_mp1,Sp1t_awp_nwp_nwp_Cw),
           (Snpt_awp_nwp_nwp_Cw,C_pck3,Snpt_awp_nwp_nwp_Cp3), (Snpt_awp_nwp_nwp_Cw,C_pck2,Snpt_awp_nwp_nwp_Cp2), (Snpt_awp_nwp_nwp_Cw,C_pck1,Snpt_awp_nwp_nwp_Cp1),
           (Snpt_awp_nwp_nwp_Cw,S_mp2,Sp2t_awp_nwp_nwp_Cw), (Sp1t_nwp_nwp_nwp_Cp3,S_pap1,Snpt_awp_nwp_nwp_Cp3), (Sp1t_nwp_nwp_nwp_Cp2,S_pap1,Snpt_awp_nwp_nwp_Cp2),
           (Sp1t_nwp_nwp_nwp_Cp1,S_pap1,Snpt_awp_nwp_nwp_Cp1), (Sp1t_awp_nwp_nwp_Cw,C_pck3,Sp1t_awp_nwp_nwp_Cp3), (Sp1t_awp_nwp_nwp_Cw,C_pck2,Sp1t_awp_nwp_nwp_Cp2),
           (Sp1t_awp_nwp_nwp_Cw,C_pck1,Sp1t_awp_nwp_nwp_Cp1), (Snpt_awp_nwp_nwp_Cp3,S_mp1,Sp1t_awp_nwp_nwp_Cp3), (Snpt_awp_nwp_nwp_Cp3,S_mp2,Sp2t_awp_nwp_nwp_Cp3),
           (Snpt_awp_nwp_nwp_Cp2,S_mp1,Sp1t_awp_nwp_nwp_Cp2), (Snpt_awp_nwp_nwp_Cp2,S_mp2,Sp2t_awp_nwp_nwp_Cp2), (Snpt_awp_nwp_nwp_Cp1,S_mp1,Sp1t_awp_nwp_nwp_Cp1),
           (Snpt_awp_nwp_nwp_Cp1,C_eop1,Snpt_nwp_nwp_nwp_Cw), (Snpt_awp_nwp_nwp_Cp1,S_mp2,Sp2t_awp_nwp_nwp_Cp1), (Sp2t_awp_nwp_nwp_Cw,S_pap2,Snpt_awp_awp_nwp_Cw),
           (Sp2t_awp_nwp_nwp_Cw,C_pck3,Sp2t_awp_nwp_nwp_Cp3), (Sp2t_awp_nwp_nwp_Cw,C_pck2,Sp2t_awp_nwp_nwp_Cp2), (Sp2t_awp_nwp_nwp_Cw,C_pck1,Sp2t_awp_nwp_nwp_Cp1),
           (Sp1t_awp_nwp_nwp_Cp1,C_eop1,Sp1t_nwp_nwp_nwp_Cw), (Sp2t_awp_nwp_nwp_Cp3,S_pap2,Snpt_awp_awp_nwp_Cp3), (Sp2t_awp_nwp_nwp_Cp2,S_pap2,Snpt_awp_awp_nwp_Cp2),
           (Sp2t_awp_nwp_nwp_Cp1,S_pap2,Snpt_awp_awp_nwp_Cp1), (Sp2t_awp_nwp_nwp_Cp1,C_eop1,Sp2t_nwp_nwp_nwp_Cw), (Snpt_awp_awp_nwp_Cw,S_mp1,Sp1t_awp_awp_nwp_Cw),
           (Snpt_awp_awp_nwp_Cw,C_pck3,Snpt_awp_awp_nwp_Cp3), (Snpt_awp_awp_nwp_Cw,C_pck2,Snpt_awp_awp_nwp_Cp2), (Snpt_awp_awp_nwp_Cw,S_mp2,Sp2t_awp_awp_nwp_Cw),
           (Snpt_awp_awp_nwp_Cw,S_mp3,Sp3t_awp_awp_nwp_Cw), (Snpt_awp_awp_nwp_Cp3,S_mp1,Sp1t_awp_awp_nwp_Cp3), (Snpt_awp_awp_nwp_Cp3,S_mp2,Sp2t_awp_awp_nwp_Cp3),
           (Snpt_awp_awp_nwp_Cp3,S_mp3,Sp3t_awp_awp_nwp_Cp3), (Snpt_awp_awp_nwp_Cp2,S_mp1,Sp1t_awp_awp_nwp_Cp2), (Snpt_awp_awp_nwp_Cp2,C_eop2,Snpt_awp_nwp_nwp_Cw),
           (Snpt_awp_awp_nwp_Cp2,S_mp2,Sp2t_awp_awp_nwp_Cp2), (Snpt_awp_awp_nwp_Cp2,S_mp3,Sp3t_awp_awp_nwp_Cp2), (Snpt_awp_awp_nwp_Cp1,S_mp1,Sp1t_awp_awp_nwp_Cp1),
           (Snpt_awp_awp_nwp_Cp1,C_eop1,Snpt_nwp_awp_nwp_Cw), (Snpt_awp_awp_nwp_Cp1,S_mp2,Sp2t_awp_awp_nwp_Cp1), (Snpt_awp_awp_nwp_Cp1,S_mp3,Sp3t_awp_awp_nwp_Cp1),
           (Sp2t_nwp_nwp_nwp_Cw,S_pap2,Snpt_nwp_awp_nwp_Cw), (Sp2t_nwp_nwp_nwp_Cw,C_pck3,Sp2t_nwp_nwp_nwp_Cp3), (Sp2t_nwp_nwp_nwp_Cw,C_pck2,Sp2t_nwp_nwp_nwp_Cp2),
           (Sp2t_nwp_nwp_nwp_Cw,C_pck1,Sp2t_nwp_nwp_nwp_Cp1), (Sp1t_awp_awp_nwp_Cw,C_pck3,Sp1t_awp_awp_nwp_Cp3), (Sp1t_awp_awp_nwp_Cw,C_pck2,Sp1t_awp_awp_nwp_Cp2),
           (Sp2t_awp_awp_nwp_Cw,C_pck3,Sp2t_awp_awp_nwp_Cp3), (Sp2t_awp_awp_nwp_Cw,C_pck2,Sp2t_awp_awp_nwp_Cp2), (Sp3t_awp_awp_nwp_Cw,S_pap3,Snpt_awp_awp_awp_Cw),
           (Sp3t_awp_awp_nwp_Cw,C_pck3,Sp3t_awp_awp_nwp_Cp3), (Sp3t_awp_awp_nwp_Cw,C_pck2,Sp3t_awp_awp_nwp_Cp2), (Sp3t_awp_awp_nwp_Cp3,S_pap3,Snpt_awp_awp_awp_Cp3),
           (Sp1t_awp_awp_nwp_Cp2,C_eop2,Sp1t_awp_nwp_nwp_Cw), (Sp2t_awp_awp_nwp_Cp2,C_eop2,Sp2t_awp_nwp_nwp_Cw), (Sp3t_awp_awp_nwp_Cp2,S_pap3,Snpt_awp_awp_awp_Cp2),
           (Sp3t_awp_awp_nwp_Cp2,C_eop2,Sp3t_awp_nwp_nwp_Cw), (Sp1t_awp_awp_nwp_Cp1,C_eop1,Sp1t_nwp_awp_nwp_Cw), (Snpt_nwp_awp_nwp_Cw,S_mp1,Sp1t_nwp_awp_nwp_Cw),
           (Snpt_nwp_awp_nwp_Cw,C_pck3,Snpt_nwp_awp_nwp_Cp3), (Snpt_nwp_awp_nwp_Cw,C_pck2,Snpt_nwp_awp_nwp_Cp2), (Sp2t_awp_awp_nwp_Cp1,C_eop1,Sp2t_nwp_awp_nwp_Cw),
           (Sp3t_awp_awp_nwp_Cp1,S_pap3,Snpt_awp_awp_awp_Cp1), (Sp3t_awp_awp_nwp_Cp1,C_eop1,Sp3t_nwp_awp_nwp_Cw), (Sp2t_nwp_nwp_nwp_Cp3,S_pap2,Snpt_nwp_awp_nwp_Cp3),
           (Sp2t_nwp_nwp_nwp_Cp2,S_pap2,Snpt_nwp_awp_nwp_Cp2), (Sp2t_nwp_nwp_nwp_Cp1,S_pap2,Snpt_nwp_awp_nwp_Cp1), (Snpt_awp_awp_awp_Cw,S_mp1,Sp1t_awp_awp_awp_Cw),
           (Snpt_awp_awp_awp_Cw,C_pck3,Snpt_awp_awp_awp_Cp3), (Snpt_awp_awp_awp_Cw,S_mp2,Sp2t_awp_awp_awp_Cw), (Snpt_awp_awp_awp_Cw,S_mp3,Sp3t_awp_awp_awp_Cw),
           (Snpt_awp_awp_awp_Cp3,S_mp1,Sp1t_awp_awp_awp_Cp3), (Snpt_awp_awp_awp_Cp3,C_eop3,Snpt_awp_awp_nwp_Cw), (Snpt_awp_awp_awp_Cp3,S_mp2,Sp2t_awp_awp_awp_Cp3),
           (Snpt_awp_awp_awp_Cp3,S_mp3,Sp3t_awp_awp_awp_Cp3), (Snpt_awp_awp_awp_Cp2,S_mp1,Sp1t_awp_awp_awp_Cp2), (Snpt_awp_awp_awp_Cp2,C_eop2,Snpt_awp_nwp_awp_Cw),
           (Snpt_awp_awp_awp_Cp2,S_mp2,Sp2t_awp_awp_awp_Cp2), (Snpt_awp_awp_awp_Cp2,S_mp3,Sp3t_awp_awp_awp_Cp2), (Sp3t_awp_nwp_nwp_Cw,S_pap3,Snpt_awp_nwp_awp_Cw),
           (Sp3t_awp_nwp_nwp_Cw,C_pck3,Sp3t_awp_nwp_nwp_Cp3), (Sp3t_awp_nwp_nwp_Cw,C_pck2,Sp3t_awp_nwp_nwp_Cp2), (Sp3t_awp_nwp_nwp_Cw,C_pck1,Sp3t_awp_nwp_nwp_Cp1),
           (Sp1t_nwp_awp_nwp_Cw,S_pap1,Snpt_awp_awp_nwp_Cw), (Sp1t_nwp_awp_nwp_Cw,C_pck3,Sp1t_nwp_awp_nwp_Cp3), (Sp1t_nwp_awp_nwp_Cw,C_pck2,Sp1t_nwp_awp_nwp_Cp2),
           (Snpt_nwp_awp_nwp_Cp3,S_mp1,Sp1t_nwp_awp_nwp_Cp3), (Snpt_nwp_awp_nwp_Cp2,S_mp1,Sp1t_nwp_awp_nwp_Cp2), (Snpt_nwp_awp_nwp_Cp2,C_eop2,Snpt_nwp_nwp_nwp_Cw),
           (Sp2t_nwp_awp_nwp_Cw,C_pck3,Sp2t_nwp_awp_nwp_Cp3), (Sp2t_nwp_awp_nwp_Cw,C_pck2,Sp2t_nwp_awp_nwp_Cp2), (Snpt_awp_awp_awp_Cp1,S_mp1,Sp1t_awp_awp_awp_Cp1),
           (Snpt_awp_awp_awp_Cp1,C_eop1,Snpt_nwp_awp_awp_Cw), (Snpt_awp_awp_awp_Cp1,S_mp2,Sp2t_awp_awp_awp_Cp1), (Snpt_awp_awp_awp_Cp1,S_mp3,Sp3t_awp_awp_awp_Cp1),
           (Sp3t_nwp_awp_nwp_Cw,S_pap3,Snpt_nwp_awp_awp_Cw), (Sp3t_nwp_awp_nwp_Cw,C_pck3,Sp3t_nwp_awp_nwp_Cp3), (Sp3t_nwp_awp_nwp_Cw,C_pck2,Sp3t_nwp_awp_nwp_Cp2),
           (Snpt_nwp_awp_nwp_Cp1,S_mp1,Sp1t_nwp_awp_nwp_Cp1), (Sp1t_awp_awp_awp_Cw,C_pck3,Sp1t_awp_awp_awp_Cp3), (Sp2t_awp_awp_awp_Cw,C_pck3,Sp2t_awp_awp_awp_Cp3),
           (Sp3t_awp_awp_awp_Cw,C_pck3,Sp3t_awp_awp_awp_Cp3), (Sp1t_awp_awp_awp_Cp3,C_eop3,Sp1t_awp_awp_nwp_Cw), (Sp2t_awp_awp_awp_Cp3,C_eop3,Sp2t_awp_awp_nwp_Cw),
           (Sp3t_awp_awp_awp_Cp3,C_eop3,Sp3t_awp_awp_nwp_Cw), (Sp1t_awp_awp_awp_Cp2,C_eop2,Sp1t_awp_nwp_awp_Cw), (Snpt_awp_nwp_awp_Cw,S_mp1,Sp1t_awp_nwp_awp_Cw),
           (Snpt_awp_nwp_awp_Cw,C_pck3,Snpt_awp_nwp_awp_Cp3), (Snpt_awp_nwp_awp_Cw,S_mp2,Sp2t_awp_nwp_awp_Cw), (Sp2t_awp_awp_awp_Cp2,C_eop2,Sp2t_awp_nwp_awp_Cw),
           (Sp3t_awp_awp_awp_Cp2,C_eop2,Sp3t_awp_nwp_awp_Cw), (Sp3t_awp_nwp_nwp_Cp3,S_pap3,Snpt_awp_nwp_awp_Cp3), (Sp3t_awp_nwp_nwp_Cp2,S_pap3,Snpt_awp_nwp_awp_Cp2),
           (Sp3t_awp_nwp_nwp_Cp1,S_pap3,Snpt_awp_nwp_awp_Cp1), (Sp3t_awp_nwp_nwp_Cp1,C_eop1,Sp3t_nwp_nwp_nwp_Cw), (Sp1t_nwp_awp_nwp_Cp3,S_pap1,Snpt_awp_awp_nwp_Cp3),
           (Sp1t_nwp_awp_nwp_Cp2,S_pap1,Snpt_awp_awp_nwp_Cp2), (Sp1t_nwp_awp_nwp_Cp2,C_eop2,Sp1t_nwp_nwp_nwp_Cw), (Sp2t_nwp_awp_nwp_Cp2,C_eop2,Sp2t_nwp_nwp_nwp_Cw),
           (Sp1t_awp_awp_awp_Cp1,C_eop1,Sp1t_nwp_awp_awp_Cw), (Snpt_nwp_awp_awp_Cw,S_mp1,Sp1t_nwp_awp_awp_Cw), (Snpt_nwp_awp_awp_Cw,C_pck3,Snpt_nwp_awp_awp_Cp3),
           (Sp2t_awp_awp_awp_Cp1,C_eop1,Sp2t_nwp_awp_awp_Cw), (Sp3t_awp_awp_awp_Cp1,C_eop1,Sp3t_nwp_awp_awp_Cw), (Sp3t_nwp_awp_nwp_Cp3,S_pap3,Snpt_nwp_awp_awp_Cp3),
           (Sp3t_nwp_awp_nwp_Cp2,S_pap3,Snpt_nwp_awp_awp_Cp2), (Sp3t_nwp_awp_nwp_Cp2,C_eop2,Sp3t_nwp_nwp_nwp_Cw), (Sp1t_nwp_awp_nwp_Cp1,S_pap1,Snpt_awp_awp_nwp_Cp1),
           (Sp1t_awp_nwp_awp_Cw,C_pck3,Sp1t_awp_nwp_awp_Cp3), (Snpt_awp_nwp_awp_Cp3,S_mp1,Sp1t_awp_nwp_awp_Cp3), (Snpt_awp_nwp_awp_Cp3,C_eop3,Snpt_awp_nwp_nwp_Cw),
           (Snpt_awp_nwp_awp_Cp3,S_mp2,Sp2t_awp_nwp_awp_Cp3), (Sp2t_awp_nwp_awp_Cw,S_pap2,Snpt_awp_awp_awp_Cw), (Sp2t_awp_nwp_awp_Cw,C_pck3,Sp2t_awp_nwp_awp_Cp3),
           (Sp3t_awp_nwp_awp_Cw,C_pck3,Sp3t_awp_nwp_awp_Cp3), (Snpt_awp_nwp_awp_Cp2,S_mp1,Sp1t_awp_nwp_awp_Cp2), (Snpt_awp_nwp_awp_Cp2,S_mp2,Sp2t_awp_nwp_awp_Cp2),
           (Snpt_awp_nwp_awp_Cp1,S_mp1,Sp1t_awp_nwp_awp_Cp1), (Snpt_awp_nwp_awp_Cp1,C_eop1,Snpt_nwp_nwp_awp_Cw), (Snpt_awp_nwp_awp_Cp1,S_mp2,Sp2t_awp_nwp_awp_Cp1),
           (Sp3t_nwp_nwp_nwp_Cw,S_pap3,Snpt_nwp_nwp_awp_Cw), (Sp3t_nwp_nwp_nwp_Cw,C_pck3,Sp3t_nwp_nwp_nwp_Cp3), (Sp3t_nwp_nwp_nwp_Cw,C_pck2,Sp3t_nwp_nwp_nwp_Cp2),
           (Sp3t_nwp_nwp_nwp_Cw,C_pck1,Sp3t_nwp_nwp_nwp_Cp1), (Sp1t_nwp_awp_awp_Cw,S_pap1,Snpt_awp_awp_awp_Cw), (Sp1t_nwp_awp_awp_Cw,C_pck3,Sp1t_nwp_awp_awp_Cp3),
           (Snpt_nwp_awp_awp_Cp3,S_mp1,Sp1t_nwp_awp_awp_Cp3), (Snpt_nwp_awp_awp_Cp3,C_eop3,Snpt_nwp_awp_nwp_Cw), (Sp2t_nwp_awp_awp_Cw,C_pck3,Sp2t_nwp_awp_awp_Cp3),
           (Sp3t_nwp_awp_awp_Cw,C_pck3,Sp3t_nwp_awp_awp_Cp3), (Snpt_nwp_awp_awp_Cp2,S_mp1,Sp1t_nwp_awp_awp_Cp2), (Snpt_nwp_awp_awp_Cp2,C_eop2,Snpt_nwp_nwp_awp_Cw),
           (Sp1t_awp_nwp_awp_Cp3,C_eop3,Sp1t_awp_nwp_nwp_Cw), (Sp2t_awp_nwp_awp_Cp3,S_pap2,Snpt_awp_awp_awp_Cp3), (Sp2t_awp_nwp_awp_Cp3,C_eop3,Sp2t_awp_nwp_nwp_Cw),
           (Sp3t_awp_nwp_awp_Cp3,C_eop3,Sp3t_awp_nwp_nwp_Cw), (Sp2t_awp_nwp_awp_Cp2,S_pap2,Snpt_awp_awp_awp_Cp2), (Sp1t_awp_nwp_awp_Cp1,C_eop1,Sp1t_nwp_nwp_awp_Cw),
           (Snpt_nwp_nwp_awp_Cw,S_mp1,Sp1t_nwp_nwp_awp_Cw), (Snpt_nwp_nwp_awp_Cw,C_pck3,Snpt_nwp_nwp_awp_Cp3), (Sp2t_awp_nwp_awp_Cp1,S_pap2,Snpt_awp_awp_awp_Cp1),
           (Sp2t_awp_nwp_awp_Cp1,C_eop1,Sp2t_nwp_nwp_awp_Cw), (Sp3t_nwp_nwp_nwp_Cp3,S_pap3,Snpt_nwp_nwp_awp_Cp3), (Sp3t_nwp_nwp_nwp_Cp2,S_pap3,Snpt_nwp_nwp_awp_Cp2),
           (Sp3t_nwp_nwp_nwp_Cp1,S_pap3,Snpt_nwp_nwp_awp_Cp1), (Sp1t_nwp_awp_awp_Cp3,S_pap1,Snpt_awp_awp_awp_Cp3), (Sp1t_nwp_awp_awp_Cp3,C_eop3,Sp1t_nwp_awp_nwp_Cw),
           (Sp2t_nwp_awp_awp_Cp3,C_eop3,Sp2t_nwp_awp_nwp_Cw), (Sp3t_nwp_awp_awp_Cp3,C_eop3,Sp3t_nwp_awp_nwp_Cw), (Sp1t_nwp_awp_awp_Cp2,S_pap1,Snpt_awp_awp_awp_Cp2),
           (Sp1t_nwp_awp_awp_Cp2,C_eop2,Sp1t_nwp_nwp_awp_Cw), (Sp1t_nwp_nwp_awp_Cw,S_pap1,Snpt_awp_nwp_awp_Cw), (Sp1t_nwp_nwp_awp_Cw,C_pck3,Sp1t_nwp_nwp_awp_Cp3),
           (Snpt_nwp_nwp_awp_Cp3,S_mp1,Sp1t_nwp_nwp_awp_Cp3), (Snpt_nwp_nwp_awp_Cp3,C_eop3,Snpt_nwp_nwp_nwp_Cw), (Sp2t_nwp_nwp_awp_Cw,S_pap2,Snpt_nwp_awp_awp_Cw),
           (Sp2t_nwp_nwp_awp_Cw,C_pck3,Sp2t_nwp_nwp_awp_Cp3), (Snpt_nwp_nwp_awp_Cp2,S_mp1,Sp1t_nwp_nwp_awp_Cp2), (Snpt_nwp_nwp_awp_Cp1,S_mp1,Sp1t_nwp_nwp_awp_Cp1),
           (Sp1t_nwp_nwp_awp_Cp3,S_pap1,Snpt_awp_nwp_awp_Cp3), (Sp1t_nwp_nwp_awp_Cp3,C_eop3,Sp1t_nwp_nwp_nwp_Cw), (Sp2t_nwp_nwp_awp_Cp3,S_pap2,Snpt_nwp_awp_awp_Cp3),
           (Sp2t_nwp_nwp_awp_Cp3,C_eop3,Sp2t_nwp_nwp_nwp_Cw), (Sp1t_nwp_nwp_awp_Cp2,S_pap1,Snpt_awp_nwp_awp_Cp2), (Sp1t_nwp_nwp_awp_Cp1,S_pap1,Snpt_awp_nwp_awp_Cp1) },
      x_0: Snpt_nwp_nwp_nwp_Cw,
      Xm: {Snpt_nwp_nwp_nwp_Cw},
      ctrlEvents: {S_mp1, S_mp2, S_mp3, C_pck1, C_pck2, C_pck3} )
 &
 sorter = {
      (Snpt_nwp_nwp_nwp_Cw,Snpt),(Sp1t_nwp_nwp_nwp_Cw,Sp1t),(Snpt_nwp_nwp_nwp_Cp3,Snpt),
      (Snpt_nwp_nwp_nwp_Cp2,Snpt),(Snpt_nwp_nwp_nwp_Cp1,Snpt),(Snpt_awp_nwp_nwp_Cw,Snpt),
      (Sp1t_nwp_nwp_nwp_Cp3,Sp1t),(Sp1t_nwp_nwp_nwp_Cp2,Sp1t),(Sp1t_nwp_nwp_nwp_Cp1,Sp1t),
      (Sp1t_awp_nwp_nwp_Cw,Sp1t),(Snpt_awp_nwp_nwp_Cp3,Snpt),(Snpt_awp_nwp_nwp_Cp2,Snpt),
      (Snpt_awp_nwp_nwp_Cp1,Snpt),(Sp2t_awp_nwp_nwp_Cw,Sp2t),(Sp1t_awp_nwp_nwp_Cp3,Sp1t),
      (Sp1t_awp_nwp_nwp_Cp2,Sp1t),(Sp1t_awp_nwp_nwp_Cp1,Sp1t),(Sp2t_awp_nwp_nwp_Cp3,Sp2t),
      (Sp2t_awp_nwp_nwp_Cp2,Sp2t),(Sp2t_awp_nwp_nwp_Cp1,Sp2t),(Snpt_awp_awp_nwp_Cw,Snpt),
      (Snpt_awp_awp_nwp_Cp3,Snpt),(Snpt_awp_awp_nwp_Cp2,Snpt),(Snpt_awp_awp_nwp_Cp1,Snpt),
      (Sp2t_nwp_nwp_nwp_Cw,Sp2t),(Sp1t_awp_awp_nwp_Cw,Sp1t),(Sp2t_awp_awp_nwp_Cw,Sp2t),
      (Sp3t_awp_awp_nwp_Cw,Sp3t),(Sp1t_awp_awp_nwp_Cp3,Sp1t),(Sp2t_awp_awp_nwp_Cp3,Sp2t),
      (Sp3t_awp_awp_nwp_Cp3,Sp3t),(Sp1t_awp_awp_nwp_Cp2,Sp1t),(Sp2t_awp_awp_nwp_Cp2,Sp2t),
      (Sp3t_awp_awp_nwp_Cp2,Sp3t),(Sp1t_awp_awp_nwp_Cp1,Sp1t),(Snpt_nwp_awp_nwp_Cw,Snpt),
      (Sp2t_awp_awp_nwp_Cp1,Sp2t),(Sp3t_awp_awp_nwp_Cp1,Sp3t),(Sp2t_nwp_nwp_nwp_Cp3,Sp2t),
      (Sp2t_nwp_nwp_nwp_Cp2,Sp2t),(Sp2t_nwp_nwp_nwp_Cp1,Sp2t),(Snpt_awp_awp_awp_Cw,Snpt),
      (Snpt_awp_awp_awp_Cp3,Snpt),(Snpt_awp_awp_awp_Cp2,Snpt),(Sp3t_awp_nwp_nwp_Cw,Sp3t),
      (Sp1t_nwp_awp_nwp_Cw,Sp1t),(Snpt_nwp_awp_nwp_Cp3,Snpt),(Snpt_nwp_awp_nwp_Cp2,Snpt),
      (Sp2t_nwp_awp_nwp_Cw,Sp2t),(Snpt_awp_awp_awp_Cp1,Snpt),(Sp3t_nwp_awp_nwp_Cw,Sp3t),
      (Snpt_nwp_awp_nwp_Cp1,Snpt),(Sp1t_awp_awp_awp_Cw,Sp1t),(Sp2t_awp_awp_awp_Cw,Sp2t),
      (Sp3t_awp_awp_awp_Cw,Sp3t),(Sp1t_awp_awp_awp_Cp3,Sp1t),(Sp2t_awp_awp_awp_Cp3,Sp2t),
      (Sp3t_awp_awp_awp_Cp3,Sp3t),(Sp1t_awp_awp_awp_Cp2,Sp1t),(Snpt_awp_nwp_awp_Cw,Snpt),
      (Sp2t_awp_awp_awp_Cp2,Sp2t),(Sp3t_awp_awp_awp_Cp2,Sp3t),(Sp3t_awp_nwp_nwp_Cp3,Sp3t),
      (Sp3t_awp_nwp_nwp_Cp2,Sp3t),(Sp3t_awp_nwp_nwp_Cp1,Sp3t),(Sp1t_nwp_awp_nwp_Cp3,Sp1t),
      (Sp1t_nwp_awp_nwp_Cp2,Sp1t),(Sp2t_nwp_awp_nwp_Cp3,Sp2t),(Sp2t_nwp_awp_nwp_Cp2,Sp2t),
      (Sp1t_awp_awp_awp_Cp1,Sp1t),(Snpt_nwp_awp_awp_Cw,Snpt),(Sp2t_awp_awp_awp_Cp1,Sp2t),
      (Sp3t_awp_awp_awp_Cp1,Sp3t),(Sp3t_nwp_awp_nwp_Cp3,Sp3t),(Sp3t_nwp_awp_nwp_Cp2,Sp3t),
      (Sp1t_nwp_awp_nwp_Cp1,Sp1t),(Sp1t_awp_nwp_awp_Cw,Sp1t),(Snpt_awp_nwp_awp_Cp3,Snpt),
      (Sp2t_awp_nwp_awp_Cw,Sp2t),(Sp3t_awp_nwp_awp_Cw,Sp3t),(Snpt_awp_nwp_awp_Cp2,Snpt),
      (Snpt_awp_nwp_awp_Cp1,Snpt),(Sp3t_nwp_nwp_nwp_Cw,Sp3t),(Sp1t_nwp_awp_awp_Cw,Sp1t),
      (Snpt_nwp_awp_awp_Cp3,Snpt),(Sp2t_nwp_awp_awp_Cw,Sp2t),(Sp3t_nwp_awp_awp_Cw,Sp3t),
      (Snpt_nwp_awp_awp_Cp2,Snpt),(Sp1t_awp_nwp_awp_Cp3,Sp1t),(Sp2t_awp_nwp_awp_Cp3,Sp2t),
      (Sp3t_awp_nwp_awp_Cp3,Sp3t),(Sp1t_awp_nwp_awp_Cp2,Sp1t),(Sp2t_awp_nwp_awp_Cp2,Sp2t),
      (Sp1t_awp_nwp_awp_Cp1,Sp1t),(Snpt_nwp_nwp_awp_Cw,Snpt),(Sp2t_awp_nwp_awp_Cp1,Sp2t),
      (Sp3t_nwp_nwp_nwp_Cp3,Sp3t),(Sp3t_nwp_nwp_nwp_Cp2,Sp3t),(Sp3t_nwp_nwp_nwp_Cp1,Sp3t),
      (Sp1t_nwp_awp_awp_Cp3,Sp1t),(Sp2t_nwp_awp_awp_Cp3,Sp2t),(Sp3t_nwp_awp_awp_Cp3,Sp3t),
      (Sp1t_nwp_awp_awp_Cp2,Sp1t),(Sp1t_nwp_nwp_awp_Cw,Sp1t),(Snpt_nwp_nwp_awp_Cp3,Snpt),
      (Sp2t_nwp_nwp_awp_Cw,Sp2t),(Snpt_nwp_nwp_awp_Cp2,Snpt),(Snpt_nwp_nwp_awp_Cp1,Snpt),
      (Sp1t_nwp_nwp_awp_Cp3,Sp1t),(Sp2t_nwp_nwp_awp_Cp3,Sp2t),(Sp1t_nwp_nwp_awp_Cp2,Sp1t),
      (Sp1t_nwp_nwp_awp_Cp1,Sp1t) }
 &
 buffer1 = {
      (Snpt_nwp_nwp_nwp_Cw,nwp),(Sp1t_nwp_nwp_nwp_Cw,nwp),(Snpt_nwp_nwp_nwp_Cp3,nwp),
      (Snpt_nwp_nwp_nwp_Cp2,nwp),(Snpt_nwp_nwp_nwp_Cp1,nwp),(Snpt_awp_nwp_nwp_Cw,awp),
      (Sp1t_nwp_nwp_nwp_Cp3,nwp),(Sp1t_nwp_nwp_nwp_Cp2,nwp),(Sp1t_nwp_nwp_nwp_Cp1,nwp),
      (Sp1t_awp_nwp_nwp_Cw,awp),(Snpt_awp_nwp_nwp_Cp3,awp),(Snpt_awp_nwp_nwp_Cp2,awp),
      (Snpt_awp_nwp_nwp_Cp1,awp),(Sp2t_awp_nwp_nwp_Cw,awp),(Sp1t_awp_nwp_nwp_Cp3,awp),
      (Sp1t_awp_nwp_nwp_Cp2,awp),(Sp1t_awp_nwp_nwp_Cp1,awp),(Sp2t_awp_nwp_nwp_Cp3,awp),
      (Sp2t_awp_nwp_nwp_Cp2,awp),(Sp2t_awp_nwp_nwp_Cp1,awp),(Snpt_awp_awp_nwp_Cw,awp),
      (Snpt_awp_awp_nwp_Cp3,awp),(Snpt_awp_awp_nwp_Cp2,awp),(Snpt_awp_awp_nwp_Cp1,awp),
      (Sp2t_nwp_nwp_nwp_Cw,nwp),(Sp1t_awp_awp_nwp_Cw,awp),(Sp2t_awp_awp_nwp_Cw,awp),
      (Sp3t_awp_awp_nwp_Cw,awp),(Sp1t_awp_awp_nwp_Cp3,awp),(Sp2t_awp_awp_nwp_Cp3,awp),
      (Sp3t_awp_awp_nwp_Cp3,awp),(Sp1t_awp_awp_nwp_Cp2,awp),(Sp2t_awp_awp_nwp_Cp2,awp),
      (Sp3t_awp_awp_nwp_Cp2,awp),(Sp1t_awp_awp_nwp_Cp1,awp),(Snpt_nwp_awp_nwp_Cw,nwp),
      (Sp2t_awp_awp_nwp_Cp1,awp),(Sp3t_awp_awp_nwp_Cp1,awp),(Sp2t_nwp_nwp_nwp_Cp3,nwp),
      (Sp2t_nwp_nwp_nwp_Cp2,nwp),(Sp2t_nwp_nwp_nwp_Cp1,nwp),(Snpt_awp_awp_awp_Cw,awp),
      (Snpt_awp_awp_awp_Cp3,awp),(Snpt_awp_awp_awp_Cp2,awp),(Sp3t_awp_nwp_nwp_Cw,awp),
      (Sp1t_nwp_awp_nwp_Cw,nwp),(Snpt_nwp_awp_nwp_Cp3,nwp),(Snpt_nwp_awp_nwp_Cp2,nwp),
      (Sp2t_nwp_awp_nwp_Cw,nwp),(Snpt_awp_awp_awp_Cp1,awp),(Sp3t_nwp_awp_nwp_Cw,nwp),
      (Snpt_nwp_awp_nwp_Cp1,nwp),(Sp1t_awp_awp_awp_Cw,awp),(Sp2t_awp_awp_awp_Cw,awp),
      (Sp3t_awp_awp_awp_Cw,awp),(Sp1t_awp_awp_awp_Cp3,awp),(Sp2t_awp_awp_awp_Cp3,awp),
      (Sp3t_awp_awp_awp_Cp3,awp),(Sp1t_awp_awp_awp_Cp2,awp),(Snpt_awp_nwp_awp_Cw,awp),
      (Sp2t_awp_awp_awp_Cp2,awp),(Sp3t_awp_awp_awp_Cp2,awp),(Sp3t_awp_nwp_nwp_Cp3,awp),
      (Sp3t_awp_nwp_nwp_Cp2,awp),(Sp3t_awp_nwp_nwp_Cp1,awp),(Sp1t_nwp_awp_nwp_Cp3,nwp),
      (Sp1t_nwp_awp_nwp_Cp2,nwp),(Sp2t_nwp_awp_nwp_Cp3,nwp),(Sp2t_nwp_awp_nwp_Cp2,nwp),
      (Sp1t_awp_awp_awp_Cp1,awp),(Snpt_nwp_awp_awp_Cw,nwp),(Sp2t_awp_awp_awp_Cp1,awp),
      (Sp3t_awp_awp_awp_Cp1,awp),(Sp3t_nwp_awp_nwp_Cp3,nwp),(Sp3t_nwp_awp_nwp_Cp2,nwp),
      (Sp1t_nwp_awp_nwp_Cp1,nwp),(Sp1t_awp_nwp_awp_Cw,awp),(Snpt_awp_nwp_awp_Cp3,awp),
      (Sp2t_awp_nwp_awp_Cw,awp),(Sp3t_awp_nwp_awp_Cw,awp),(Snpt_awp_nwp_awp_Cp2,awp),
      (Snpt_awp_nwp_awp_Cp1,awp),(Sp3t_nwp_nwp_nwp_Cw,nwp),(Sp1t_nwp_awp_awp_Cw,nwp),
      (Snpt_nwp_awp_awp_Cp3,nwp),(Sp2t_nwp_awp_awp_Cw,nwp),(Sp3t_nwp_awp_awp_Cw,nwp),
      (Snpt_nwp_awp_awp_Cp2,nwp),(Sp1t_awp_nwp_awp_Cp3,awp),(Sp2t_awp_nwp_awp_Cp3,awp),
      (Sp3t_awp_nwp_awp_Cp3,awp),(Sp1t_awp_nwp_awp_Cp2,awp),(Sp2t_awp_nwp_awp_Cp2,awp),
      (Sp1t_awp_nwp_awp_Cp1,awp),(Snpt_nwp_nwp_awp_Cw,nwp),(Sp2t_awp_nwp_awp_Cp1,awp),
      (Sp3t_nwp_nwp_nwp_Cp3,nwp),(Sp3t_nwp_nwp_nwp_Cp2,nwp),(Sp3t_nwp_nwp_nwp_Cp1,nwp),
      (Sp1t_nwp_awp_awp_Cp3,nwp),(Sp2t_nwp_awp_awp_Cp3,nwp),(Sp3t_nwp_awp_awp_Cp3,nwp),
      (Sp1t_nwp_awp_awp_Cp2,nwp),(Sp1t_nwp_nwp_awp_Cw,nwp),(Snpt_nwp_nwp_awp_Cp3,nwp),
      (Sp2t_nwp_nwp_awp_Cw,nwp),(Snpt_nwp_nwp_awp_Cp2,nwp),(Snpt_nwp_nwp_awp_Cp1,nwp),
      (Sp1t_nwp_nwp_awp_Cp3,nwp),(Sp2t_nwp_nwp_awp_Cp3,nwp),(Sp1t_nwp_nwp_awp_Cp2,nwp),
      (Sp1t_nwp_nwp_awp_Cp1,nwp) }
 &
 buffer2 = {
      (Snpt_nwp_nwp_nwp_Cw,nwp),(Sp1t_nwp_nwp_nwp_Cw,nwp),(Snpt_nwp_nwp_nwp_Cp3,nwp),
      (Snpt_nwp_nwp_nwp_Cp2,nwp),(Snpt_nwp_nwp_nwp_Cp1,nwp),(Snpt_awp_nwp_nwp_Cw,nwp),
      (Sp1t_nwp_nwp_nwp_Cp3,nwp),(Sp1t_nwp_nwp_nwp_Cp2,nwp),(Sp1t_nwp_nwp_nwp_Cp1,nwp),
      (Sp1t_awp_nwp_nwp_Cw,nwp),(Snpt_awp_nwp_nwp_Cp3,nwp),(Snpt_awp_nwp_nwp_Cp2,nwp),
      (Snpt_awp_nwp_nwp_Cp1,nwp),(Sp2t_awp_nwp_nwp_Cw,nwp),(Sp1t_awp_nwp_nwp_Cp3,nwp),
      (Sp1t_awp_nwp_nwp_Cp2,nwp),(Sp1t_awp_nwp_nwp_Cp1,nwp),(Sp2t_awp_nwp_nwp_Cp3,nwp),
      (Sp2t_awp_nwp_nwp_Cp2,nwp),(Sp2t_awp_nwp_nwp_Cp1,nwp),(Snpt_awp_awp_nwp_Cw,awp),
      (Snpt_awp_awp_nwp_Cp3,awp),(Snpt_awp_awp_nwp_Cp2,awp),(Snpt_awp_awp_nwp_Cp1,awp),
      (Sp2t_nwp_nwp_nwp_Cw,nwp),(Sp1t_awp_awp_nwp_Cw,awp),(Sp2t_awp_awp_nwp_Cw,awp),
      (Sp3t_awp_awp_nwp_Cw,awp),(Sp1t_awp_awp_nwp_Cp3,awp),(Sp2t_awp_awp_nwp_Cp3,awp),
      (Sp3t_awp_awp_nwp_Cp3,awp),(Sp1t_awp_awp_nwp_Cp2,awp),(Sp2t_awp_awp_nwp_Cp2,awp),
      (Sp3t_awp_awp_nwp_Cp2,awp),(Sp1t_awp_awp_nwp_Cp1,awp),(Snpt_nwp_awp_nwp_Cw,awp),
      (Sp2t_awp_awp_nwp_Cp1,awp),(Sp3t_awp_awp_nwp_Cp1,awp),(Sp2t_nwp_nwp_nwp_Cp3,nwp),
      (Sp2t_nwp_nwp_nwp_Cp2,nwp),(Sp2t_nwp_nwp_nwp_Cp1,nwp),(Snpt_awp_awp_awp_Cw,awp),
      (Snpt_awp_awp_awp_Cp3,awp),(Snpt_awp_awp_awp_Cp2,awp),(Sp3t_awp_nwp_nwp_Cw,nwp),
      (Sp1t_nwp_awp_nwp_Cw,awp),(Snpt_nwp_awp_nwp_Cp3,awp),(Snpt_nwp_awp_nwp_Cp2,awp),
      (Sp2t_nwp_awp_nwp_Cw,awp),(Snpt_awp_awp_awp_Cp1,awp),(Sp3t_nwp_awp_nwp_Cw,awp),
      (Snpt_nwp_awp_nwp_Cp1,awp),(Sp1t_awp_awp_awp_Cw,awp),(Sp2t_awp_awp_awp_Cw,awp),
      (Sp3t_awp_awp_awp_Cw,awp),(Sp1t_awp_awp_awp_Cp3,awp),(Sp2t_awp_awp_awp_Cp3,awp),
      (Sp3t_awp_awp_awp_Cp3,awp),(Sp1t_awp_awp_awp_Cp2,awp),(Snpt_awp_nwp_awp_Cw,nwp),
      (Sp2t_awp_awp_awp_Cp2,awp),(Sp3t_awp_awp_awp_Cp2,awp),(Sp3t_awp_nwp_nwp_Cp3,nwp),
      (Sp3t_awp_nwp_nwp_Cp2,nwp),(Sp3t_awp_nwp_nwp_Cp1,nwp),(Sp1t_nwp_awp_nwp_Cp3,awp),
      (Sp1t_nwp_awp_nwp_Cp2,awp),(Sp2t_nwp_awp_nwp_Cp3,awp),(Sp2t_nwp_awp_nwp_Cp2,awp),
      (Sp1t_awp_awp_awp_Cp1,awp),(Snpt_nwp_awp_awp_Cw,awp),(Sp2t_awp_awp_awp_Cp1,awp),
      (Sp3t_awp_awp_awp_Cp1,awp),(Sp3t_nwp_awp_nwp_Cp3,awp),(Sp3t_nwp_awp_nwp_Cp2,awp),
      (Sp1t_nwp_awp_nwp_Cp1,awp),(Sp1t_awp_nwp_awp_Cw,nwp),(Snpt_awp_nwp_awp_Cp3,nwp),
      (Sp2t_awp_nwp_awp_Cw,nwp),(Sp3t_awp_nwp_awp_Cw,nwp),(Snpt_awp_nwp_awp_Cp2,nwp),
      (Snpt_awp_nwp_awp_Cp1,nwp),(Sp3t_nwp_nwp_nwp_Cw,nwp),(Sp1t_nwp_awp_awp_Cw,awp),
      (Snpt_nwp_awp_awp_Cp3,awp),(Sp2t_nwp_awp_awp_Cw,awp),(Sp3t_nwp_awp_awp_Cw,awp),
      (Snpt_nwp_awp_awp_Cp2,awp),(Sp1t_awp_nwp_awp_Cp3,nwp),(Sp2t_awp_nwp_awp_Cp3,nwp),
      (Sp3t_awp_nwp_awp_Cp3,nwp),(Sp1t_awp_nwp_awp_Cp2,nwp),(Sp2t_awp_nwp_awp_Cp2,nwp),
      (Sp1t_awp_nwp_awp_Cp1,nwp),(Snpt_nwp_nwp_awp_Cw,nwp),(Sp2t_awp_nwp_awp_Cp1,nwp),
      (Sp3t_nwp_nwp_nwp_Cp3,nwp),(Sp3t_nwp_nwp_nwp_Cp2,nwp),(Sp3t_nwp_nwp_nwp_Cp1,nwp),
      (Sp1t_nwp_awp_awp_Cp3,awp),(Sp2t_nwp_awp_awp_Cp3,awp),(Sp3t_nwp_awp_awp_Cp3,awp),
      (Sp1t_nwp_awp_awp_Cp2,awp),(Sp1t_nwp_nwp_awp_Cw,nwp),(Snpt_nwp_nwp_awp_Cp3,nwp),
      (Sp2t_nwp_nwp_awp_Cw,nwp),(Snpt_nwp_nwp_awp_Cp2,nwp),(Snpt_nwp_nwp_awp_Cp1,nwp),
      (Sp1t_nwp_nwp_awp_Cp3,nwp),(Sp2t_nwp_nwp_awp_Cp3,nwp),(Sp1t_nwp_nwp_awp_Cp2,nwp),
      (Sp1t_nwp_nwp_awp_Cp1,nwp) }
 &
 buffer3 = {
      (Snpt_nwp_nwp_nwp_Cw,nwp),(Sp1t_nwp_nwp_nwp_Cw,nwp),(Snpt_nwp_nwp_nwp_Cp3,nwp),
      (Snpt_nwp_nwp_nwp_Cp2,nwp),(Snpt_nwp_nwp_nwp_Cp1,nwp),(Snpt_awp_nwp_nwp_Cw,nwp),
      (Sp1t_nwp_nwp_nwp_Cp3,nwp),(Sp1t_nwp_nwp_nwp_Cp2,nwp),(Sp1t_nwp_nwp_nwp_Cp1,nwp),
      (Sp1t_awp_nwp_nwp_Cw,nwp),(Snpt_awp_nwp_nwp_Cp3,nwp),(Snpt_awp_nwp_nwp_Cp2,nwp),
      (Snpt_awp_nwp_nwp_Cp1,nwp),(Sp2t_awp_nwp_nwp_Cw,nwp),(Sp1t_awp_nwp_nwp_Cp3,nwp),
      (Sp1t_awp_nwp_nwp_Cp2,nwp),(Sp1t_awp_nwp_nwp_Cp1,nwp),(Sp2t_awp_nwp_nwp_Cp3,nwp),
      (Sp2t_awp_nwp_nwp_Cp2,nwp),(Sp2t_awp_nwp_nwp_Cp1,nwp),(Snpt_awp_awp_nwp_Cw,nwp),
      (Snpt_awp_awp_nwp_Cp3,nwp),(Snpt_awp_awp_nwp_Cp2,nwp),(Snpt_awp_awp_nwp_Cp1,nwp),
      (Sp2t_nwp_nwp_nwp_Cw,nwp),(Sp1t_awp_awp_nwp_Cw,nwp),(Sp2t_awp_awp_nwp_Cw,nwp),
      (Sp3t_awp_awp_nwp_Cw,nwp),(Sp1t_awp_awp_nwp_Cp3,nwp),(Sp2t_awp_awp_nwp_Cp3,nwp),
      (Sp3t_awp_awp_nwp_Cp3,nwp),(Sp1t_awp_awp_nwp_Cp2,nwp),(Sp2t_awp_awp_nwp_Cp2,nwp),
      (Sp3t_awp_awp_nwp_Cp2,nwp),(Sp1t_awp_awp_nwp_Cp1,nwp),(Snpt_nwp_awp_nwp_Cw,nwp),
      (Sp2t_awp_awp_nwp_Cp1,nwp),(Sp3t_awp_awp_nwp_Cp1,nwp),(Sp2t_nwp_nwp_nwp_Cp3,nwp),
      (Sp2t_nwp_nwp_nwp_Cp2,nwp),(Sp2t_nwp_nwp_nwp_Cp1,nwp),(Snpt_awp_awp_awp_Cw,awp),
      (Snpt_awp_awp_awp_Cp3,awp),(Snpt_awp_awp_awp_Cp2,awp),(Sp3t_awp_nwp_nwp_Cw,nwp),
      (Sp1t_nwp_awp_nwp_Cw,nwp),(Snpt_nwp_awp_nwp_Cp3,nwp),(Snpt_nwp_awp_nwp_Cp2,nwp),
      (Sp2t_nwp_awp_nwp_Cw,nwp),(Snpt_awp_awp_awp_Cp1,awp),(Sp3t_nwp_awp_nwp_Cw,nwp),
      (Snpt_nwp_awp_nwp_Cp1,nwp),(Sp1t_awp_awp_awp_Cw,awp),(Sp2t_awp_awp_awp_Cw,awp),
      (Sp3t_awp_awp_awp_Cw,awp),(Sp1t_awp_awp_awp_Cp3,awp),(Sp2t_awp_awp_awp_Cp3,awp),
      (Sp3t_awp_awp_awp_Cp3,awp),(Sp1t_awp_awp_awp_Cp2,awp),(Snpt_awp_nwp_awp_Cw,awp),
      (Sp2t_awp_awp_awp_Cp2,awp),(Sp3t_awp_awp_awp_Cp2,awp),(Sp3t_awp_nwp_nwp_Cp3,nwp),
      (Sp3t_awp_nwp_nwp_Cp2,nwp),(Sp3t_awp_nwp_nwp_Cp1,nwp),(Sp1t_nwp_awp_nwp_Cp3,nwp),
      (Sp1t_nwp_awp_nwp_Cp2,nwp),(Sp2t_nwp_awp_nwp_Cp3,nwp),(Sp2t_nwp_awp_nwp_Cp2,nwp),
      (Sp1t_awp_awp_awp_Cp1,awp),(Snpt_nwp_awp_awp_Cw,awp),(Sp2t_awp_awp_awp_Cp1,awp),
      (Sp3t_awp_awp_awp_Cp1,awp),(Sp3t_nwp_awp_nwp_Cp3,nwp),(Sp3t_nwp_awp_nwp_Cp2,nwp),
      (Sp1t_nwp_awp_nwp_Cp1,nwp),(Sp1t_awp_nwp_awp_Cw,awp),(Snpt_awp_nwp_awp_Cp3,awp),
      (Sp2t_awp_nwp_awp_Cw,awp),(Sp3t_awp_nwp_awp_Cw,awp),(Snpt_awp_nwp_awp_Cp2,awp),
      (Snpt_awp_nwp_awp_Cp1,awp),(Sp3t_nwp_nwp_nwp_Cw,nwp),(Sp1t_nwp_awp_awp_Cw,awp),
      (Snpt_nwp_awp_awp_Cp3,awp),(Sp2t_nwp_awp_awp_Cw,awp),(Sp3t_nwp_awp_awp_Cw,awp),
      (Snpt_nwp_awp_awp_Cp2,awp),(Sp1t_awp_nwp_awp_Cp3,awp),(Sp2t_awp_nwp_awp_Cp3,awp),
      (Sp3t_awp_nwp_awp_Cp3,awp),(Sp1t_awp_nwp_awp_Cp2,awp),(Sp2t_awp_nwp_awp_Cp2,awp),
      (Sp1t_awp_nwp_awp_Cp1,awp),(Snpt_nwp_nwp_awp_Cw,awp),(Sp2t_awp_nwp_awp_Cp1,awp),
      (Sp3t_nwp_nwp_nwp_Cp3,nwp),(Sp3t_nwp_nwp_nwp_Cp2,nwp),(Sp3t_nwp_nwp_nwp_Cp1,nwp),
      (Sp1t_nwp_awp_awp_Cp3,awp),(Sp2t_nwp_awp_awp_Cp3,awp),(Sp3t_nwp_awp_awp_Cp3,awp),
      (Sp1t_nwp_awp_awp_Cp2,awp),(Sp1t_nwp_nwp_awp_Cw,awp),(Snpt_nwp_nwp_awp_Cp3,awp),
      (Sp2t_nwp_nwp_awp_Cw,awp),(Snpt_nwp_nwp_awp_Cp2,awp),(Snpt_nwp_nwp_awp_Cp1,awp),
      (Sp1t_nwp_nwp_awp_Cp3,awp),(Sp2t_nwp_nwp_awp_Cp3,awp),(Sp1t_nwp_nwp_awp_Cp2,awp),
      (Sp1t_nwp_nwp_awp_Cp1,awp) }
 &
 crane = {
      (Snpt_nwp_nwp_nwp_Cw,Cw),(Sp1t_nwp_nwp_nwp_Cw,Cw),(Snpt_nwp_nwp_nwp_Cp3,Cp3),
      (Snpt_nwp_nwp_nwp_Cp2,Cp2),(Snpt_nwp_nwp_nwp_Cp1,Cp1),(Snpt_awp_nwp_nwp_Cw,Cw),
      (Sp1t_nwp_nwp_nwp_Cp3,Cp3),(Sp1t_nwp_nwp_nwp_Cp2,Cp2),(Sp1t_nwp_nwp_nwp_Cp1,Cp1),
      (Sp1t_awp_nwp_nwp_Cw,Cw),(Snpt_awp_nwp_nwp_Cp3,Cp3),(Snpt_awp_nwp_nwp_Cp2,Cp2),
      (Snpt_awp_nwp_nwp_Cp1,Cp1),(Sp2t_awp_nwp_nwp_Cw,Cw),(Sp1t_awp_nwp_nwp_Cp3,Cp3),
      (Sp1t_awp_nwp_nwp_Cp2,Cp2),(Sp1t_awp_nwp_nwp_Cp1,Cp1),(Sp2t_awp_nwp_nwp_Cp3,Cp3),
      (Sp2t_awp_nwp_nwp_Cp2,Cp2),(Sp2t_awp_nwp_nwp_Cp1,Cp1),(Snpt_awp_awp_nwp_Cw,Cw),
      (Snpt_awp_awp_nwp_Cp3,Cp3),(Snpt_awp_awp_nwp_Cp2,Cp2),(Snpt_awp_awp_nwp_Cp1,Cp1),
      (Sp2t_nwp_nwp_nwp_Cw,Cw),(Sp1t_awp_awp_nwp_Cw,Cw),(Sp2t_awp_awp_nwp_Cw,Cw),
      (Sp3t_awp_awp_nwp_Cw,Cw),(Sp1t_awp_awp_nwp_Cp3,Cp3),(Sp2t_awp_awp_nwp_Cp3,Cp3),
      (Sp3t_awp_awp_nwp_Cp3,Cp3),(Sp1t_awp_awp_nwp_Cp2,Cp2),(Sp2t_awp_awp_nwp_Cp2,Cp2),
      (Sp3t_awp_awp_nwp_Cp2,Cp2),(Sp1t_awp_awp_nwp_Cp1,Cp1),(Snpt_nwp_awp_nwp_Cw,Cw),
      (Sp2t_awp_awp_nwp_Cp1,Cp1),(Sp3t_awp_awp_nwp_Cp1,Cp1),(Sp2t_nwp_nwp_nwp_Cp3,Cp3),
      (Sp2t_nwp_nwp_nwp_Cp2,Cp2),(Sp2t_nwp_nwp_nwp_Cp1,Cp1),(Snpt_awp_awp_awp_Cw,Cw),
      (Snpt_awp_awp_awp_Cp3,Cp3),(Snpt_awp_awp_awp_Cp2,Cp2),(Sp3t_awp_nwp_nwp_Cw,Cw),
      (Sp1t_nwp_awp_nwp_Cw,Cw),(Snpt_nwp_awp_nwp_Cp3,Cp3),(Snpt_nwp_awp_nwp_Cp2,Cp2),
      (Sp2t_nwp_awp_nwp_Cw,Cw),(Snpt_awp_awp_awp_Cp1,Cp1),(Sp3t_nwp_awp_nwp_Cw,Cw),
      (Snpt_nwp_awp_nwp_Cp1,Cp1),(Sp1t_awp_awp_awp_Cw,Cw),(Sp2t_awp_awp_awp_Cw,Cw),
      (Sp3t_awp_awp_awp_Cw,Cw),(Sp1t_awp_awp_awp_Cp3,Cp3),(Sp2t_awp_awp_awp_Cp3,Cp3),
      (Sp3t_awp_awp_awp_Cp3,Cp3),(Sp1t_awp_awp_awp_Cp2,Cp2),(Snpt_awp_nwp_awp_Cw,Cw),
      (Sp2t_awp_awp_awp_Cp2,Cp2),(Sp3t_awp_awp_awp_Cp2,Cp2),(Sp3t_awp_nwp_nwp_Cp3,Cp3),
      (Sp3t_awp_nwp_nwp_Cp2,Cp2),(Sp3t_awp_nwp_nwp_Cp1,Cp1),(Sp1t_nwp_awp_nwp_Cp3,Cp3),
      (Sp1t_nwp_awp_nwp_Cp2,Cp2),(Sp2t_nwp_awp_nwp_Cp3,Cp3),(Sp2t_nwp_awp_nwp_Cp2,Cp2),
      (Sp1t_awp_awp_awp_Cp1,Cp1),(Snpt_nwp_awp_awp_Cw,Cw),(Sp2t_awp_awp_awp_Cp1,Cp1),
      (Sp3t_awp_awp_awp_Cp1,Cp1),(Sp3t_nwp_awp_nwp_Cp3,Cp3),(Sp3t_nwp_awp_nwp_Cp2,Cp2),
      (Sp1t_nwp_awp_nwp_Cp1,Cp1),(Sp1t_awp_nwp_awp_Cw,Cw),(Snpt_awp_nwp_awp_Cp3,Cp3),
      (Sp2t_awp_nwp_awp_Cw,Cw),(Sp3t_awp_nwp_awp_Cw,Cw),(Snpt_awp_nwp_awp_Cp2,Cp2),
      (Snpt_awp_nwp_awp_Cp1,Cp1),(Sp3t_nwp_nwp_nwp_Cw,Cw),(Sp1t_nwp_awp_awp_Cw,Cw),
      (Snpt_nwp_awp_awp_Cp3,Cp3),(Sp2t_nwp_awp_awp_Cw,Cw),(Sp3t_nwp_awp_awp_Cw,Cw),
      (Snpt_nwp_awp_awp_Cp2,Cp2),(Sp1t_awp_nwp_awp_Cp3,Cp3),(Sp2t_awp_nwp_awp_Cp3,Cp3),
      (Sp3t_awp_nwp_awp_Cp3,Cp3),(Sp1t_awp_nwp_awp_Cp2,Cp2),(Sp2t_awp_nwp_awp_Cp2,Cp2),
      (Sp1t_awp_nwp_awp_Cp1,Cp1),(Snpt_nwp_nwp_awp_Cw,Cw),(Sp2t_awp_nwp_awp_Cp1,Cp1),
      (Sp3t_nwp_nwp_nwp_Cp3,Cp3),(Sp3t_nwp_nwp_nwp_Cp2,Cp2),(Sp3t_nwp_nwp_nwp_Cp1,Cp1),
      (Sp1t_nwp_awp_awp_Cp3,Cp3),(Sp2t_nwp_awp_awp_Cp3,Cp3),(Sp3t_nwp_awp_awp_Cp3,Cp3),
      (Sp1t_nwp_awp_awp_Cp2,Cp2),(Sp1t_nwp_nwp_awp_Cw,Cw),(Snpt_nwp_nwp_awp_Cp3,Cp3),
      (Sp2t_nwp_nwp_awp_Cw,Cw),(Snpt_nwp_nwp_awp_Cp2,Cp2),(Snpt_nwp_nwp_awp_Cp1,Cp1),
      (Sp1t_nwp_nwp_awp_Cp3,Cp3),(Sp2t_nwp_nwp_awp_Cp3,Cp3),(Sp1t_nwp_nwp_awp_Cp2,Cp2),
      (Sp1t_nwp_nwp_awp_Cp1,Cp1) }
 &

/*
  Standard constraints:
    1, 3, 5. The crane cannot attempt to pick-up a workpiece at the first/second/third
             position when there is no workpiece in the first/second/third buffer.
    2, 4, 6. The sorter cannot attempt to move a workpiece at the first/second/third
             position when there is a workpiece in the first/second/third buffer.
  Explicit constraints:
     7. Priority is given to the first buffer w.r.t. to the second buffer, when sorting
        a workpiece,
        The event "S_mp2" can only occur if the value of the state variable
        is <_,awp,_,_,_>.
     8. Priority is given to the first and second buffers w.r.t. the third buffer, when
        sorting a workpiece.
        The event "S_mp3" can only occur if the value of the state variable
        is <_,awp,awp,_,_>.
     9. Priority is given to the second and third buffers w.r.t. to the first buffer,
        when picking-up a workpiece.
        The event "C_cpk1" can only occur if the value of the state variable
        is <_,_,nwp,nwp,_>.
    10. Priority is given to the third buffer w.r.t. to the second buffer, when
        picking-up a workpiece.
        The event "C_cpk2" can only occur if the value of the state variable
        is <_,_,_,nwp,_>.
*/
 BadStates =
// There is no workpiece in the first buffer and the crane is picking-up a piece at the
// first position:
  { x | x : States & buffer1(x) = nwp & crane(x) = Cp1} \/
// There is workpiece in the first buffer and the sorter has targeted the first position:
  { x | x : States & buffer1(x) = awp & sorter(x) = Sp1t} \/
// There is no workpiece in the second buffer and the crane is picking-up a piece at the
// second position:
  { x | x : States & buffer2(x) = nwp & crane(x) = Cp2} \/
// There is workpiece in the second buffer and the sorter has targeted the second position:
  { x | x : States & buffer2(x) = awp & sorter(x) = Sp2t} \/
// There is no workpiece in the third buffer and the crane is picking-up a piece at the
// third position:
  { x | x : States & buffer3(x) = nwp & crane(x) = Cp3} \/
// There is workpiece in the third buffer and the sorter has targeted the third position:
  { x | x : States & buffer3(x) = awp & sorter(x) = Sp3t}
 &
 Qpred = States - BadStates
END

/*
OUTPUT:
The transition structure of the agent
 (i.e.,the solution of the control problem)
Number of states of the exhaustive model: 112
Number of transitions of the exhaustive model: 198
Cardinality of the predicate: 44
Supremal coreachable and controllable predicates (44):
{Snpt_nwp_nwp_nwp_Cw,Sp1t_nwp_nwp_nwp_Cw,Snpt_awp_nwp_nwp_Cw,
 Snpt_awp_nwp_nwp_Cp1,Sp2t_awp_nwp_nwp_Cw,Sp2t_awp_nwp_nwp_Cp1,
 Snpt_awp_awp_nwp_Cw,Snpt_awp_awp_nwp_Cp2,Snpt_awp_awp_nwp_Cp1,
 Sp2t_nwp_nwp_nwp_Cw,Sp3t_awp_awp_nwp_Cw,Sp3t_awp_awp_nwp_Cp2,
 Snpt_nwp_awp_nwp_Cw,Sp3t_awp_awp_nwp_Cp1,Snpt_awp_awp_awp_Cw,
 Snpt_awp_awp_awp_Cp3,Snpt_awp_awp_awp_Cp2,Sp3t_awp_nwp_nwp_Cw,
 Sp1t_nwp_awp_nwp_Cw,Snpt_nwp_awp_nwp_Cp2,Snpt_awp_awp_awp_Cp1,
 Sp3t_nwp_awp_nwp_Cw,Snpt_awp_nwp_awp_Cw,Sp3t_awp_nwp_nwp_Cp1,
 Sp1t_nwp_awp_nwp_Cp2,Snpt_nwp_awp_awp_Cw,Sp3t_nwp_awp_nwp_Cp2,
 Snpt_awp_nwp_awp_Cp3,Sp2t_awp_nwp_awp_Cw,Snpt_awp_nwp_awp_Cp1,
 Sp3t_nwp_nwp_nwp_Cw,Sp1t_nwp_awp_awp_Cw,Snpt_nwp_awp_awp_Cp3,
 Snpt_nwp_awp_awp_Cp2,Sp2t_awp_nwp_awp_Cp3,Snpt_nwp_nwp_awp_Cw,
 Sp2t_awp_nwp_awp_Cp1,Sp1t_nwp_awp_awp_Cp3,Sp1t_nwp_awp_awp_Cp2,
 Sp1t_nwp_nwp_awp_Cw,Snpt_nwp_nwp_awp_Cp3,Sp2t_nwp_nwp_awp_Cw,
 Sp1t_nwp_nwp_awp_Cp3,Sp2t_nwp_nwp_awp_Cp3}
Disabled events:
{(Snpt_nwp_nwp_nwp_Cw|->{C_pck1,C_pck2,C_pck3}),
 (Sp1t_nwp_nwp_nwp_Cw|->{C_pck1,C_pck2,C_pck3}),
 (Snpt_awp_nwp_nwp_Cw|->{S_mp1,C_pck2,C_pck3}),
 (Snpt_awp_nwp_nwp_Cp1|->{S_mp1}),(Sp2t_awp_nwp_nwp_Cw|->{C_pck2,C_pck3}),
 (Sp2t_awp_nwp_nwp_Cp1|->{}),(Snpt_awp_awp_nwp_Cw|->{S_mp1,S_mp2,C_pck3}),
 (Snpt_awp_awp_nwp_Cp2|->{S_mp1,S_mp2}),(Snpt_awp_awp_nwp_Cp1|->{S_mp1,S_mp2}),
 (Sp2t_nwp_nwp_nwp_Cw|->{C_pck1,C_pck2,C_pck3}),(Sp3t_awp_awp_nwp_Cw|->{C_pck3}),
 (Sp3t_awp_awp_nwp_Cp2|->{}),(Snpt_nwp_awp_nwp_Cw|->{C_pck3}),
 (Sp3t_awp_awp_nwp_Cp1|->{}),(Snpt_awp_awp_awp_Cw|->{S_mp1,S_mp2,S_mp3}),
 (Snpt_awp_awp_awp_Cp3|->{S_mp1,S_mp2,S_mp3}),
 (Snpt_awp_awp_awp_Cp2|->{S_mp1,S_mp2,S_mp3}),
 (Sp3t_awp_nwp_nwp_Cw|->{C_pck2,C_pck3}),(Sp1t_nwp_awp_nwp_Cw|->{C_pck3}),
 (Snpt_nwp_awp_nwp_Cp2|->{}),(Snpt_awp_awp_awp_Cp1|->{S_mp1,S_mp2,S_mp3}),
 (Sp3t_nwp_awp_nwp_Cw|->{C_pck3}),(Snpt_awp_nwp_awp_Cw|->{S_mp1}),
 (Sp3t_awp_nwp_nwp_Cp1|->{}),(Sp1t_nwp_awp_nwp_Cp2|->{}),
 (Snpt_nwp_awp_awp_Cw|->{}),(Sp3t_nwp_awp_nwp_Cp2|->{}),
 (Snpt_awp_nwp_awp_Cp3|->{S_mp1}),(Sp2t_awp_nwp_awp_Cw|->{}),
 (Snpt_awp_nwp_awp_Cp1|->{S_mp1}),
 (Sp3t_nwp_nwp_nwp_Cw|->{C_pck1,C_pck2,C_pck3}),
 (Sp1t_nwp_awp_awp_Cw|->{}),(Snpt_nwp_awp_awp_Cp3|->{}),
 (Snpt_nwp_awp_awp_Cp2|->{}),(Sp2t_awp_nwp_awp_Cp3|->{}),
 (Snpt_nwp_nwp_awp_Cw|->{}),(Sp2t_awp_nwp_awp_Cp1|->{}),
 (Sp1t_nwp_awp_awp_Cp3|->{}),(Sp1t_nwp_awp_awp_Cp2|->{}),
 (Sp1t_nwp_nwp_awp_Cw|->{}),(Snpt_nwp_nwp_awp_Cp3|->{}),
 (Sp2t_nwp_nwp_awp_Cw|->{}),(Sp1t_nwp_nwp_awp_Cp3|->{}),(Sp2t_nwp_nwp_awp_Cp3|->{})}
Enabled events:
{(Snpt_nwp_nwp_nwp_Cw|->{S_mp1}),(Sp1t_nwp_nwp_nwp_Cw|->{S_pap1}),
 (Snpt_awp_nwp_nwp_Cw|->{S_mp2,C_pck1}),(Snpt_awp_nwp_nwp_Cp1|->{S_mp2,C_eop1}),
 (Sp2t_awp_nwp_nwp_Cw|->{S_pap2,C_pck1}),(Sp2t_awp_nwp_nwp_Cp1|->{S_pap2,C_eop1}),
 (Snpt_awp_awp_nwp_Cw|->{S_mp3,C_pck2}),(Snpt_awp_awp_nwp_Cp2|->{S_mp3,C_eop2}),
 (Snpt_awp_awp_nwp_Cp1|->{S_mp3,C_eop1}),(Sp2t_nwp_nwp_nwp_Cw|->{S_pap2}),
 (Sp3t_awp_awp_nwp_Cw|->{S_pap3,C_pck2}),(Sp3t_awp_awp_nwp_Cp2|->{S_pap3,C_eop2}),
 (Snpt_nwp_awp_nwp_Cw|->{S_mp1,C_pck2}),(Sp3t_awp_awp_nwp_Cp1|->{S_pap3,C_eop1}),
 (Snpt_awp_awp_awp_Cw|->{C_pck3}),(Snpt_awp_awp_awp_Cp3|->{C_eop3}),
 (Snpt_awp_awp_awp_Cp2|->{C_eop2}),(Sp3t_awp_nwp_nwp_Cw|->{S_pap3,C_pck1}),
 (Sp1t_nwp_awp_nwp_Cw|->{S_pap1,C_pck2}),(Snpt_nwp_awp_nwp_Cp2|->{S_mp1,C_eop2}),
 (Snpt_awp_awp_awp_Cp1|->{C_eop1}),(Sp3t_nwp_awp_nwp_Cw|->{S_pap3,C_pck2}),
 (Snpt_awp_nwp_awp_Cw|->{S_mp2,C_pck3}),(Sp3t_awp_nwp_nwp_Cp1|->{S_pap3,C_eop1}),
 (Sp1t_nwp_awp_nwp_Cp2|->{S_pap1,C_eop2}),(Snpt_nwp_awp_awp_Cw|->{S_mp1,C_pck3}),
 (Sp3t_nwp_awp_nwp_Cp2|->{S_pap3,C_eop2}),(Snpt_awp_nwp_awp_Cp3|->{S_mp2,C_eop3}),
 (Sp2t_awp_nwp_awp_Cw|->{S_pap2,C_pck3}),(Snpt_awp_nwp_awp_Cp1|->{S_mp2,C_eop1}),
 (Sp3t_nwp_nwp_nwp_Cw|->{S_pap3}),(Sp1t_nwp_awp_awp_Cw|->{S_pap1,C_pck3}),
 (Snpt_nwp_awp_awp_Cp3|->{S_mp1,C_eop3}),(Snpt_nwp_awp_awp_Cp2|->{S_mp1,C_eop2}),
 (Sp2t_awp_nwp_awp_Cp3|->{S_pap2,C_eop3}),(Snpt_nwp_nwp_awp_Cw|->{S_mp1,C_pck3}),
 (Sp2t_awp_nwp_awp_Cp1|->{S_pap2,C_eop1}),(Sp1t_nwp_awp_awp_Cp3|->{S_pap1,C_eop3}),
 (Sp1t_nwp_awp_awp_Cp2|->{S_pap1,C_eop2}),(Sp1t_nwp_nwp_awp_Cw|->{S_pap1,C_pck3}),
 (Snpt_nwp_nwp_awp_Cp3|->{S_mp1,C_eop3}),(Sp2t_nwp_nwp_awp_Cw|->{S_pap2,C_pck3}),
 (Sp1t_nwp_nwp_awp_Cp3|->{S_pap1,C_eop3}),(Sp2t_nwp_nwp_awp_Cp3|->{S_pap2,C_eop3})}
Closed-loop system:
 rec(Xm:{Snpt_nwp_nwp_nwp_Cw},
     ctrlEvents:{S_mp1,S_mp2,S_mp3,C_pck1,C_pck2,C_pck3},
     events:{S_mp1,S_mp2,S_mp3,S_pap1,S_pap2,S_pap3,
             C_eop1,C_eop2,C_pck1,C_eop3,C_pck2,C_pck3},
     states:{Snpt_nwp_nwp_nwp_Cw,Sp1t_nwp_nwp_nwp_Cw,Snpt_awp_nwp_nwp_Cw,
             Snpt_awp_nwp_nwp_Cp1,Sp2t_awp_nwp_nwp_Cw,Sp2t_awp_nwp_nwp_Cp1,
             Snpt_awp_awp_nwp_Cw,Snpt_awp_awp_nwp_Cp2,Snpt_awp_awp_nwp_Cp1,
             Sp2t_nwp_nwp_nwp_Cw,Sp3t_awp_awp_nwp_Cw,Sp3t_awp_awp_nwp_Cp2,
             Snpt_nwp_awp_nwp_Cw,Sp3t_awp_awp_nwp_Cp1,Snpt_awp_awp_awp_Cw,
             Snpt_awp_awp_awp_Cp3,Snpt_awp_awp_awp_Cp2,Sp3t_awp_nwp_nwp_Cw,
             Sp1t_nwp_awp_nwp_Cw,Snpt_nwp_awp_nwp_Cp2,Snpt_awp_awp_awp_Cp1,
             Sp3t_nwp_awp_nwp_Cw,Snpt_awp_nwp_awp_Cw,Sp3t_awp_nwp_nwp_Cp1,
             Sp1t_nwp_awp_nwp_Cp2,Snpt_nwp_awp_awp_Cw,Sp3t_nwp_awp_nwp_Cp2,
             Snpt_awp_nwp_awp_Cp3,Sp2t_awp_nwp_awp_Cw,Snpt_awp_nwp_awp_Cp1,
             Sp3t_nwp_nwp_nwp_Cw,Sp1t_nwp_awp_awp_Cw,Snpt_nwp_awp_awp_Cp3,
             Snpt_nwp_awp_awp_Cp2,Sp2t_awp_nwp_awp_Cp3,Snpt_nwp_nwp_awp_Cw,
             Sp2t_awp_nwp_awp_Cp1,Sp1t_nwp_awp_awp_Cp3,Sp1t_nwp_awp_awp_Cp2,
             Sp1t_nwp_nwp_awp_Cw,Snpt_nwp_nwp_awp_Cp3,Sp2t_nwp_nwp_awp_Cw,
             Sp1t_nwp_nwp_awp_Cp3,Sp2t_nwp_nwp_awp_Cp3},
     tf:{((Snpt_nwp_nwp_nwp_Cw|->S_mp1)|->Sp1t_nwp_nwp_nwp_Cw),
         ((Sp1t_nwp_nwp_nwp_Cw|->S_pap1)|->Snpt_awp_nwp_nwp_Cw),
         ((Snpt_awp_nwp_nwp_Cw|->S_mp2)|->Sp2t_awp_nwp_nwp_Cw),
         ((Snpt_awp_nwp_nwp_Cw|->C_pck1)|->Snpt_awp_nwp_nwp_Cp1),
         ((Snpt_awp_nwp_nwp_Cp1|->S_mp2)|->Sp2t_awp_nwp_nwp_Cp1),
         ((Snpt_awp_nwp_nwp_Cp1|->C_eop1)|->Snpt_nwp_nwp_nwp_Cw),
         ((Sp2t_awp_nwp_nwp_Cw|->S_pap2)|->Snpt_awp_awp_nwp_Cw),
         ((Sp2t_awp_nwp_nwp_Cw|->C_pck1)|->Sp2t_awp_nwp_nwp_Cp1),
         ((Sp2t_awp_nwp_nwp_Cp1|->S_pap2)|->Snpt_awp_awp_nwp_Cp1),
         ((Sp2t_awp_nwp_nwp_Cp1|->C_eop1)|->Sp2t_nwp_nwp_nwp_Cw),
         ((Snpt_awp_awp_nwp_Cw|->S_mp3)|->Sp3t_awp_awp_nwp_Cw),
         ((Snpt_awp_awp_nwp_Cw|->C_pck2)|->Snpt_awp_awp_nwp_Cp2),
         ((Snpt_awp_awp_nwp_Cp2|->S_mp3)|->Sp3t_awp_awp_nwp_Cp2),
         ((Snpt_awp_awp_nwp_Cp2|->C_eop2)|->Snpt_awp_nwp_nwp_Cw),
         ((Snpt_awp_awp_nwp_Cp1|->S_mp3)|->Sp3t_awp_awp_nwp_Cp1),
         ((Snpt_awp_awp_nwp_Cp1|->C_eop1)|->Snpt_nwp_awp_nwp_Cw),
         ((Sp2t_nwp_nwp_nwp_Cw|->S_pap2)|->Snpt_nwp_awp_nwp_Cw),
         ((Sp3t_awp_awp_nwp_Cw|->S_pap3)|->Snpt_awp_awp_awp_Cw),
         ((Sp3t_awp_awp_nwp_Cw|->C_pck2)|->Sp3t_awp_awp_nwp_Cp2),
         ((Sp3t_awp_awp_nwp_Cp2|->S_pap3)|->Snpt_awp_awp_awp_Cp2),
         ((Sp3t_awp_awp_nwp_Cp2|->C_eop2)|->Sp3t_awp_nwp_nwp_Cw),
         ((Snpt_nwp_awp_nwp_Cw|->S_mp1)|->Sp1t_nwp_awp_nwp_Cw),
         ((Snpt_nwp_awp_nwp_Cw|->C_pck2)|->Snpt_nwp_awp_nwp_Cp2),
         ((Sp3t_awp_awp_nwp_Cp1|->S_pap3)|->Snpt_awp_awp_awp_Cp1),
         ((Sp3t_awp_awp_nwp_Cp1|->C_eop1)|->Sp3t_nwp_awp_nwp_Cw),
         ((Snpt_awp_awp_awp_Cw|->C_pck3)|->Snpt_awp_awp_awp_Cp3),
         ((Snpt_awp_awp_awp_Cp3|->C_eop3)|->Snpt_awp_awp_nwp_Cw),
         ((Snpt_awp_awp_awp_Cp2|->C_eop2)|->Snpt_awp_nwp_awp_Cw),
         ((Sp3t_awp_nwp_nwp_Cw|->S_pap3)|->Snpt_awp_nwp_awp_Cw),
         ((Sp3t_awp_nwp_nwp_Cw|->C_pck1)|->Sp3t_awp_nwp_nwp_Cp1),
         ((Sp1t_nwp_awp_nwp_Cw|->S_pap1)|->Snpt_awp_awp_nwp_Cw),
         ((Sp1t_nwp_awp_nwp_Cw|->C_pck2)|->Sp1t_nwp_awp_nwp_Cp2),
         ((Snpt_nwp_awp_nwp_Cp2|->S_mp1)|->Sp1t_nwp_awp_nwp_Cp2),
         ((Snpt_nwp_awp_nwp_Cp2|->C_eop2)|->Snpt_nwp_nwp_nwp_Cw),
         ((Snpt_awp_awp_awp_Cp1|->C_eop1)|->Snpt_nwp_awp_awp_Cw),
         ((Sp3t_nwp_awp_nwp_Cw|->S_pap3)|->Snpt_nwp_awp_awp_Cw),
         ((Sp3t_nwp_awp_nwp_Cw|->C_pck2)|->Sp3t_nwp_awp_nwp_Cp2),
         ((Snpt_awp_nwp_awp_Cw|->S_mp2)|->Sp2t_awp_nwp_awp_Cw),
         ((Snpt_awp_nwp_awp_Cw|->C_pck3)|->Snpt_awp_nwp_awp_Cp3),
         ((Sp3t_awp_nwp_nwp_Cp1|->S_pap3)|->Snpt_awp_nwp_awp_Cp1),
         ((Sp3t_awp_nwp_nwp_Cp1|->C_eop1)|->Sp3t_nwp_nwp_nwp_Cw),
         ((Sp1t_nwp_awp_nwp_Cp2|->S_pap1)|->Snpt_awp_awp_nwp_Cp2),
         ((Sp1t_nwp_awp_nwp_Cp2|->C_eop2)|->Sp1t_nwp_nwp_nwp_Cw),
         ((Snpt_nwp_awp_awp_Cw|->S_mp1)|->Sp1t_nwp_awp_awp_Cw),
         ((Snpt_nwp_awp_awp_Cw|->C_pck3)|->Snpt_nwp_awp_awp_Cp3),
         ((Sp3t_nwp_awp_nwp_Cp2|->S_pap3)|->Snpt_nwp_awp_awp_Cp2),
         ((Sp3t_nwp_awp_nwp_Cp2|->C_eop2)|->Sp3t_nwp_nwp_nwp_Cw),
         ((Snpt_awp_nwp_awp_Cp3|->S_mp2)|->Sp2t_awp_nwp_awp_Cp3),
         ((Snpt_awp_nwp_awp_Cp3|->C_eop3)|->Snpt_awp_nwp_nwp_Cw),
         ((Sp2t_awp_nwp_awp_Cw|->S_pap2)|->Snpt_awp_awp_awp_Cw),
         ((Sp2t_awp_nwp_awp_Cw|->C_pck3)|->Sp2t_awp_nwp_awp_Cp3),
         ((Snpt_awp_nwp_awp_Cp1|->S_mp2)|->Sp2t_awp_nwp_awp_Cp1),
         ((Snpt_awp_nwp_awp_Cp1|->C_eop1)|->Snpt_nwp_nwp_awp_Cw),
         ((Sp3t_nwp_nwp_nwp_Cw|->S_pap3)|->Snpt_nwp_nwp_awp_Cw),
         ((Sp1t_nwp_awp_awp_Cw|->S_pap1)|->Snpt_awp_awp_awp_Cw),
         ((Sp1t_nwp_awp_awp_Cw|->C_pck3)|->Sp1t_nwp_awp_awp_Cp3),
         ((Snpt_nwp_awp_awp_Cp3|->S_mp1)|->Sp1t_nwp_awp_awp_Cp3),
         ((Snpt_nwp_awp_awp_Cp3|->C_eop3)|->Snpt_nwp_awp_nwp_Cw),
         ((Snpt_nwp_awp_awp_Cp2|->S_mp1)|->Sp1t_nwp_awp_awp_Cp2),
         ((Snpt_nwp_awp_awp_Cp2|->C_eop2)|->Snpt_nwp_nwp_awp_Cw),
         ((Sp2t_awp_nwp_awp_Cp3|->S_pap2)|->Snpt_awp_awp_awp_Cp3),
         ((Sp2t_awp_nwp_awp_Cp3|->C_eop3)|->Sp2t_awp_nwp_nwp_Cw),
         ((Snpt_nwp_nwp_awp_Cw|->S_mp1)|->Sp1t_nwp_nwp_awp_Cw),
         ((Snpt_nwp_nwp_awp_Cw|->C_pck3)|->Snpt_nwp_nwp_awp_Cp3),
         ((Sp2t_awp_nwp_awp_Cp1|->S_pap2)|->Snpt_awp_awp_awp_Cp1),
         ((Sp2t_awp_nwp_awp_Cp1|->C_eop1)|->Sp2t_nwp_nwp_awp_Cw),
         ((Sp1t_nwp_awp_awp_Cp3|->S_pap1)|->Snpt_awp_awp_awp_Cp3),
         ((Sp1t_nwp_awp_awp_Cp3|->C_eop3)|->Sp1t_nwp_awp_nwp_Cw),
         ((Sp1t_nwp_awp_awp_Cp2|->S_pap1)|->Snpt_awp_awp_awp_Cp2),
         ((Sp1t_nwp_awp_awp_Cp2|->C_eop2)|->Sp1t_nwp_nwp_awp_Cw),
         ((Sp1t_nwp_nwp_awp_Cw|->S_pap1)|->Snpt_awp_nwp_awp_Cw),
         ((Sp1t_nwp_nwp_awp_Cw|->C_pck3)|->Sp1t_nwp_nwp_awp_Cp3),
         ((Snpt_nwp_nwp_awp_Cp3|->S_mp1)|->Sp1t_nwp_nwp_awp_Cp3),
         ((Snpt_nwp_nwp_awp_Cp3|->C_eop3)|->Snpt_nwp_nwp_nwp_Cw),
         ((Sp2t_nwp_nwp_awp_Cw|->S_pap2)|->Snpt_nwp_awp_awp_Cw),
         ((Sp2t_nwp_nwp_awp_Cw|->C_pck3)|->Sp2t_nwp_nwp_awp_Cp3),
         ((Sp1t_nwp_nwp_awp_Cp3|->S_pap1)|->Snpt_awp_nwp_awp_Cp3),
         ((Sp1t_nwp_nwp_awp_Cp3|->C_eop3)|->Sp1t_nwp_nwp_nwp_Cw),
         ((Sp2t_nwp_nwp_awp_Cp3|->S_pap2)|->Snpt_nwp_awp_awp_Cp3),
         ((Sp2t_nwp_nwp_awp_Cp3|->C_eop3)|->Sp2t_nwp_nwp_nwp_Cw)},
     x_0:Snpt_nwp_nwp_nwp_Cw)
*/

/*
  ControlLaw(S_mp1) = Snpt & nwp_buffer1
  ControlLaw(S_mp2) = Snpt & awp_buffer1 & nwp_buffer2
  ControlLaw(S_mp3) = Snpt & awp_buffer1 & awp_buffer2 & nwp_buffer3
  ControlLaw(C_pck1) = Cw & awp_buffer1 & nwp_buffer2 & nwp_buffer3
  ControlLaw(C_pck2) = Cw & awp_buffer2 & nwp_buffer3
  ControlLaw(C_pck3) = Cw & awp_buffer3
*/

/*
% Runtime for SOLUTION for SETUP_CONSTANTS: 100 ms (walltime: 100 ms)
% Finished searching for valid initial states 120 ms walltime (120 ms runtime), since start: 1900 ms
% Starting random_animate (max_steps=5)
perform_random_step(1,1)
random(2,1)
perform_random_step(1,2)
random(2,1)
perform_random_step(1,3)
random(2,1)
"Compute supCCP..."
"Compute state feedback functions..."
"Save results..."
perform_random_step(1,4)
random(2,1)
"The control is nonconflicting."

ALL OPERATIONS COVERED

perform_random_step(1,5)
random(2,1)
finished_random_animate(5,[runtime/1130,total_runtime/1130,walltime/1140])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 1320 ms walltime (1300 ms runtime), since start: 3050 ms

% Finished process_file (errors=0, warnings=0)
*/