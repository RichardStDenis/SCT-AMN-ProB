// copyright Richard St_Denis, Universite de Sherbrooke, 2023.
MODEL SBdistribution

SETS
 States = {s0_Ir_CatR, s1_Iwim_CatR, s1_Iie_CatR,
           s2_Ie_CatR, s2_Ie_CnrL, s3_Ie_CatL,
           s3_Ie_CnrR, s3_Ie_CgWp, s3_Ie_CatR,
           s4_Ie_CaWp, s5_Ir_CaWp, s4_Ie_CnrRaWp,
           s6_Iwim_CaWp, s5_Ir_CnrRaWp, s4_Ie_CatRaWp,
           s6_Iwim_CnrRaWp, s6_Iie_CaWp, s7_Ir_CatRaWp,
           s5_Ir_CatRaWp, s4_Ie_CdWp, s8_Iwim_CatRaWp,
           s6_Iie_CnrRaWp, s6_Ie_CaWp, s7_Ir_CdWp,
           s6_Iwim_CatRaWp, s5_Ir_CdWp, s4_Ie_CatR,
           s8_Iie_CatRaWp, s8_Iwim_CdWp, s6_Ie_CnrRaWp,
           s7_Ir_CatR, s6_Iwim_CdWp, s6_Iie_CatRaWp,
           s5_Ir_CatR, s9_Ie_CatRaWp, s8_Iie_CdWp,
           s8_Iwim_CatR, s8_Ie_CatRaWp, s11_Ir_CnrL,
           s6_Iwim_CatR, s6_Iie_CdWp, s6_Ie_CatRaWp,
           s9_Ie_CdWp, s8_Iie_CatR, s8_Ie_CdWp,
           s12_Iwim_CnrL, s13_Ir_CatL, s6_Iie_CatR,
           s6_Ie_CdWp, s9_Ie_CatR, s8_Ie_CatR,
           s14_Iwim_CatL, s12_Iie_CnrL, s16_Ir_CatL,
           s13_Ir_CnrR, s13_Ir_CgWp, s6_Ie_CatR,
           s9_Ie_CnrL, s15_Iwim_CatL, s14_Iwim_CnrR,
           s14_Iwim_CgWp, s14_Iie_CatL, s12_Ie_CnrL,
           s16_Ir_CnrR, s16_Ir_CgWp, s13_Ir_CatR,
           s13_Ir_CaWp, s10_Ie_CatL, s15_Iwim_CnrR,
           s15_Iwim_CgWp, s15_Iie_CatL, s14_Iie_CnrR,
           s14_Iwim_CatR, s14_Iie_CgWp, s14_Iwim_CaWp,
           s14_Ie_CatL, s16_Ir_CaWp, s16_Ir_CatR,
           s13_Ir_CnrRaWp, s10_Ie_CnrR, s10_Ie_CgWp,
           s15_Iie_CnrR, s15_Iie_CgWp, s15_Iwim_CaWp,
           s15_Ie_CatL, s14_Ie_CnrR, s14_Iie_CatR,
           s15_Iwim_CatR, s14_Ie_CgWp, s14_Iie_CaWp,
           s14_Iwim_CnrRaWp, s16_Ir_CnrRaWp, s13_Ir_CatRaWp,
           s10_Ie_CatR, s10_Ie_CaWp, s15_Ie_CnrR,
           s15_Ie_CgWp, s15_Iie_CaWp, s15_Iwim_CnrRaWp,
           s14_Ie_CatR, s15_Iie_CatR, s14_Ie_CaWp,
           s14_Iie_CnrRaWp, s14_Iwim_CatRaWp, s0_Ir_CatRaWp,
           s16_Ir_CatRaWp, s13_Ir_CdWp, s3_Ie_CaWp,
           s10_Ie_CnrRaWp, s1_Ie_CatR, s15_Ie_CaWp,
           s15_Iie_CnrRaWp, s1_Iwim_CatRaWp, s15_Ie_CatR,
           s14_Ie_CnrRaWp, s14_Iie_CatRaWp, s15_Iwim_CatRaWp,
           s14_Iwim_CdWp, s0_Ir_CdWp, s16_Ir_CdWp,
           s3_Ie_CnrRaWp, s10_Ie_CatRaWp, s15_Ie_CnrRaWp,
           s1_Iie_CatRaWp, s1_Iwim_CdWp, s14_Ie_CatRaWp,
           s15_Iie_CatRaWp, s14_Iie_CdWp, s15_Iwim_CdWp,
           s3_Ie_CatRaWp, s10_Ie_CdWp, s1_Ie_CatRaWp,
           s2_Ie_CatRaWp, s1_Iie_CdWp, s15_Ie_CatRaWp,
           s14_Ie_CdWp, s15_Iie_CdWp, s3_Ie_CdWp,
           s1_Ie_CdWp, s2_Ie_CdWp, s15_Ie_CdWp };
 Events = {reset, I_eoi, I_rearm, I_wpa, C_eog, C_eogl, C_eogr, C_gl, I_inject, C_drop, C_eod, C_grab, C_gr};
 LocalStates = {s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, Ie, Iie, Ir, Iwim, CatL, CatR, CatRaWp, CaWp, CdWp, CgWp, CnrL, CnrR, CnrRaWp}

CONSTANTS
 G_FB,
 seqD, injector, crane,
 Qpred, BadStates

PROPERTIES
 G_FB = rec(       // Automaton modeling the free behavior
            states: States,
            events: Events,
            tf: {(s0_Ir_CatR,I_wpa,s1_Iwim_CatR), (s1_Iwim_CatR,I_inject,s1_Iie_CatR), (s1_Iie_CatR,I_eoi,s2_Ie_CatR),
                 (s2_Ie_CatR,C_gl,s2_Ie_CnrL), (s2_Ie_CnrL,C_eogl,s3_Ie_CatL), (s3_Ie_CatL,C_gr,s3_Ie_CnrR),
                 (s3_Ie_CatL,C_grab,s3_Ie_CgWp), (s3_Ie_CnrR,C_eogr,s3_Ie_CatR), (s3_Ie_CgWp,C_eog,s4_Ie_CaWp),
                 (s4_Ie_CaWp,I_rearm,s5_Ir_CaWp), (s4_Ie_CaWp,C_gr,s4_Ie_CnrRaWp), (s5_Ir_CaWp,I_wpa,s6_Iwim_CaWp),
                 (s5_Ir_CaWp,C_gr,s5_Ir_CnrRaWp), (s4_Ie_CnrRaWp,I_rearm,s5_Ir_CnrRaWp), (s4_Ie_CnrRaWp,C_eogr,s4_Ie_CatRaWp),
                 (s6_Iwim_CaWp,C_gr,s6_Iwim_CnrRaWp), (s6_Iwim_CaWp,I_inject,s6_Iie_CaWp), (s5_Ir_CnrRaWp,I_wpa,s6_Iwim_CnrRaWp),
                 (s5_Ir_CnrRaWp,C_eogr,s7_Ir_CatRaWp), (s4_Ie_CatRaWp,I_rearm,s5_Ir_CatRaWp), (s4_Ie_CatRaWp,C_drop,s4_Ie_CdWp),
                 (s6_Iwim_CnrRaWp,C_eogr,s8_Iwim_CatRaWp), (s6_Iwim_CnrRaWp,I_inject,s6_Iie_CnrRaWp), (s6_Iie_CaWp,C_gr,s6_Iie_CnrRaWp),
                 (s6_Iie_CaWp,I_eoi,s6_Ie_CaWp), (s7_Ir_CatRaWp,C_drop,s7_Ir_CdWp), (s7_Ir_CatRaWp,I_wpa,s8_Iwim_CatRaWp),
                 (s5_Ir_CatRaWp,I_wpa,s6_Iwim_CatRaWp), (s5_Ir_CatRaWp,C_drop,s5_Ir_CdWp), (s4_Ie_CdWp,I_rearm,s5_Ir_CdWp),
                 (s4_Ie_CdWp,C_eod,s4_Ie_CatR), (s8_Iwim_CatRaWp,I_inject,s8_Iie_CatRaWp), (s8_Iwim_CatRaWp,C_drop,s8_Iwim_CdWp),
                 (s6_Iie_CnrRaWp,C_eogr,s8_Iie_CatRaWp), (s6_Iie_CnrRaWp,I_eoi,s6_Ie_CnrRaWp), (s6_Ie_CaWp,C_gr,s6_Ie_CnrRaWp),
                 (s7_Ir_CdWp,C_eod,s7_Ir_CatR), (s7_Ir_CdWp,I_wpa,s8_Iwim_CdWp), (s6_Iwim_CatRaWp,C_drop,s6_Iwim_CdWp),
                 (s6_Iwim_CatRaWp,I_inject,s6_Iie_CatRaWp), (s5_Ir_CdWp,I_wpa,s6_Iwim_CdWp), (s5_Ir_CdWp,C_eod,s5_Ir_CatR),
                 (s4_Ie_CatR,I_rearm,s5_Ir_CatR), (s8_Iie_CatRaWp,I_eoi,s9_Ie_CatRaWp), (s8_Iie_CatRaWp,C_drop,s8_Iie_CdWp),
                 (s8_Iwim_CdWp,I_inject,s8_Iie_CdWp), (s8_Iwim_CdWp,C_eod,s8_Iwim_CatR), (s6_Ie_CnrRaWp,C_eogr,s8_Ie_CatRaWp),
                 (s7_Ir_CatR,C_gl,s11_Ir_CnrL), (s7_Ir_CatR,I_wpa,s8_Iwim_CatR), (s6_Iwim_CdWp,C_eod,s6_Iwim_CatR),
                 (s6_Iwim_CdWp,I_inject,s6_Iie_CdWp), (s6_Iie_CatRaWp,C_drop,s6_Iie_CdWp), (s6_Iie_CatRaWp,I_eoi,s6_Ie_CatRaWp),
                 (s5_Ir_CatR,I_wpa,s6_Iwim_CatR), (s9_Ie_CatRaWp,C_drop,s9_Ie_CdWp), (s8_Iie_CdWp,I_eoi,s9_Ie_CdWp),
                 (s8_Iie_CdWp,C_eod,s8_Iie_CatR), (s8_Iwim_CatR,I_inject,s8_Iie_CatR), (s8_Ie_CatRaWp,C_drop,s8_Ie_CdWp),
                 (s11_Ir_CnrL,I_wpa,s12_Iwim_CnrL), (s11_Ir_CnrL,C_eogl,s13_Ir_CatL), (s6_Iwim_CatR,I_inject,s6_Iie_CatR),
                 (s6_Iie_CdWp,C_eod,s6_Iie_CatR), (s6_Iie_CdWp,I_eoi,s6_Ie_CdWp), (s6_Ie_CatRaWp,C_drop,s6_Ie_CdWp),
                 (s9_Ie_CdWp,C_eod,s9_Ie_CatR), (s8_Iie_CatR,I_eoi,s9_Ie_CatR), (s8_Ie_CdWp,C_eod,s8_Ie_CatR),
                 (s12_Iwim_CnrL,C_eogl,s14_Iwim_CatL), (s12_Iwim_CnrL,I_inject,s12_Iie_CnrL), (s13_Ir_CatL,reset,s16_Ir_CatL),
                 (s13_Ir_CatL,C_gr,s13_Ir_CnrR), (s13_Ir_CatL,C_grab,s13_Ir_CgWp), (s13_Ir_CatL,I_wpa,s14_Iwim_CatL),
                 (s6_Iie_CatR,I_eoi,s6_Ie_CatR), (s6_Ie_CdWp,C_eod,s6_Ie_CatR), (s9_Ie_CatR,C_gl,s9_Ie_CnrL),
                 (s14_Iwim_CatL,reset,s15_Iwim_CatL), (s14_Iwim_CatL,C_gr,s14_Iwim_CnrR), (s14_Iwim_CatL,C_grab,s14_Iwim_CgWp),
                 (s14_Iwim_CatL,I_inject,s14_Iie_CatL), (s12_Iie_CnrL,C_eogl,s14_Iie_CatL), (s12_Iie_CnrL,I_eoi,s12_Ie_CnrL),
                 (s16_Ir_CatL,I_wpa,s15_Iwim_CatL), (s16_Ir_CatL,C_gr,s16_Ir_CnrR), (s16_Ir_CatL,C_grab,s16_Ir_CgWp),
                 (s13_Ir_CnrR,reset,s16_Ir_CnrR), (s13_Ir_CnrR,I_wpa,s14_Iwim_CnrR), (s13_Ir_CnrR,C_eogr,s13_Ir_CatR),
                 (s13_Ir_CgWp,reset,s16_Ir_CgWp), (s13_Ir_CgWp,I_wpa,s14_Iwim_CgWp), (s13_Ir_CgWp,C_eog,s13_Ir_CaWp),
                 (s9_Ie_CnrL,C_eogl,s10_Ie_CatL), (s15_Iwim_CatL,C_gr,s15_Iwim_CnrR), (s15_Iwim_CatL,C_grab,s15_Iwim_CgWp),
                 (s15_Iwim_CatL,I_inject,s15_Iie_CatL), (s14_Iwim_CnrR,reset,s15_Iwim_CnrR), (s14_Iwim_CnrR,I_inject,s14_Iie_CnrR),
                 (s14_Iwim_CnrR,C_eogr,s14_Iwim_CatR), (s14_Iwim_CgWp,reset,s15_Iwim_CgWp), (s14_Iwim_CgWp,I_inject,s14_Iie_CgWp),
                 (s14_Iwim_CgWp,C_eog,s14_Iwim_CaWp), (s14_Iie_CatL,reset,s15_Iie_CatL), (s14_Iie_CatL,C_gr,s14_Iie_CnrR),
                 (s14_Iie_CatL,C_grab,s14_Iie_CgWp), (s14_Iie_CatL,I_eoi,s14_Ie_CatL), (s12_Ie_CnrL,C_eogl,s14_Ie_CatL),
                 (s16_Ir_CnrR,I_wpa,s15_Iwim_CnrR), (s16_Ir_CnrR,C_eogr,s0_Ir_CatR), (s16_Ir_CgWp,I_wpa,s15_Iwim_CgWp),
                 (s16_Ir_CgWp,C_eog,s16_Ir_CaWp), (s13_Ir_CatR,reset,s16_Ir_CatR), (s13_Ir_CatR,I_wpa,s14_Iwim_CatR),
                 (s13_Ir_CaWp,reset,s16_Ir_CaWp), (s13_Ir_CaWp,C_gr,s13_Ir_CnrRaWp), (s13_Ir_CaWp,I_wpa,s14_Iwim_CaWp),
                 (s10_Ie_CatL,reset,s3_Ie_CatL), (s10_Ie_CatL,C_gr,s10_Ie_CnrR), (s10_Ie_CatL,C_grab,s10_Ie_CgWp),
                 (s15_Iwim_CnrR,C_eogr,s1_Iwim_CatR), (s15_Iwim_CnrR,I_inject,s15_Iie_CnrR), (s15_Iwim_CgWp,I_inject,s15_Iie_CgWp),
                 (s15_Iwim_CgWp,C_eog,s15_Iwim_CaWp), (s15_Iie_CatL,C_gr,s15_Iie_CnrR), (s15_Iie_CatL,C_grab,s15_Iie_CgWp),
                 (s15_Iie_CatL,I_eoi,s15_Ie_CatL), (s14_Iie_CnrR,reset,s15_Iie_CnrR), (s14_Iie_CnrR,I_eoi,s14_Ie_CnrR),
                 (s14_Iie_CnrR,C_eogr,s14_Iie_CatR), (s14_Iwim_CatR,reset,s15_Iwim_CatR), (s14_Iwim_CatR,I_inject,s14_Iie_CatR),
                 (s14_Iie_CgWp,reset,s15_Iie_CgWp), (s14_Iie_CgWp,I_eoi,s14_Ie_CgWp), (s14_Iie_CgWp,C_eog,s14_Iie_CaWp),
                 (s14_Iwim_CaWp,reset,s15_Iwim_CaWp), (s14_Iwim_CaWp,C_gr,s14_Iwim_CnrRaWp), (s14_Iwim_CaWp,I_inject,s14_Iie_CaWp),
                 (s14_Ie_CatL,reset,s15_Ie_CatL), (s14_Ie_CatL,C_gr,s14_Ie_CnrR), (s14_Ie_CatL,C_grab,s14_Ie_CgWp),
                 (s16_Ir_CaWp,I_wpa,s15_Iwim_CaWp), (s16_Ir_CaWp,C_gr,s16_Ir_CnrRaWp), (s16_Ir_CatR,I_wpa,s15_Iwim_CatR),
                 (s13_Ir_CnrRaWp,reset,s16_Ir_CnrRaWp), (s13_Ir_CnrRaWp,I_wpa,s14_Iwim_CnrRaWp), (s13_Ir_CnrRaWp,C_eogr,s13_Ir_CatRaWp),
                 (s10_Ie_CnrR,reset,s3_Ie_CnrR), (s10_Ie_CnrR,C_eogr,s10_Ie_CatR), (s10_Ie_CgWp,reset,s3_Ie_CgWp),
                 (s10_Ie_CgWp,C_eog,s10_Ie_CaWp), (s15_Iie_CnrR,C_eogr,s1_Iie_CatR), (s15_Iie_CnrR,I_eoi,s15_Ie_CnrR),
                 (s15_Iie_CgWp,I_eoi,s15_Ie_CgWp), (s15_Iie_CgWp,C_eog,s15_Iie_CaWp), (s15_Iwim_CaWp,C_gr,s15_Iwim_CnrRaWp),
                 (s15_Iwim_CaWp,I_inject,s15_Iie_CaWp), (s15_Ie_CatL,C_gr,s15_Ie_CnrR), (s15_Ie_CatL,C_grab,s15_Ie_CgWp),
                 (s14_Ie_CnrR,reset,s15_Ie_CnrR), (s14_Ie_CnrR,C_eogr,s14_Ie_CatR), (s14_Iie_CatR,reset,s15_Iie_CatR),
                 (s14_Iie_CatR,I_eoi,s14_Ie_CatR), (s15_Iwim_CatR,I_inject,s15_Iie_CatR), (s14_Ie_CgWp,reset,s15_Ie_CgWp),
                 (s14_Ie_CgWp,C_eog,s14_Ie_CaWp), (s14_Iie_CaWp,reset,s15_Iie_CaWp), (s14_Iie_CaWp,C_gr,s14_Iie_CnrRaWp),
                 (s14_Iie_CaWp,I_eoi,s14_Ie_CaWp), (s14_Iwim_CnrRaWp,reset,s15_Iwim_CnrRaWp), (s14_Iwim_CnrRaWp,I_inject,s14_Iie_CnrRaWp),
                 (s14_Iwim_CnrRaWp,C_eogr,s14_Iwim_CatRaWp), (s16_Ir_CnrRaWp,I_wpa,s15_Iwim_CnrRaWp), (s16_Ir_CnrRaWp,C_eogr,s0_Ir_CatRaWp),
                 (s13_Ir_CatRaWp,reset,s16_Ir_CatRaWp), (s13_Ir_CatRaWp,C_drop,s13_Ir_CdWp), (s13_Ir_CatRaWp,I_wpa,s14_Iwim_CatRaWp),
                 (s10_Ie_CatR,reset,s3_Ie_CatR), (s10_Ie_CaWp,reset,s3_Ie_CaWp), (s10_Ie_CaWp,C_gr,s10_Ie_CnrRaWp),
                 (s15_Ie_CnrR,C_eogr,s1_Ie_CatR), (s15_Ie_CgWp,C_eog,s15_Ie_CaWp), (s15_Iie_CaWp,C_gr,s15_Iie_CnrRaWp),
                 (s15_Iie_CaWp,I_eoi,s15_Ie_CaWp), (s15_Iwim_CnrRaWp,C_eogr,s1_Iwim_CatRaWp), (s15_Iwim_CnrRaWp,I_inject,s15_Iie_CnrRaWp),
                 (s14_Ie_CatR,reset,s15_Ie_CatR), (s15_Iie_CatR,I_eoi,s15_Ie_CatR), (s14_Ie_CaWp,reset,s15_Ie_CaWp),
                 (s14_Ie_CaWp,C_gr,s14_Ie_CnrRaWp), (s14_Iie_CnrRaWp,reset,s15_Iie_CnrRaWp), (s14_Iie_CnrRaWp,I_eoi,s14_Ie_CnrRaWp),
                 (s14_Iie_CnrRaWp,C_eogr,s14_Iie_CatRaWp), (s14_Iwim_CatRaWp,reset,s15_Iwim_CatRaWp), (s14_Iwim_CatRaWp,C_drop,s14_Iwim_CdWp),
                 (s14_Iwim_CatRaWp,I_inject,s14_Iie_CatRaWp), (s0_Ir_CatRaWp,I_wpa,s1_Iwim_CatRaWp), (s0_Ir_CatRaWp,C_drop,s0_Ir_CdWp),
                 (s16_Ir_CatRaWp,I_wpa,s15_Iwim_CatRaWp), (s16_Ir_CatRaWp,C_drop,s16_Ir_CdWp), (s13_Ir_CdWp,reset,s16_Ir_CdWp),
                 (s13_Ir_CdWp,C_eod,s13_Ir_CatR), (s13_Ir_CdWp,I_wpa,s14_Iwim_CdWp), (s3_Ie_CaWp,C_gr,s3_Ie_CnrRaWp),
                 (s10_Ie_CnrRaWp,reset,s3_Ie_CnrRaWp), (s10_Ie_CnrRaWp,C_eogr,s10_Ie_CatRaWp), (s15_Ie_CaWp,C_gr,s15_Ie_CnrRaWp),
                 (s15_Iie_CnrRaWp,C_eogr,s1_Iie_CatRaWp), (s15_Iie_CnrRaWp,I_eoi,s15_Ie_CnrRaWp), (s1_Iwim_CatRaWp,I_inject,s1_Iie_CatRaWp),
                 (s1_Iwim_CatRaWp,C_drop,s1_Iwim_CdWp), (s14_Ie_CnrRaWp,reset,s15_Ie_CnrRaWp), (s14_Ie_CnrRaWp,C_eogr,s14_Ie_CatRaWp),
                 (s14_Iie_CatRaWp,reset,s15_Iie_CatRaWp), (s14_Iie_CatRaWp,C_drop,s14_Iie_CdWp), (s14_Iie_CatRaWp,I_eoi,s14_Ie_CatRaWp),
                 (s15_Iwim_CatRaWp,C_drop,s15_Iwim_CdWp), (s15_Iwim_CatRaWp,I_inject,s15_Iie_CatRaWp), (s14_Iwim_CdWp,reset,s15_Iwim_CdWp),
                 (s14_Iwim_CdWp,C_eod,s14_Iwim_CatR), (s14_Iwim_CdWp,I_inject,s14_Iie_CdWp), (s0_Ir_CdWp,I_wpa,s1_Iwim_CdWp),
                 (s0_Ir_CdWp,C_eod,s0_Ir_CatR), (s16_Ir_CdWp,I_wpa,s15_Iwim_CdWp), (s16_Ir_CdWp,C_eod,s16_Ir_CatR),
                 (s3_Ie_CnrRaWp,C_eogr,s3_Ie_CatRaWp), (s10_Ie_CatRaWp,reset,s3_Ie_CatRaWp), (s10_Ie_CatRaWp,C_drop,s10_Ie_CdWp),
                 (s15_Ie_CnrRaWp,C_eogr,s1_Ie_CatRaWp), (s1_Iie_CatRaWp,I_eoi,s2_Ie_CatRaWp), (s1_Iie_CatRaWp,C_drop,s1_Iie_CdWp),
                 (s1_Iwim_CdWp,I_inject,s1_Iie_CdWp), (s1_Iwim_CdWp,C_eod,s1_Iwim_CatR), (s14_Ie_CatRaWp,reset,s15_Ie_CatRaWp),
                 (s14_Ie_CatRaWp,C_drop,s14_Ie_CdWp), (s15_Iie_CatRaWp,C_drop,s15_Iie_CdWp), (s15_Iie_CatRaWp,I_eoi,s15_Ie_CatRaWp),
                 (s14_Iie_CdWp,reset,s15_Iie_CdWp), (s14_Iie_CdWp,C_eod,s14_Iie_CatR), (s14_Iie_CdWp,I_eoi,s14_Ie_CdWp),
                 (s15_Iwim_CdWp,C_eod,s15_Iwim_CatR), (s15_Iwim_CdWp,I_inject,s15_Iie_CdWp), (s3_Ie_CatRaWp,C_drop,s3_Ie_CdWp),
                 (s10_Ie_CdWp,reset,s3_Ie_CdWp), (s10_Ie_CdWp,C_eod,s10_Ie_CatR), (s1_Ie_CatRaWp,C_drop,s1_Ie_CdWp),
                 (s2_Ie_CatRaWp,C_drop,s2_Ie_CdWp), (s1_Iie_CdWp,I_eoi,s2_Ie_CdWp), (s1_Iie_CdWp,C_eod,s1_Iie_CatR),
                 (s15_Ie_CatRaWp,C_drop,s15_Ie_CdWp), (s14_Ie_CdWp,reset,s15_Ie_CdWp), (s14_Ie_CdWp,C_eod,s14_Ie_CatR),
                 (s15_Iie_CdWp,C_eod,s15_Iie_CatR), (s15_Iie_CdWp,I_eoi,s15_Ie_CdWp), (s3_Ie_CdWp,C_eod,s3_Ie_CatR),
                 (s1_Ie_CdWp,C_eod,s1_Ie_CatR), (s2_Ie_CdWp,C_eod,s2_Ie_CatR), (s15_Ie_CdWp,C_eod,s15_Ie_CatR) },
            x_0: s0_Ir_CatR,
            Xm: {s0_Ir_CatR},
            ctrlEvents: {reset, I_rearm, C_gl, I_inject, C_drop, C_grab, C_gr} )
 &
 seqD = {
      (s0_Ir_CatR,s0),(s1_Iwim_CatR,s1),(s1_Iie_CatR,s1),
      (s2_Ie_CatR,s2),(s2_Ie_CnrL,s2),(s3_Ie_CatL,s3),
      (s3_Ie_CnrR,s3),(s3_Ie_CgWp,s3),(s3_Ie_CatR,s3),
      (s4_Ie_CaWp,s4),(s5_Ir_CaWp,s5),(s4_Ie_CnrRaWp,s4),
      (s6_Iwim_CaWp,s6),(s5_Ir_CnrRaWp,s5),(s4_Ie_CatRaWp,s4),
      (s6_Iwim_CnrRaWp,s6),(s6_Iie_CaWp,s6),(s7_Ir_CatRaWp,s7),
      (s5_Ir_CatRaWp,s5),(s4_Ie_CdWp,s4),(s8_Iwim_CatRaWp,s8),
      (s6_Iie_CnrRaWp,s6),(s6_Ie_CaWp,s6),(s7_Ir_CdWp,s7),
      (s6_Iwim_CatRaWp,s6),(s5_Ir_CdWp,s5),(s4_Ie_CatR,s4),
      (s8_Iie_CatRaWp,s8),(s8_Iwim_CdWp,s8),(s6_Ie_CnrRaWp,s6),
      (s7_Ir_CatR,s7),(s6_Iwim_CdWp,s6),(s6_Iie_CatRaWp,s6),
      (s5_Ir_CatR,s5),(s9_Ie_CatRaWp,s9),(s8_Iie_CdWp,s8),
      (s8_Iwim_CatR,s8),(s8_Ie_CatRaWp,s8),(s11_Ir_CnrL,s11),
      (s6_Iwim_CatR,s6),(s6_Iie_CdWp,s6),(s6_Ie_CatRaWp,s6),
      (s9_Ie_CdWp,s9),(s8_Iie_CatR,s8),(s8_Ie_CdWp,s8),
      (s12_Iwim_CnrL,s12),(s13_Ir_CatL,s13),(s6_Iie_CatR,s6),
      (s6_Ie_CdWp,s6),(s9_Ie_CatR,s9),(s8_Ie_CatR,s8),
      (s14_Iwim_CatL,s14),(s12_Iie_CnrL,s12),(s16_Ir_CatL,s16),
      (s13_Ir_CnrR,s13),(s13_Ir_CgWp,s13),(s6_Ie_CatR,s6),
      (s9_Ie_CnrL,s9),(s15_Iwim_CatL,s15),(s14_Iwim_CnrR,s14),
      (s14_Iwim_CgWp,s14),(s14_Iie_CatL,s14),(s12_Ie_CnrL,s12),
      (s16_Ir_CnrR,s16),(s16_Ir_CgWp,s16),(s13_Ir_CatR,s13),
      (s13_Ir_CaWp,s13),(s10_Ie_CatL,s10),(s15_Iwim_CnrR,s15),
      (s15_Iwim_CgWp,s15),(s15_Iie_CatL,s15),(s14_Iie_CnrR,s14),
      (s14_Iwim_CatR,s14),(s14_Iie_CgWp,s14),(s14_Iwim_CaWp,s14),
      (s14_Ie_CatL,s14),(s16_Ir_CaWp,s16),(s16_Ir_CatR,s16),
      (s13_Ir_CnrRaWp,s13),(s10_Ie_CnrR,s10),(s10_Ie_CgWp,s10),
      (s15_Iie_CnrR,s15),(s15_Iie_CgWp,s15),(s15_Iwim_CaWp,s15),
      (s15_Ie_CatL,s15),(s14_Ie_CnrR,s14),(s14_Iie_CatR,s14),
      (s15_Iwim_CatR,s15),(s14_Ie_CgWp,s14),(s14_Iie_CaWp,s14),
      (s14_Iwim_CnrRaWp,s14),(s16_Ir_CnrRaWp,s16),(s13_Ir_CatRaWp,s13),
      (s10_Ie_CatR,s10),(s10_Ie_CaWp,s10),(s15_Ie_CnrR,s15),
      (s15_Ie_CgWp,s15),(s15_Iie_CaWp,s15),(s15_Iwim_CnrRaWp,s15),
      (s14_Ie_CatR,s14),(s15_Iie_CatR,s15),(s14_Ie_CaWp,s14),
      (s14_Iie_CnrRaWp,s14),(s14_Iwim_CatRaWp,s14),(s0_Ir_CatRaWp,s0),
      (s16_Ir_CatRaWp,s16),(s13_Ir_CdWp,s13),(s3_Ie_CaWp,s3),
      (s10_Ie_CnrRaWp,s10),(s1_Ie_CatR,s1),(s15_Ie_CaWp,s15),
      (s15_Iie_CnrRaWp,s15),(s1_Iwim_CatRaWp,s1),(s15_Ie_CatR,s15),
      (s14_Ie_CnrRaWp,s14),(s14_Iie_CatRaWp,s14),(s15_Iwim_CatRaWp,s15),
      (s14_Iwim_CdWp,s14),(s0_Ir_CdWp,s0),(s16_Ir_CdWp,s16),
      (s3_Ie_CnrRaWp,s3),(s10_Ie_CatRaWp,s10),(s15_Ie_CnrRaWp,s15),
      (s1_Iie_CatRaWp,s1),(s1_Iwim_CdWp,s1),(s14_Ie_CatRaWp,s14),
      (s15_Iie_CatRaWp,s15),(s14_Iie_CdWp,s14),(s15_Iwim_CdWp,s15),
      (s3_Ie_CatRaWp,s3),(s10_Ie_CdWp,s10),(s1_Ie_CatRaWp,s1),
      (s2_Ie_CatRaWp,s2),(s1_Iie_CdWp,s1),(s15_Ie_CatRaWp,s15),
      (s14_Ie_CdWp,s14),(s15_Iie_CdWp,s15),(s3_Ie_CdWp,s3),
      (s1_Ie_CdWp,s1),(s2_Ie_CdWp,s2),(s15_Ie_CdWp,s15) }
 &
 injector = {
      (s0_Ir_CatR,Ir),(s1_Iwim_CatR,Iwim),(s1_Iie_CatR,Iie),
      (s2_Ie_CatR,Ie),(s2_Ie_CnrL,Ie),(s3_Ie_CatL,Ie),
      (s3_Ie_CnrR,Ie),(s3_Ie_CgWp,Ie),(s3_Ie_CatR,Ie),
      (s4_Ie_CaWp,Ie),(s5_Ir_CaWp,Ir),(s4_Ie_CnrRaWp,Ie),
      (s6_Iwim_CaWp,Iwim),(s5_Ir_CnrRaWp,Ir),(s4_Ie_CatRaWp,Ie),
      (s6_Iwim_CnrRaWp,Iwim),(s6_Iie_CaWp,Iie),(s7_Ir_CatRaWp,Ir),
      (s5_Ir_CatRaWp,Ir),(s4_Ie_CdWp,Ie),(s8_Iwim_CatRaWp,Iwim),
      (s6_Iie_CnrRaWp,Iie),(s6_Ie_CaWp,Ie),(s7_Ir_CdWp,Ir),
      (s6_Iwim_CatRaWp,Iwim),(s5_Ir_CdWp,Ir),(s4_Ie_CatR,Ie),
      (s8_Iie_CatRaWp,Iie),(s8_Iwim_CdWp,Iwim),(s6_Ie_CnrRaWp,Ie),
      (s7_Ir_CatR,Ir),(s6_Iwim_CdWp,Iwim),(s6_Iie_CatRaWp,Iie),
      (s5_Ir_CatR,Ir),(s9_Ie_CatRaWp,Ie),(s8_Iie_CdWp,Iie),
      (s8_Iwim_CatR,Iwim),(s8_Ie_CatRaWp,Ie),(s11_Ir_CnrL,Ir),
      (s6_Iwim_CatR,Iwim),(s6_Iie_CdWp,Iie),(s6_Ie_CatRaWp,Ie),
      (s9_Ie_CdWp,Ie),(s8_Iie_CatR,Iie),(s8_Ie_CdWp,Ie),
      (s12_Iwim_CnrL,Iwim),(s13_Ir_CatL,Ir),(s6_Iie_CatR,Iie),
      (s6_Ie_CdWp,Ie),(s9_Ie_CatR,Ie),(s8_Ie_CatR,Ie),
      (s14_Iwim_CatL,Iwim),(s12_Iie_CnrL,Iie),(s16_Ir_CatL,Ir),
      (s13_Ir_CnrR,Ir),(s13_Ir_CgWp,Ir),(s6_Ie_CatR,Ie),
      (s9_Ie_CnrL,Ie),(s15_Iwim_CatL,Iwim),(s14_Iwim_CnrR,Iwim),
      (s14_Iwim_CgWp,Iwim),(s14_Iie_CatL,Iie),(s12_Ie_CnrL,Ie),
      (s16_Ir_CnrR,Ir),(s16_Ir_CgWp,Ir),(s13_Ir_CatR,Ir),
      (s13_Ir_CaWp,Ir),(s10_Ie_CatL,Ie),(s15_Iwim_CnrR,Iwim),
      (s15_Iwim_CgWp,Iwim),(s15_Iie_CatL,Iie),(s14_Iie_CnrR,Iie),
      (s14_Iwim_CatR,Iwim),(s14_Iie_CgWp,Iie),(s14_Iwim_CaWp,Iwim),
      (s14_Ie_CatL,Ie),(s16_Ir_CaWp,Ir),(s16_Ir_CatR,Ir),
      (s13_Ir_CnrRaWp,Ir),(s10_Ie_CnrR,Ie),(s10_Ie_CgWp,Ie),
      (s15_Iie_CnrR,Iie),(s15_Iie_CgWp,Iie),(s15_Iwim_CaWp,Iwim),
      (s15_Ie_CatL,Ie),(s14_Ie_CnrR,Ie),(s14_Iie_CatR,Iie),
      (s15_Iwim_CatR,Iwim),(s14_Ie_CgWp,Ie),(s14_Iie_CaWp,Iie),
      (s14_Iwim_CnrRaWp,Iwim),(s16_Ir_CnrRaWp,Ir),(s13_Ir_CatRaWp,Ir),
      (s10_Ie_CatR,Ie),(s10_Ie_CaWp,Ie),(s15_Ie_CnrR,Ie),
      (s15_Ie_CgWp,Ie),(s15_Iie_CaWp,Iie),(s15_Iwim_CnrRaWp,Iwim),
      (s14_Ie_CatR,Ie),(s15_Iie_CatR,Iie),(s14_Ie_CaWp,Ie),
      (s14_Iie_CnrRaWp,Iie),(s14_Iwim_CatRaWp,Iwim),(s0_Ir_CatRaWp,Ir),
      (s16_Ir_CatRaWp,Ir),(s13_Ir_CdWp,Ir),(s3_Ie_CaWp,Ie),
      (s10_Ie_CnrRaWp,Ie),(s1_Ie_CatR,Ie),(s15_Ie_CaWp,Ie),
      (s15_Iie_CnrRaWp,Iie),(s1_Iwim_CatRaWp,Iwim),(s15_Ie_CatR,Ie),
      (s14_Ie_CnrRaWp,Ie),(s14_Iie_CatRaWp,Iie),(s15_Iwim_CatRaWp,Iwim),
      (s14_Iwim_CdWp,Iwim),(s0_Ir_CdWp,Ir),(s16_Ir_CdWp,Ir),
      (s3_Ie_CnrRaWp,Ie),(s10_Ie_CatRaWp,Ie),(s15_Ie_CnrRaWp,Ie),
      (s1_Iie_CatRaWp,Iie),(s1_Iwim_CdWp,Iwim),(s14_Ie_CatRaWp,Ie),
      (s15_Iie_CatRaWp,Iie),(s14_Iie_CdWp,Iie),(s15_Iwim_CdWp,Iwim),
      (s3_Ie_CatRaWp,Ie),(s10_Ie_CdWp,Ie),(s1_Ie_CatRaWp,Ie),
      (s2_Ie_CatRaWp,Ie),(s1_Iie_CdWp,Iie),(s15_Ie_CatRaWp,Ie),
      (s14_Ie_CdWp,Ie),(s15_Iie_CdWp,Iie),(s3_Ie_CdWp,Ie),
      (s1_Ie_CdWp,Ie),(s2_Ie_CdWp,Ie),(s15_Ie_CdWp,Ie) }
 &
 crane = {
      (s0_Ir_CatR,CatR),(s1_Iwim_CatR,CatR),(s1_Iie_CatR,CatR),
      (s2_Ie_CatR,CatR),(s2_Ie_CnrL,CnrL),(s3_Ie_CatL,CatL),
      (s3_Ie_CnrR,CnrR),(s3_Ie_CgWp,CgWp),(s3_Ie_CatR,CatR),
      (s4_Ie_CaWp,CaWp),(s5_Ir_CaWp,CaWp),(s4_Ie_CnrRaWp,CnrRaWp),
      (s6_Iwim_CaWp,CaWp),(s5_Ir_CnrRaWp,CnrRaWp),(s4_Ie_CatRaWp,CatRaWp),
      (s6_Iwim_CnrRaWp,CnrRaWp),(s6_Iie_CaWp,CaWp),(s7_Ir_CatRaWp,CatRaWp),
      (s5_Ir_CatRaWp,CatRaWp),(s4_Ie_CdWp,CdWp),(s8_Iwim_CatRaWp,CatRaWp),
      (s6_Iie_CnrRaWp,CnrRaWp),(s6_Ie_CaWp,CaWp),(s7_Ir_CdWp,CdWp),
      (s6_Iwim_CatRaWp,CatRaWp),(s5_Ir_CdWp,CdWp),(s4_Ie_CatR,CatR),
      (s8_Iie_CatRaWp,CatRaWp),(s8_Iwim_CdWp,CdWp),(s6_Ie_CnrRaWp,CnrRaWp),
      (s7_Ir_CatR,CatR),(s6_Iwim_CdWp,CdWp),(s6_Iie_CatRaWp,CatRaWp),
      (s5_Ir_CatR,CatR),(s9_Ie_CatRaWp,CatRaWp),(s8_Iie_CdWp,CdWp),
      (s8_Iwim_CatR,CatR),(s8_Ie_CatRaWp,CatRaWp),(s11_Ir_CnrL,CnrL),
      (s6_Iwim_CatR,CatR),(s6_Iie_CdWp,CdWp),(s6_Ie_CatRaWp,CatRaWp),
      (s9_Ie_CdWp,CdWp),(s8_Iie_CatR,CatR),(s8_Ie_CdWp,CdWp),
      (s12_Iwim_CnrL,CnrL),(s13_Ir_CatL,CatL),(s6_Iie_CatR,CatR),
      (s6_Ie_CdWp,CdWp),(s9_Ie_CatR,CatR),(s8_Ie_CatR,CatR),
      (s14_Iwim_CatL,CatL),(s12_Iie_CnrL,CnrL),(s16_Ir_CatL,CatL),
      (s13_Ir_CnrR,CnrR),(s13_Ir_CgWp,CgWp),(s6_Ie_CatR,CatR),
      (s9_Ie_CnrL,CnrL),(s15_Iwim_CatL,CatL),(s14_Iwim_CnrR,CnrR),
      (s14_Iwim_CgWp,CgWp),(s14_Iie_CatL,CatL),(s12_Ie_CnrL,CnrL),
      (s16_Ir_CnrR,CnrR),(s16_Ir_CgWp,CgWp),(s13_Ir_CatR,CatR),
      (s13_Ir_CaWp,CaWp),(s10_Ie_CatL,CatL),(s15_Iwim_CnrR,CnrR),
      (s15_Iwim_CgWp,CgWp),(s15_Iie_CatL,CatL),(s14_Iie_CnrR,CnrR),
      (s14_Iwim_CatR,CatR),(s14_Iie_CgWp,CgWp),(s14_Iwim_CaWp,CaWp),
      (s14_Ie_CatL,CatL),(s16_Ir_CaWp,CaWp),(s16_Ir_CatR,CatR),
      (s13_Ir_CnrRaWp,CnrRaWp),(s10_Ie_CnrR,CnrR),(s10_Ie_CgWp,CgWp),
      (s15_Iie_CnrR,CnrR),(s15_Iie_CgWp,CgWp),(s15_Iwim_CaWp,CaWp),
      (s15_Ie_CatL,CatL),(s14_Ie_CnrR,CnrR),(s14_Iie_CatR,CatR),
      (s15_Iwim_CatR,CatR),(s14_Ie_CgWp,CgWp),(s14_Iie_CaWp,CaWp),
      (s14_Iwim_CnrRaWp,CnrRaWp),(s16_Ir_CnrRaWp,CnrRaWp),(s13_Ir_CatRaWp,CatRaWp),
      (s10_Ie_CatR,CatR),(s10_Ie_CaWp,CaWp),(s15_Ie_CnrR,CnrR),
      (s15_Ie_CgWp,CgWp),(s15_Iie_CaWp,CaWp),(s15_Iwim_CnrRaWp,CnrRaWp),
      (s14_Ie_CatR,CatR),(s15_Iie_CatR,CatR),(s14_Ie_CaWp,CaWp),
      (s14_Iie_CnrRaWp,CnrRaWp),(s14_Iwim_CatRaWp,CatRaWp),(s0_Ir_CatRaWp,CatRaWp),
      (s16_Ir_CatRaWp,CatRaWp),(s13_Ir_CdWp,CdWp),(s3_Ie_CaWp,CaWp),
      (s10_Ie_CnrRaWp,CnrRaWp),(s1_Ie_CatR,CatR),(s15_Ie_CaWp,CaWp),
      (s15_Iie_CnrRaWp,CnrRaWp),(s1_Iwim_CatRaWp,CatRaWp),(s15_Ie_CatR,CatR),
      (s14_Ie_CnrRaWp,CnrRaWp),(s14_Iie_CatRaWp,CatRaWp),(s15_Iwim_CatRaWp,CatRaWp),
      (s14_Iwim_CdWp,CdWp),(s0_Ir_CdWp,CdWp),(s16_Ir_CdWp,CdWp),
      (s3_Ie_CnrRaWp,CnrRaWp),(s10_Ie_CatRaWp,CatRaWp),(s15_Ie_CnrRaWp,CnrRaWp),
      (s1_Iie_CatRaWp,CatRaWp),(s1_Iwim_CdWp,CdWp),(s14_Ie_CatRaWp,CatRaWp),
      (s15_Iie_CatRaWp,CatRaWp),(s14_Iie_CdWp,CdWp),(s15_Iwim_CdWp,CdWp),
      (s3_Ie_CatRaWp,CatRaWp),(s10_Ie_CdWp,CdWp),(s1_Ie_CatRaWp,CatRaWp),
      (s2_Ie_CatRaWp,CatRaWp),(s1_Iie_CdWp,CdWp),(s15_Ie_CatRaWp,CatRaWp),
      (s14_Ie_CdWp,CdWp),(s15_Iie_CdWp,CdWp),(s3_Ie_CdWp,CdWp),
      (s1_Ie_CdWp,CdWp),(s2_Ie_CdWp,CdWp),(s15_Ie_CdWp,CdWp) }
 &
 BadStates =  States    // to be completed
 &
 Qpred = States - BadStates
END