// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL test_SBdtphLocallyModular

SETS
 States = {DcrR_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle, Dm1stwp_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle, D2ndwp_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle,
           DcrL_b11_Tidle_Tidle_b20_Pready_Pready_b30_Hidle, DcrL1_b11_Tidle_Tidle_b20_Pready_Pready_b30_Hidle, DcrL_b11_Tpause_Tpause_b20_Pready_Pready_b30_Hidle,
           DcrL1_b11_Tpause_Tpause_b20_Pready_Pready_b30_Hidle, DcrL_b11_Trt_Trt_b20_Pready_Pready_b30_Hidle, DcrL_b11_Twt_Twt_b20_Pready_Pready_b30_Hidle,
           DcrL1_b11_Trt_Trt_b20_Pready_Pready_b30_Hidle, DcrL1_b11_Twt_Twt_b20_Pready_Pready_b30_Hidle, DcrL_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle,
           DcrL_b11_Tit_Tit_b20_Pready_Pready_b30_Hidle, DcrL1_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle, DcrL1_b11_Tit_Tit_b20_Pready_Pready_b30_Hidle,
           DcrL_b11_Trt_Trt_b21_Pready_Pready_b30_Hidle, DcrL1_b11_Trt_Trt_b21_Pready_Pready_b30_Hidle, DcrL_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle,
           DcrL_b11_Trt_Trt_b21_Punready_Punready_b30_Hidle, DcrL1_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle, DcrL1_b11_Trt_Trt_b21_Punready_Punready_b30_Hidle,
           DcrR_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle, DcrL_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle, DcrL_b11_Trt_Trt_b20_Pready_Pready_b31_Hidle,
           Dm1stwp_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle, DcrL1_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle, DcrL1_b11_Trt_Trt_b20_Pready_Pready_b31_Hidle,
           DcrR_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle, DcrL_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle, DcrL_b11_Trt_Trt_b20_Pready_Pready_b31_Hbusy,
           D2ndwp_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle, DcrL_b11_Tidle_Tidle_b21_Pready_Pready_b30_Hidle, Dm1stwp_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle,
           DcrL1_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle, DcrL1_b11_Trt_Trt_b20_Pready_Pready_b31_Hbusy, DcrR_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle,
           DcrL_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy, DcrL1_b11_Tidle_Tidle_b21_Pready_Pready_b30_Hidle, D2ndwp_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle,
           DcrL_b11_Tpause_Tpause_b21_Pready_Pready_b30_Hidle, DcrL_b11_Tidle_Tidle_b21_Punready_Punready_b30_Hidle, Dm1stwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle,
           DcrL1_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy, DcrR_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy, DcrL1_b11_Tpause_Tpause_b21_Pready_Pready_b30_Hidle,
           DcrL1_b11_Tidle_Tidle_b21_Punready_Punready_b30_Hidle, D2ndwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle, DcrL_b11_Twt_Twt_b21_Pready_Pready_b30_Hidle,
           DcrL_b11_Tpause_Tpause_b21_Punready_Punready_b30_Hidle, DcrL_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hidle, Dm1stwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy,
           DcrL1_b11_Twt_Twt_b21_Pready_Pready_b30_Hidle, DcrL1_b11_Tpause_Tpause_b21_Punready_Punready_b30_Hidle, DcrL1_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hidle,
           D2ndwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy, DcrL_b11_Twt_Twt_b21_Punready_Punready_b30_Hidle, DcrL_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hidle,
           DcrL_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy, DcrL1_b11_Twt_Twt_b21_Punready_Punready_b30_Hidle, DcrL1_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hidle,
           DcrL1_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy, DcrL_b11_Twt_Twt_b20_Pready_Pready_b31_Hidle, DcrL_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hbusy,
           DcrL1_b11_Twt_Twt_b20_Pready_Pready_b31_Hidle, DcrL1_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hbusy, DcrL_b11_Tit_Tit_b20_Pready_Pready_b31_Hidle,
           DcrL_b11_Twt_Twt_b20_Pready_Pready_b31_Hbusy, DcrL1_b11_Tit_Tit_b20_Pready_Pready_b31_Hidle, DcrL1_b11_Twt_Twt_b20_Pready_Pready_b31_Hbusy,
           DcrL_b11_Trt_Trt_b21_Pready_Pready_b31_Hidle, DcrL_b11_Tit_Tit_b20_Pready_Pready_b31_Hbusy, DcrL1_b11_Trt_Trt_b21_Pready_Pready_b31_Hidle,
           DcrL1_b11_Tit_Tit_b20_Pready_Pready_b31_Hbusy, DcrL_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle, DcrL_b11_Trt_Trt_b21_Pready_Pready_b31_Hbusy,
           DcrL1_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle, DcrL1_b11_Trt_Trt_b21_Pready_Pready_b31_Hbusy, DcrR_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle,
           DcrL_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy, Dm1stwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle, DcrL1_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy,
           DcrR_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy, D2ndwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle, DcrL_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hidle,
           Dm1stwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy, DcrL1_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hidle, D2ndwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy,
           DcrL_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hidle, DcrL_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy, DcrL1_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hidle,
           DcrL1_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy, DcrL_b11_Twt_Twt_b21_Pready_Pready_b31_Hidle, DcrL_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hbusy,
           DcrL1_b11_Twt_Twt_b21_Pready_Pready_b31_Hidle, DcrL1_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hbusy, DcrL_b11_Twt_Twt_b21_Pready_Pready_b31_Hbusy,
           DcrL1_b11_Twt_Twt_b21_Pready_Pready_b31_Hbusy };
 Events = {D_wpa, D_release, D_creogl, T_eot, T_eotwp, T_fail, T_pass, T_test, T_twp, P_eoc, P_open, H_start, H_stop}

CONSTANTS
 G_FB

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
 "../../Theories/SBCDES.def";
 "LibraryIO.def"; 

PROPERTIES
 G_FB = rec(       // Automaton modeling the free behavior
            states: States,
            events: Events,
            tf: {(DcrR_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle,D_wpa,Dm1stwp_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle), (Dm1stwp_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle,D_wpa,D2ndwp_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle), (Dm1stwp_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle,D_creogl,DcrL_b11_Tidle_Tidle_b20_Pready_Pready_b30_Hidle),
                 (D2ndwp_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle,D_creogl,DcrL1_b11_Tidle_Tidle_b20_Pready_Pready_b30_Hidle), (DcrL_b11_Tidle_Tidle_b20_Pready_Pready_b30_Hidle,D_wpa,DcrL1_b11_Tidle_Tidle_b20_Pready_Pready_b30_Hidle), (DcrL_b11_Tidle_Tidle_b20_Pready_Pready_b30_Hidle,T_test,DcrL_b11_Tpause_Tpause_b20_Pready_Pready_b30_Hidle),
                 (DcrL1_b11_Tidle_Tidle_b20_Pready_Pready_b30_Hidle,T_test,DcrL1_b11_Tpause_Tpause_b20_Pready_Pready_b30_Hidle), (DcrL_b11_Tpause_Tpause_b20_Pready_Pready_b30_Hidle,D_wpa,DcrL1_b11_Tpause_Tpause_b20_Pready_Pready_b30_Hidle), (DcrL_b11_Tpause_Tpause_b20_Pready_Pready_b30_Hidle,T_fail,DcrL_b11_Trt_Trt_b20_Pready_Pready_b30_Hidle),
                 (DcrL_b11_Tpause_Tpause_b20_Pready_Pready_b30_Hidle,T_pass,DcrL_b11_Twt_Twt_b20_Pready_Pready_b30_Hidle), (DcrL1_b11_Tpause_Tpause_b20_Pready_Pready_b30_Hidle,T_fail,DcrL1_b11_Trt_Trt_b20_Pready_Pready_b30_Hidle), (DcrL1_b11_Tpause_Tpause_b20_Pready_Pready_b30_Hidle,T_pass,DcrL1_b11_Twt_Twt_b20_Pready_Pready_b30_Hidle),
                 (DcrL_b11_Trt_Trt_b20_Pready_Pready_b30_Hidle,D_wpa,DcrL1_b11_Trt_Trt_b20_Pready_Pready_b30_Hidle), (DcrL_b11_Trt_Trt_b20_Pready_Pready_b30_Hidle,T_eot,DcrL_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle), (DcrL_b11_Twt_Twt_b20_Pready_Pready_b30_Hidle,D_wpa,DcrL1_b11_Twt_Twt_b20_Pready_Pready_b30_Hidle),
                 (DcrL_b11_Twt_Twt_b20_Pready_Pready_b30_Hidle,T_twp,DcrL_b11_Tit_Tit_b20_Pready_Pready_b30_Hidle), (DcrL1_b11_Trt_Trt_b20_Pready_Pready_b30_Hidle,T_eot,DcrL1_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle), (DcrL1_b11_Twt_Twt_b20_Pready_Pready_b30_Hidle,T_twp,DcrL1_b11_Tit_Tit_b20_Pready_Pready_b30_Hidle),
                 (DcrL_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle,D_wpa,DcrL1_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle), (DcrL_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle,D_release,DcrR_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle), (DcrL_b11_Tit_Tit_b20_Pready_Pready_b30_Hidle,D_wpa,DcrL1_b11_Tit_Tit_b20_Pready_Pready_b30_Hidle),
                 (DcrL_b11_Tit_Tit_b20_Pready_Pready_b30_Hidle,T_eotwp,DcrL_b11_Trt_Trt_b21_Pready_Pready_b30_Hidle), (DcrL1_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle,D_release,Dm1stwp_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle), (DcrL1_b11_Tit_Tit_b20_Pready_Pready_b30_Hidle,T_eotwp,DcrL1_b11_Trt_Trt_b21_Pready_Pready_b30_Hidle),
                 (DcrL_b11_Trt_Trt_b21_Pready_Pready_b30_Hidle,D_wpa,DcrL1_b11_Trt_Trt_b21_Pready_Pready_b30_Hidle), (DcrL_b11_Trt_Trt_b21_Pready_Pready_b30_Hidle,T_eot,DcrL_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle), (DcrL_b11_Trt_Trt_b21_Pready_Pready_b30_Hidle,P_open,DcrL_b11_Trt_Trt_b21_Punready_Punready_b30_Hidle),
                 (DcrL1_b11_Trt_Trt_b21_Pready_Pready_b30_Hidle,T_eot,DcrL1_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle), (DcrL1_b11_Trt_Trt_b21_Pready_Pready_b30_Hidle,P_open,DcrL1_b11_Trt_Trt_b21_Punready_Punready_b30_Hidle), (DcrL_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle,D_wpa,DcrL1_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle),
                 (DcrL_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle,D_release,DcrR_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle), (DcrL_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle,P_open,DcrL_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle), (DcrL_b11_Trt_Trt_b21_Punready_Punready_b30_Hidle,D_wpa,DcrL1_b11_Trt_Trt_b21_Punready_Punready_b30_Hidle),
                 (DcrL_b11_Trt_Trt_b21_Punready_Punready_b30_Hidle,T_eot,DcrL_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle), (DcrL_b11_Trt_Trt_b21_Punready_Punready_b30_Hidle,P_eoc,DcrL_b11_Trt_Trt_b20_Pready_Pready_b31_Hidle), (DcrL1_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle,D_release,Dm1stwp_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle),
                 (DcrL1_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle,P_open,DcrL1_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle), (DcrL1_b11_Trt_Trt_b21_Punready_Punready_b30_Hidle,T_eot,DcrL1_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle), (DcrL1_b11_Trt_Trt_b21_Punready_Punready_b30_Hidle,P_eoc,DcrL1_b11_Trt_Trt_b20_Pready_Pready_b31_Hidle),
                 (DcrR_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle,D_wpa,Dm1stwp_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle), (DcrR_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle,P_open,DcrR_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle), (DcrL_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle,D_wpa,DcrL1_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle),
                 (DcrL_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle,D_release,DcrR_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle), (DcrL_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle,P_eoc,DcrL_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle), (DcrL_b11_Trt_Trt_b20_Pready_Pready_b31_Hidle,D_wpa,DcrL1_b11_Trt_Trt_b20_Pready_Pready_b31_Hidle),
                 (DcrL_b11_Trt_Trt_b20_Pready_Pready_b31_Hidle,T_eot,DcrL_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle), (DcrL_b11_Trt_Trt_b20_Pready_Pready_b31_Hidle,H_start,DcrL_b11_Trt_Trt_b20_Pready_Pready_b31_Hbusy), (Dm1stwp_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle,D_wpa,D2ndwp_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle),
                 (Dm1stwp_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle,D_creogl,DcrL_b11_Tidle_Tidle_b21_Pready_Pready_b30_Hidle), (Dm1stwp_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle,P_open,Dm1stwp_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle), (DcrL1_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle,D_release,Dm1stwp_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle),
                 (DcrL1_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle,P_eoc,DcrL1_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle), (DcrL1_b11_Trt_Trt_b20_Pready_Pready_b31_Hidle,T_eot,DcrL1_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle), (DcrL1_b11_Trt_Trt_b20_Pready_Pready_b31_Hidle,H_start,DcrL1_b11_Trt_Trt_b20_Pready_Pready_b31_Hbusy),
                 (DcrR_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle,D_wpa,Dm1stwp_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle), (DcrR_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle,P_eoc,DcrR_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle), (DcrL_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle,D_wpa,DcrL1_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle),
                 (DcrL_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle,D_release,DcrR_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle), (DcrL_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle,H_start,DcrL_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy), (DcrL_b11_Trt_Trt_b20_Pready_Pready_b31_Hbusy,D_wpa,DcrL1_b11_Trt_Trt_b20_Pready_Pready_b31_Hbusy),
                 (DcrL_b11_Trt_Trt_b20_Pready_Pready_b31_Hbusy,T_eot,DcrL_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy), (DcrL_b11_Trt_Trt_b20_Pready_Pready_b31_Hbusy,H_stop,DcrL_b11_Trt_Trt_b20_Pready_Pready_b30_Hidle), (D2ndwp_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle,D_creogl,DcrL1_b11_Tidle_Tidle_b21_Pready_Pready_b30_Hidle),
                 (D2ndwp_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle,P_open,D2ndwp_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle), (DcrL_b11_Tidle_Tidle_b21_Pready_Pready_b30_Hidle,D_wpa,DcrL1_b11_Tidle_Tidle_b21_Pready_Pready_b30_Hidle), (DcrL_b11_Tidle_Tidle_b21_Pready_Pready_b30_Hidle,T_test,DcrL_b11_Tpause_Tpause_b21_Pready_Pready_b30_Hidle),
                 (DcrL_b11_Tidle_Tidle_b21_Pready_Pready_b30_Hidle,P_open,DcrL_b11_Tidle_Tidle_b21_Punready_Punready_b30_Hidle), (Dm1stwp_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle,D_wpa,D2ndwp_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle), (Dm1stwp_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle,D_creogl,DcrL_b11_Tidle_Tidle_b21_Punready_Punready_b30_Hidle),
                 (Dm1stwp_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle,P_eoc,Dm1stwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle), (DcrL1_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle,D_release,Dm1stwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle), (DcrL1_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle,H_start,DcrL1_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy),
                 (DcrL1_b11_Trt_Trt_b20_Pready_Pready_b31_Hbusy,T_eot,DcrL1_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy), (DcrL1_b11_Trt_Trt_b20_Pready_Pready_b31_Hbusy,H_stop,DcrL1_b11_Trt_Trt_b20_Pready_Pready_b30_Hidle), (DcrR_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle,D_wpa,Dm1stwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle),
                 (DcrR_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle,H_start,DcrR_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy), (DcrL_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy,D_wpa,DcrL1_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy), (DcrL_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy,D_release,DcrR_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy),
                 (DcrL_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy,H_stop,DcrL_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle), (DcrL1_b11_Tidle_Tidle_b21_Pready_Pready_b30_Hidle,T_test,DcrL1_b11_Tpause_Tpause_b21_Pready_Pready_b30_Hidle), (DcrL1_b11_Tidle_Tidle_b21_Pready_Pready_b30_Hidle,P_open,DcrL1_b11_Tidle_Tidle_b21_Punready_Punready_b30_Hidle),
                 (D2ndwp_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle,D_creogl,DcrL1_b11_Tidle_Tidle_b21_Punready_Punready_b30_Hidle), (D2ndwp_b10_Tidle_Tidle_b21_Punready_Punready_b30_Hidle,P_eoc,D2ndwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle), (DcrL_b11_Tpause_Tpause_b21_Pready_Pready_b30_Hidle,D_wpa,DcrL1_b11_Tpause_Tpause_b21_Pready_Pready_b30_Hidle),
                 (DcrL_b11_Tpause_Tpause_b21_Pready_Pready_b30_Hidle,T_fail,DcrL_b11_Trt_Trt_b21_Pready_Pready_b30_Hidle), (DcrL_b11_Tpause_Tpause_b21_Pready_Pready_b30_Hidle,T_pass,DcrL_b11_Twt_Twt_b21_Pready_Pready_b30_Hidle), (DcrL_b11_Tpause_Tpause_b21_Pready_Pready_b30_Hidle,P_open,DcrL_b11_Tpause_Tpause_b21_Punready_Punready_b30_Hidle),
                 (DcrL_b11_Tidle_Tidle_b21_Punready_Punready_b30_Hidle,D_wpa,DcrL1_b11_Tidle_Tidle_b21_Punready_Punready_b30_Hidle), (DcrL_b11_Tidle_Tidle_b21_Punready_Punready_b30_Hidle,T_test,DcrL_b11_Tpause_Tpause_b21_Punready_Punready_b30_Hidle), (DcrL_b11_Tidle_Tidle_b21_Punready_Punready_b30_Hidle,P_eoc,DcrL_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hidle),
                 (Dm1stwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle,D_wpa,D2ndwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle), (Dm1stwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle,D_creogl,DcrL_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hidle), (Dm1stwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle,H_start,Dm1stwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy),
                 (DcrL1_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy,D_release,Dm1stwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy), (DcrL1_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy,H_stop,DcrL1_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle), (DcrR_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy,D_wpa,Dm1stwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy),
                 (DcrR_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy,H_stop,DcrR_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle), (DcrL1_b11_Tpause_Tpause_b21_Pready_Pready_b30_Hidle,T_fail,DcrL1_b11_Trt_Trt_b21_Pready_Pready_b30_Hidle), (DcrL1_b11_Tpause_Tpause_b21_Pready_Pready_b30_Hidle,T_pass,DcrL1_b11_Twt_Twt_b21_Pready_Pready_b30_Hidle),
                 (DcrL1_b11_Tpause_Tpause_b21_Pready_Pready_b30_Hidle,P_open,DcrL1_b11_Tpause_Tpause_b21_Punready_Punready_b30_Hidle), (DcrL1_b11_Tidle_Tidle_b21_Punready_Punready_b30_Hidle,T_test,DcrL1_b11_Tpause_Tpause_b21_Punready_Punready_b30_Hidle), (DcrL1_b11_Tidle_Tidle_b21_Punready_Punready_b30_Hidle,P_eoc,DcrL1_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hidle),
                 (D2ndwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle,D_creogl,DcrL1_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hidle), (D2ndwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hidle,H_start,D2ndwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy), (DcrL_b11_Twt_Twt_b21_Pready_Pready_b30_Hidle,D_wpa,DcrL1_b11_Twt_Twt_b21_Pready_Pready_b30_Hidle),
                 (DcrL_b11_Twt_Twt_b21_Pready_Pready_b30_Hidle,P_open,DcrL_b11_Twt_Twt_b21_Punready_Punready_b30_Hidle), (DcrL_b11_Tpause_Tpause_b21_Punready_Punready_b30_Hidle,D_wpa,DcrL1_b11_Tpause_Tpause_b21_Punready_Punready_b30_Hidle), (DcrL_b11_Tpause_Tpause_b21_Punready_Punready_b30_Hidle,T_fail,DcrL_b11_Trt_Trt_b21_Punready_Punready_b30_Hidle),
                 (DcrL_b11_Tpause_Tpause_b21_Punready_Punready_b30_Hidle,T_pass,DcrL_b11_Twt_Twt_b21_Punready_Punready_b30_Hidle), (DcrL_b11_Tpause_Tpause_b21_Punready_Punready_b30_Hidle,P_eoc,DcrL_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hidle), (DcrL_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hidle,D_wpa,DcrL1_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hidle),
                 (DcrL_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hidle,T_test,DcrL_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hidle), (DcrL_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hidle,H_start,DcrL_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy), (Dm1stwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy,D_wpa,D2ndwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy),
                 (Dm1stwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy,D_creogl,DcrL_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy), (Dm1stwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy,H_stop,Dm1stwp_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle), (DcrL1_b11_Twt_Twt_b21_Pready_Pready_b30_Hidle,P_open,DcrL1_b11_Twt_Twt_b21_Punready_Punready_b30_Hidle),
                 (DcrL1_b11_Tpause_Tpause_b21_Punready_Punready_b30_Hidle,T_fail,DcrL1_b11_Trt_Trt_b21_Punready_Punready_b30_Hidle), (DcrL1_b11_Tpause_Tpause_b21_Punready_Punready_b30_Hidle,T_pass,DcrL1_b11_Twt_Twt_b21_Punready_Punready_b30_Hidle), (DcrL1_b11_Tpause_Tpause_b21_Punready_Punready_b30_Hidle,P_eoc,DcrL1_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hidle),
                 (DcrL1_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hidle,T_test,DcrL1_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hidle), (DcrL1_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hidle,H_start,DcrL1_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy), (D2ndwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy,D_creogl,DcrL1_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy),
                 (D2ndwp_b10_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy,H_stop,D2ndwp_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle), (DcrL_b11_Twt_Twt_b21_Punready_Punready_b30_Hidle,D_wpa,DcrL1_b11_Twt_Twt_b21_Punready_Punready_b30_Hidle), (DcrL_b11_Twt_Twt_b21_Punready_Punready_b30_Hidle,P_eoc,DcrL_b11_Twt_Twt_b20_Pready_Pready_b31_Hidle),
                 (DcrL_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hidle,D_wpa,DcrL1_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hidle), (DcrL_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hidle,T_fail,DcrL_b11_Trt_Trt_b20_Pready_Pready_b31_Hidle), (DcrL_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hidle,T_pass,DcrL_b11_Twt_Twt_b20_Pready_Pready_b31_Hidle),
                 (DcrL_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hidle,H_start,DcrL_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hbusy), (DcrL_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy,D_wpa,DcrL1_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy), (DcrL_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy,T_test,DcrL_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hbusy),
                 (DcrL_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy,H_stop,DcrL_b11_Tidle_Tidle_b20_Pready_Pready_b30_Hidle), (DcrL1_b11_Twt_Twt_b21_Punready_Punready_b30_Hidle,P_eoc,DcrL1_b11_Twt_Twt_b20_Pready_Pready_b31_Hidle), (DcrL1_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hidle,T_fail,DcrL1_b11_Trt_Trt_b20_Pready_Pready_b31_Hidle),
                 (DcrL1_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hidle,T_pass,DcrL1_b11_Twt_Twt_b20_Pready_Pready_b31_Hidle), (DcrL1_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hidle,H_start,DcrL1_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hbusy), (DcrL1_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy,T_test,DcrL1_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hbusy),
                 (DcrL1_b11_Tidle_Tidle_b20_Pready_Pready_b31_Hbusy,H_stop,DcrL1_b11_Tidle_Tidle_b20_Pready_Pready_b30_Hidle), (DcrL_b11_Twt_Twt_b20_Pready_Pready_b31_Hidle,D_wpa,DcrL1_b11_Twt_Twt_b20_Pready_Pready_b31_Hidle), (DcrL_b11_Twt_Twt_b20_Pready_Pready_b31_Hidle,T_twp,DcrL_b11_Tit_Tit_b20_Pready_Pready_b31_Hidle),
                 (DcrL_b11_Twt_Twt_b20_Pready_Pready_b31_Hidle,H_start,DcrL_b11_Twt_Twt_b20_Pready_Pready_b31_Hbusy), (DcrL_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hbusy,D_wpa,DcrL1_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hbusy), (DcrL_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hbusy,T_fail,DcrL_b11_Trt_Trt_b20_Pready_Pready_b31_Hbusy),
                 (DcrL_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hbusy,T_pass,DcrL_b11_Twt_Twt_b20_Pready_Pready_b31_Hbusy), (DcrL_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hbusy,H_stop,DcrL_b11_Tpause_Tpause_b20_Pready_Pready_b30_Hidle), (DcrL1_b11_Twt_Twt_b20_Pready_Pready_b31_Hidle,T_twp,DcrL1_b11_Tit_Tit_b20_Pready_Pready_b31_Hidle),
                 (DcrL1_b11_Twt_Twt_b20_Pready_Pready_b31_Hidle,H_start,DcrL1_b11_Twt_Twt_b20_Pready_Pready_b31_Hbusy), (DcrL1_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hbusy,T_fail,DcrL1_b11_Trt_Trt_b20_Pready_Pready_b31_Hbusy), (DcrL1_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hbusy,T_pass,DcrL1_b11_Twt_Twt_b20_Pready_Pready_b31_Hbusy),
                 (DcrL1_b11_Tpause_Tpause_b20_Pready_Pready_b31_Hbusy,H_stop,DcrL1_b11_Tpause_Tpause_b20_Pready_Pready_b30_Hidle), (DcrL_b11_Tit_Tit_b20_Pready_Pready_b31_Hidle,D_wpa,DcrL1_b11_Tit_Tit_b20_Pready_Pready_b31_Hidle), (DcrL_b11_Tit_Tit_b20_Pready_Pready_b31_Hidle,T_eotwp,DcrL_b11_Trt_Trt_b21_Pready_Pready_b31_Hidle),
                 (DcrL_b11_Tit_Tit_b20_Pready_Pready_b31_Hidle,H_start,DcrL_b11_Tit_Tit_b20_Pready_Pready_b31_Hbusy), (DcrL_b11_Twt_Twt_b20_Pready_Pready_b31_Hbusy,D_wpa,DcrL1_b11_Twt_Twt_b20_Pready_Pready_b31_Hbusy), (DcrL_b11_Twt_Twt_b20_Pready_Pready_b31_Hbusy,T_twp,DcrL_b11_Tit_Tit_b20_Pready_Pready_b31_Hbusy),
                 (DcrL_b11_Twt_Twt_b20_Pready_Pready_b31_Hbusy,H_stop,DcrL_b11_Twt_Twt_b20_Pready_Pready_b30_Hidle), (DcrL1_b11_Tit_Tit_b20_Pready_Pready_b31_Hidle,T_eotwp,DcrL1_b11_Trt_Trt_b21_Pready_Pready_b31_Hidle), (DcrL1_b11_Tit_Tit_b20_Pready_Pready_b31_Hidle,H_start,DcrL1_b11_Tit_Tit_b20_Pready_Pready_b31_Hbusy),
                 (DcrL1_b11_Twt_Twt_b20_Pready_Pready_b31_Hbusy,T_twp,DcrL1_b11_Tit_Tit_b20_Pready_Pready_b31_Hbusy), (DcrL1_b11_Twt_Twt_b20_Pready_Pready_b31_Hbusy,H_stop,DcrL1_b11_Twt_Twt_b20_Pready_Pready_b30_Hidle), (DcrL_b11_Trt_Trt_b21_Pready_Pready_b31_Hidle,D_wpa,DcrL1_b11_Trt_Trt_b21_Pready_Pready_b31_Hidle),
                 (DcrL_b11_Trt_Trt_b21_Pready_Pready_b31_Hidle,T_eot,DcrL_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle), (DcrL_b11_Trt_Trt_b21_Pready_Pready_b31_Hidle,H_start,DcrL_b11_Trt_Trt_b21_Pready_Pready_b31_Hbusy), (DcrL_b11_Tit_Tit_b20_Pready_Pready_b31_Hbusy,D_wpa,DcrL1_b11_Tit_Tit_b20_Pready_Pready_b31_Hbusy),
                 (DcrL_b11_Tit_Tit_b20_Pready_Pready_b31_Hbusy,T_eotwp,DcrL_b11_Trt_Trt_b21_Pready_Pready_b31_Hbusy), (DcrL_b11_Tit_Tit_b20_Pready_Pready_b31_Hbusy,H_stop,DcrL_b11_Tit_Tit_b20_Pready_Pready_b30_Hidle), (DcrL1_b11_Trt_Trt_b21_Pready_Pready_b31_Hidle,T_eot,DcrL1_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle),
                 (DcrL1_b11_Trt_Trt_b21_Pready_Pready_b31_Hidle,H_start,DcrL1_b11_Trt_Trt_b21_Pready_Pready_b31_Hbusy), (DcrL1_b11_Tit_Tit_b20_Pready_Pready_b31_Hbusy,T_eotwp,DcrL1_b11_Trt_Trt_b21_Pready_Pready_b31_Hbusy), (DcrL1_b11_Tit_Tit_b20_Pready_Pready_b31_Hbusy,H_stop,DcrL1_b11_Tit_Tit_b20_Pready_Pready_b30_Hidle),
                 (DcrL_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle,D_wpa,DcrL1_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle), (DcrL_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle,D_release,DcrR_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle), (DcrL_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle,H_start,DcrL_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy),
                 (DcrL_b11_Trt_Trt_b21_Pready_Pready_b31_Hbusy,D_wpa,DcrL1_b11_Trt_Trt_b21_Pready_Pready_b31_Hbusy), (DcrL_b11_Trt_Trt_b21_Pready_Pready_b31_Hbusy,T_eot,DcrL_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy), (DcrL_b11_Trt_Trt_b21_Pready_Pready_b31_Hbusy,H_stop,DcrL_b11_Trt_Trt_b21_Pready_Pready_b30_Hidle),
                 (DcrL1_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle,D_release,Dm1stwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle), (DcrL1_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle,H_start,DcrL1_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy), (DcrL1_b11_Trt_Trt_b21_Pready_Pready_b31_Hbusy,T_eot,DcrL1_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy),
                 (DcrL1_b11_Trt_Trt_b21_Pready_Pready_b31_Hbusy,H_stop,DcrL1_b11_Trt_Trt_b21_Pready_Pready_b30_Hidle), (DcrR_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle,D_wpa,Dm1stwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle), (DcrR_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle,H_start,DcrR_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy),
                 (DcrL_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy,D_wpa,DcrL1_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy), (DcrL_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy,D_release,DcrR_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy), (DcrL_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy,H_stop,DcrL_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle),
                 (Dm1stwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle,D_wpa,D2ndwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle), (Dm1stwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle,D_creogl,DcrL_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hidle), (Dm1stwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle,H_start,Dm1stwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy),
                 (DcrL1_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy,D_release,Dm1stwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy), (DcrL1_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy,H_stop,DcrL1_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle), (DcrR_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy,D_wpa,Dm1stwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy),
                 (DcrR_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy,H_stop,DcrR_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle), (D2ndwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle,D_creogl,DcrL1_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hidle), (D2ndwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hidle,H_start,D2ndwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy),
                 (DcrL_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hidle,D_wpa,DcrL1_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hidle), (DcrL_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hidle,T_test,DcrL_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hidle), (DcrL_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hidle,H_start,DcrL_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy),
                 (Dm1stwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy,D_wpa,D2ndwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy), (Dm1stwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy,D_creogl,DcrL_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy), (Dm1stwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy,H_stop,Dm1stwp_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle),
                 (DcrL1_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hidle,T_test,DcrL1_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hidle), (DcrL1_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hidle,H_start,DcrL1_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy), (D2ndwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy,D_creogl,DcrL1_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy),
                 (D2ndwp_b10_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy,H_stop,D2ndwp_b10_Tidle_Tidle_b21_Pready_Pready_b30_Hidle), (DcrL_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hidle,D_wpa,DcrL1_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hidle), (DcrL_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hidle,T_fail,DcrL_b11_Trt_Trt_b21_Pready_Pready_b31_Hidle),
                 (DcrL_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hidle,T_pass,DcrL_b11_Twt_Twt_b21_Pready_Pready_b31_Hidle), (DcrL_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hidle,H_start,DcrL_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hbusy), (DcrL_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy,D_wpa,DcrL1_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy),
                 (DcrL_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy,T_test,DcrL_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hbusy), (DcrL_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy,H_stop,DcrL_b11_Tidle_Tidle_b21_Pready_Pready_b30_Hidle), (DcrL1_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hidle,T_fail,DcrL1_b11_Trt_Trt_b21_Pready_Pready_b31_Hidle),
                 (DcrL1_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hidle,T_pass,DcrL1_b11_Twt_Twt_b21_Pready_Pready_b31_Hidle), (DcrL1_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hidle,H_start,DcrL1_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hbusy), (DcrL1_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy,T_test,DcrL1_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hbusy),
                 (DcrL1_b11_Tidle_Tidle_b21_Pready_Pready_b31_Hbusy,H_stop,DcrL1_b11_Tidle_Tidle_b21_Pready_Pready_b30_Hidle), (DcrL_b11_Twt_Twt_b21_Pready_Pready_b31_Hidle,D_wpa,DcrL1_b11_Twt_Twt_b21_Pready_Pready_b31_Hidle), (DcrL_b11_Twt_Twt_b21_Pready_Pready_b31_Hidle,H_start,DcrL_b11_Twt_Twt_b21_Pready_Pready_b31_Hbusy),
                 (DcrL_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hbusy,D_wpa,DcrL1_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hbusy), (DcrL_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hbusy,T_fail,DcrL_b11_Trt_Trt_b21_Pready_Pready_b31_Hbusy), (DcrL_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hbusy,T_pass,DcrL_b11_Twt_Twt_b21_Pready_Pready_b31_Hbusy),
                 (DcrL_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hbusy,H_stop,DcrL_b11_Tpause_Tpause_b21_Pready_Pready_b30_Hidle), (DcrL1_b11_Twt_Twt_b21_Pready_Pready_b31_Hidle,H_start,DcrL1_b11_Twt_Twt_b21_Pready_Pready_b31_Hbusy), (DcrL1_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hbusy,T_fail,DcrL1_b11_Trt_Trt_b21_Pready_Pready_b31_Hbusy),
                 (DcrL1_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hbusy,T_pass,DcrL1_b11_Twt_Twt_b21_Pready_Pready_b31_Hbusy), (DcrL1_b11_Tpause_Tpause_b21_Pready_Pready_b31_Hbusy,H_stop,DcrL1_b11_Tpause_Tpause_b21_Pready_Pready_b30_Hidle), (DcrL_b11_Twt_Twt_b21_Pready_Pready_b31_Hbusy,D_wpa,DcrL1_b11_Twt_Twt_b21_Pready_Pready_b31_Hbusy),
                 (DcrL_b11_Twt_Twt_b21_Pready_Pready_b31_Hbusy,H_stop,DcrL_b11_Twt_Twt_b21_Pready_Pready_b30_Hidle), (DcrL1_b11_Twt_Twt_b21_Pready_Pready_b31_Hbusy,H_stop,DcrL1_b11_Twt_Twt_b21_Pready_Pready_b30_Hidle) },
            x_0: DcrR_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle,
            Xm: {DcrR_b10_Tidle_Tidle_b20_Pready_Pready_b30_Hidle},
            ctrlEvents: {} )
 &
 G_FB : Generator_C(States,Events)
 
VARIABLES
 code

INITIALISATION
 code := 0

INVARIANT
 code >= 0 & code <= 1

OPERATIONS
 checkCoreachability = PRE code = 0 THEN
  SELECT card(coReachable(G_FB)) = card(G_FB'states) THEN
   code := 1; 
   PRINT("The languages are locally modular.");
   PRINTF("Number of coreachable states = ~w.~n", [card(G_FB'states)]);
   PRINTF("Number of transitions = ~w~n.", [card(G_FB'tf)]) END END END

/*
OUTPUT:
% Runtime for SOLUTION for SETUP_CONSTANTS: 50 ms (walltime: 50 ms)
% Finished searching for valid initial states 70 ms walltime (70 ms runtime), since start: 1670 ms
% Starting random_animate (max_steps=3)
perform_random_step(1,1)
random(2,1)
perform_random_step(1,2)
random(2,1)
"The languages are locally modular."
Number of coreachable states = 97.
Number of transitions = 230
.
ALL OPERATIONS COVERED

perform_random_step(1,3)
random(2,1)
finished_random_animate(3,[runtime/710,total_runtime/710,walltime/720])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 840 ms walltime (830 ms runtime), since start: 2390 ms

% Finished process_file (errors=0, warnings=0)

*/