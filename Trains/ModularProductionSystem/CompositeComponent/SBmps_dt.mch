// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBmps_dt

SETS
 States = {DcrR_b10_Tidle, Dm1stwp_b10_Tidle, DcrR_b10_Tpause,
           DcrL_b11_Tidle, Dm1stwp_b10_Tpause, D2ndwp_b10_Tidle,
           DcrR_b10_Trt, DcrR_b10_Twt, DcrR_b11_Tidle,
           DcrL_b11_Tpause, DcrL1_b11_Tidle, Dm1stwp_b10_Trt,
           Dm1stwp_b10_Twt, D2ndwp_b10_Tpause, DcrR_b10_Tit,
           Dm1stwp_b11_Tidle, DcrR_b11_Tpause, DcrL_b11_Trt,
           DcrL_b11_Twt, DcrL1_b11_Tpause, D2ndwp_b10_Trt,
           Dm1stwp_b10_Tit, D2ndwp_b10_Twt, Dm1stwp_b11_Tpause,
           D2ndwp_b11_Tidle, DcrR_b11_Trt, DcrR_b11_Twt,
           DcrL_b10_Tidle, DcrL1_b11_Trt, DcrL_b11_Tit,
           DcrL1_b11_Twt, D2ndwp_b10_Tit, Dm1stwp_b11_Trt,
           Dm1stwp_b11_Twt, D2ndwp_b11_Tpause, DcrR_b11_Tit,
           DcrL_b10_Tpause, DcrL1_b10_Tidle, DcrL1_b11_Tit,
           D2ndwp_b11_Trt, Dm1stwp_b11_Tit, D2ndwp_b11_Twt,
           DcrL_b10_Trt, DcrL_b10_Twt, DcrL1_b10_Tpause,
           D2ndwp_b11_Tit, DcrL1_b10_Trt, DcrL_b10_Tit,
           DcrL1_b10_Twt, DcrL1_b10_Tit };
 Events = {D_wpa, D_release, D_creogl, T_eot, T_eotwp, T_fail, T_pass, T_test, T_twp};
 LocalStates = {DcrR, Dm1stwp, D2ndwp, DcrL, DcrL1, b10, b11, Tidle, Tit, Tpause, Trt, Twt}

CONSTANTS
 G_FB,
 distribution, buffer1, testing,
 Qpred, BadStates

PROPERTIES
 G_FB = rec(       // Automaton modeling the free behavior
            states: States,
            events: Events,
            tf: {(DcrR_b10_Tidle,D_wpa,Dm1stwp_b10_Tidle), (DcrR_b10_Tidle,T_test,DcrR_b10_Tpause), (Dm1stwp_b10_Tidle,D_creogl,DcrL_b11_Tidle),
                 (Dm1stwp_b10_Tidle,T_test,Dm1stwp_b10_Tpause), (Dm1stwp_b10_Tidle,D_wpa,D2ndwp_b10_Tidle), (DcrR_b10_Tpause,D_wpa,Dm1stwp_b10_Tpause),
                 (DcrR_b10_Tpause,T_fail,DcrR_b10_Trt), (DcrR_b10_Tpause,T_pass,DcrR_b10_Twt), (DcrL_b11_Tidle,D_release,DcrR_b11_Tidle),
                 (DcrL_b11_Tidle,T_test,DcrL_b11_Tpause), (DcrL_b11_Tidle,D_wpa,DcrL1_b11_Tidle), (Dm1stwp_b10_Tpause,D_creogl,DcrL_b11_Tpause),
                 (Dm1stwp_b10_Tpause,T_fail,Dm1stwp_b10_Trt), (Dm1stwp_b10_Tpause,T_pass,Dm1stwp_b10_Twt), (Dm1stwp_b10_Tpause,D_wpa,D2ndwp_b10_Tpause),
                 (D2ndwp_b10_Tidle,D_creogl,DcrL1_b11_Tidle), (D2ndwp_b10_Tidle,T_test,D2ndwp_b10_Tpause), (DcrR_b10_Trt,D_wpa,Dm1stwp_b10_Trt),
                 (DcrR_b10_Trt,T_eot,DcrR_b10_Tidle), (DcrR_b10_Twt,D_wpa,Dm1stwp_b10_Twt), (DcrR_b10_Twt,T_twp,DcrR_b10_Tit),
                 (DcrR_b11_Tidle,D_wpa,Dm1stwp_b11_Tidle), (DcrR_b11_Tidle,T_test,DcrR_b11_Tpause), (DcrL_b11_Tpause,D_release,DcrR_b11_Tpause),
                 (DcrL_b11_Tpause,T_fail,DcrL_b11_Trt), (DcrL_b11_Tpause,T_pass,DcrL_b11_Twt), (DcrL_b11_Tpause,D_wpa,DcrL1_b11_Tpause),
                 (DcrL1_b11_Tidle,D_release,Dm1stwp_b11_Tidle), (DcrL1_b11_Tidle,T_test,DcrL1_b11_Tpause), (Dm1stwp_b10_Trt,D_creogl,DcrL_b11_Trt),
                 (Dm1stwp_b10_Trt,T_eot,Dm1stwp_b10_Tidle), (Dm1stwp_b10_Trt,D_wpa,D2ndwp_b10_Trt), (Dm1stwp_b10_Twt,D_creogl,DcrL_b11_Twt),
                 (Dm1stwp_b10_Twt,T_twp,Dm1stwp_b10_Tit), (Dm1stwp_b10_Twt,D_wpa,D2ndwp_b10_Twt), (D2ndwp_b10_Tpause,D_creogl,DcrL1_b11_Tpause),
                 (D2ndwp_b10_Tpause,T_fail,D2ndwp_b10_Trt), (D2ndwp_b10_Tpause,T_pass,D2ndwp_b10_Twt), (DcrR_b10_Tit,D_wpa,Dm1stwp_b10_Tit),
                 (DcrR_b10_Tit,T_eotwp,DcrR_b10_Trt), (Dm1stwp_b11_Tidle,T_test,Dm1stwp_b11_Tpause), (Dm1stwp_b11_Tidle,D_creogl,DcrL_b11_Tidle),
                 (Dm1stwp_b11_Tidle,D_wpa,D2ndwp_b11_Tidle), (DcrR_b11_Tpause,D_wpa,Dm1stwp_b11_Tpause), (DcrR_b11_Tpause,T_fail,DcrR_b11_Trt),
                 (DcrR_b11_Tpause,T_pass,DcrR_b11_Twt), (DcrL_b11_Trt,D_release,DcrR_b11_Trt), (DcrL_b11_Trt,T_eot,DcrL_b10_Tidle),
                 (DcrL_b11_Trt,D_wpa,DcrL1_b11_Trt), (DcrL_b11_Twt,D_release,DcrR_b11_Twt), (DcrL_b11_Twt,T_twp,DcrL_b11_Tit),
                 (DcrL_b11_Twt,D_wpa,DcrL1_b11_Twt), (DcrL1_b11_Tpause,D_release,Dm1stwp_b11_Tpause), (DcrL1_b11_Tpause,T_fail,DcrL1_b11_Trt),
                 (DcrL1_b11_Tpause,T_pass,DcrL1_b11_Twt), (D2ndwp_b10_Trt,D_creogl,DcrL1_b11_Trt), (D2ndwp_b10_Trt,T_eot,D2ndwp_b10_Tidle),
                 (Dm1stwp_b10_Tit,D_creogl,DcrL_b11_Tit), (Dm1stwp_b10_Tit,T_eotwp,Dm1stwp_b10_Trt), (Dm1stwp_b10_Tit,D_wpa,D2ndwp_b10_Tit),
                 (D2ndwp_b10_Twt,D_creogl,DcrL1_b11_Twt), (D2ndwp_b10_Twt,T_twp,D2ndwp_b10_Tit), (Dm1stwp_b11_Tpause,T_fail,Dm1stwp_b11_Trt),
                 (Dm1stwp_b11_Tpause,T_pass,Dm1stwp_b11_Twt), (Dm1stwp_b11_Tpause,D_creogl,DcrL_b11_Tpause), (Dm1stwp_b11_Tpause,D_wpa,D2ndwp_b11_Tpause),
                 (D2ndwp_b11_Tidle,T_test,D2ndwp_b11_Tpause), (D2ndwp_b11_Tidle,D_creogl,DcrL1_b11_Tidle), (DcrR_b11_Trt,D_wpa,Dm1stwp_b11_Trt),
                 (DcrR_b11_Trt,T_eot,DcrR_b10_Tidle), (DcrR_b11_Twt,D_wpa,Dm1stwp_b11_Twt), (DcrR_b11_Twt,T_twp,DcrR_b11_Tit),
                 (DcrL_b10_Tidle,D_release,DcrR_b10_Tidle), (DcrL_b10_Tidle,T_test,DcrL_b10_Tpause), (DcrL_b10_Tidle,D_wpa,DcrL1_b10_Tidle),
                 (DcrL1_b11_Trt,D_release,Dm1stwp_b11_Trt), (DcrL1_b11_Trt,T_eot,DcrL1_b10_Tidle), (DcrL_b11_Tit,D_release,DcrR_b11_Tit),
                 (DcrL_b11_Tit,T_eotwp,DcrL_b11_Trt), (DcrL_b11_Tit,D_wpa,DcrL1_b11_Tit), (DcrL1_b11_Twt,D_release,Dm1stwp_b11_Twt),
                 (DcrL1_b11_Twt,T_twp,DcrL1_b11_Tit), (D2ndwp_b10_Tit,D_creogl,DcrL1_b11_Tit), (D2ndwp_b10_Tit,T_eotwp,D2ndwp_b10_Trt),
                 (Dm1stwp_b11_Trt,T_eot,Dm1stwp_b10_Tidle), (Dm1stwp_b11_Trt,D_creogl,DcrL_b11_Trt), (Dm1stwp_b11_Trt,D_wpa,D2ndwp_b11_Trt),
                 (Dm1stwp_b11_Twt,T_twp,Dm1stwp_b11_Tit), (Dm1stwp_b11_Twt,D_creogl,DcrL_b11_Twt), (Dm1stwp_b11_Twt,D_wpa,D2ndwp_b11_Twt),
                 (D2ndwp_b11_Tpause,T_fail,D2ndwp_b11_Trt), (D2ndwp_b11_Tpause,T_pass,D2ndwp_b11_Twt), (D2ndwp_b11_Tpause,D_creogl,DcrL1_b11_Tpause),
                 (DcrR_b11_Tit,D_wpa,Dm1stwp_b11_Tit), (DcrR_b11_Tit,T_eotwp,DcrR_b11_Trt), (DcrL_b10_Tpause,D_release,DcrR_b10_Tpause),
                 (DcrL_b10_Tpause,T_fail,DcrL_b10_Trt), (DcrL_b10_Tpause,T_pass,DcrL_b10_Twt), (DcrL_b10_Tpause,D_wpa,DcrL1_b10_Tpause),
                 (DcrL1_b10_Tidle,D_release,Dm1stwp_b10_Tidle), (DcrL1_b10_Tidle,T_test,DcrL1_b10_Tpause), (DcrL1_b11_Tit,D_release,Dm1stwp_b11_Tit),
                 (DcrL1_b11_Tit,T_eotwp,DcrL1_b11_Trt), (D2ndwp_b11_Trt,T_eot,D2ndwp_b10_Tidle), (D2ndwp_b11_Trt,D_creogl,DcrL1_b11_Trt),
                 (Dm1stwp_b11_Tit,T_eotwp,Dm1stwp_b11_Trt), (Dm1stwp_b11_Tit,D_creogl,DcrL_b11_Tit), (Dm1stwp_b11_Tit,D_wpa,D2ndwp_b11_Tit),
                 (D2ndwp_b11_Twt,T_twp,D2ndwp_b11_Tit), (D2ndwp_b11_Twt,D_creogl,DcrL1_b11_Twt), (DcrL_b10_Trt,D_release,DcrR_b10_Trt),
                 (DcrL_b10_Trt,T_eot,DcrL_b10_Tidle), (DcrL_b10_Trt,D_wpa,DcrL1_b10_Trt), (DcrL_b10_Twt,D_release,DcrR_b10_Twt),
                 (DcrL_b10_Twt,T_twp,DcrL_b10_Tit), (DcrL_b10_Twt,D_wpa,DcrL1_b10_Twt), (DcrL1_b10_Tpause,D_release,Dm1stwp_b10_Tpause),
                 (DcrL1_b10_Tpause,T_fail,DcrL1_b10_Trt), (DcrL1_b10_Tpause,T_pass,DcrL1_b10_Twt), (D2ndwp_b11_Tit,T_eotwp,D2ndwp_b11_Trt),
                 (D2ndwp_b11_Tit,D_creogl,DcrL1_b11_Tit), (DcrL1_b10_Trt,D_release,Dm1stwp_b10_Trt), (DcrL1_b10_Trt,T_eot,DcrL1_b10_Tidle),
                 (DcrL_b10_Tit,D_release,DcrR_b10_Tit), (DcrL_b10_Tit,T_eotwp,DcrL_b10_Trt), (DcrL_b10_Tit,D_wpa,DcrL1_b10_Tit),
                 (DcrL1_b10_Twt,D_release,Dm1stwp_b10_Twt), (DcrL1_b10_Twt,T_twp,DcrL1_b10_Tit), (DcrL1_b10_Tit,D_release,Dm1stwp_b10_Tit),
                 (DcrL1_b10_Tit,T_eotwp,DcrL1_b10_Trt) },
            x_0: DcrR_b10_Tidle,
            Xm: {DcrR_b10_Tidle},
            ctrlEvents: {D_release, T_test, T_twp} )
 &
 distribution = {
      (DcrR_b10_Tidle,DcrR),(Dm1stwp_b10_Tidle,Dm1stwp),(DcrR_b10_Tpause,DcrR),
      (DcrL_b11_Tidle,DcrL),(Dm1stwp_b10_Tpause,Dm1stwp),(D2ndwp_b10_Tidle,D2ndwp),
      (DcrR_b10_Trt,DcrR),(DcrR_b10_Twt,DcrR),(DcrR_b11_Tidle,DcrR),
      (DcrL_b11_Tpause,DcrL),(DcrL1_b11_Tidle,DcrL1),(Dm1stwp_b10_Trt,Dm1stwp),
      (Dm1stwp_b10_Twt,Dm1stwp),(D2ndwp_b10_Tpause,D2ndwp),(DcrR_b10_Tit,DcrR),
      (Dm1stwp_b11_Tidle,Dm1stwp),(DcrR_b11_Tpause,DcrR),(DcrL_b11_Trt,DcrL),
      (DcrL_b11_Twt,DcrL),(DcrL1_b11_Tpause,DcrL1),(D2ndwp_b10_Trt,D2ndwp),
      (Dm1stwp_b10_Tit,Dm1stwp),(D2ndwp_b10_Twt,D2ndwp),(Dm1stwp_b11_Tpause,Dm1stwp),
      (D2ndwp_b11_Tidle,D2ndwp),(DcrR_b11_Trt,DcrR),(DcrR_b11_Twt,DcrR),
      (DcrL_b10_Tidle,DcrL),(DcrL1_b11_Trt,DcrL1),(DcrL_b11_Tit,DcrL),
      (DcrL1_b11_Twt,DcrL1),(D2ndwp_b10_Tit,D2ndwp),(Dm1stwp_b11_Trt,Dm1stwp),
      (Dm1stwp_b11_Twt,Dm1stwp),(D2ndwp_b11_Tpause,D2ndwp),(DcrR_b11_Tit,DcrR),
      (DcrL_b10_Tpause,DcrL),(DcrL1_b10_Tidle,DcrL1),(DcrL1_b11_Tit,DcrL1),
      (D2ndwp_b11_Trt,D2ndwp),(Dm1stwp_b11_Tit,Dm1stwp),(D2ndwp_b11_Twt,D2ndwp),
      (DcrL_b10_Trt,DcrL),(DcrL_b10_Twt,DcrL),(DcrL1_b10_Tpause,DcrL1),
      (D2ndwp_b11_Tit,D2ndwp),(DcrL1_b10_Trt,DcrL1),(DcrL_b10_Tit,DcrL),
      (DcrL1_b10_Twt,DcrL1),(DcrL1_b10_Tit,DcrL1) }
 &
 buffer1 = {
      (DcrR_b10_Tidle,b10),(Dm1stwp_b10_Tidle,b10),(DcrR_b10_Tpause,b10),
      (DcrL_b11_Tidle,b11),(Dm1stwp_b10_Tpause,b10),(D2ndwp_b10_Tidle,b10),
      (DcrR_b10_Trt,b10),(DcrR_b10_Twt,b10),(DcrR_b11_Tidle,b11),
      (DcrL_b11_Tpause,b11),(DcrL1_b11_Tidle,b11),(Dm1stwp_b10_Trt,b10),
      (Dm1stwp_b10_Twt,b10),(D2ndwp_b10_Tpause,b10),(DcrR_b10_Tit,b10),
      (Dm1stwp_b11_Tidle,b11),(DcrR_b11_Tpause,b11),(DcrL_b11_Trt,b11),
      (DcrL_b11_Twt,b11),(DcrL1_b11_Tpause,b11),(D2ndwp_b10_Trt,b10),
      (Dm1stwp_b10_Tit,b10),(D2ndwp_b10_Twt,b10),(Dm1stwp_b11_Tpause,b11),
      (D2ndwp_b11_Tidle,b11),(DcrR_b11_Trt,b11),(DcrR_b11_Twt,b11),
      (DcrL_b10_Tidle,b10),(DcrL1_b11_Trt,b11),(DcrL_b11_Tit,b11),
      (DcrL1_b11_Twt,b11),(D2ndwp_b10_Tit,b10),(Dm1stwp_b11_Trt,b11),
      (Dm1stwp_b11_Twt,b11),(D2ndwp_b11_Tpause,b11),(DcrR_b11_Tit,b11),
      (DcrL_b10_Tpause,b10),(DcrL1_b10_Tidle,b10),(DcrL1_b11_Tit,b11),
      (D2ndwp_b11_Trt,b11),(Dm1stwp_b11_Tit,b11),(D2ndwp_b11_Twt,b11),
      (DcrL_b10_Trt,b10),(DcrL_b10_Twt,b10),(DcrL1_b10_Tpause,b10),
      (D2ndwp_b11_Tit,b11),(DcrL1_b10_Trt,b10),(DcrL_b10_Tit,b10),
      (DcrL1_b10_Twt,b10),(DcrL1_b10_Tit,b10) }
 &
 testing = {
      (DcrR_b10_Tidle,Tidle),(Dm1stwp_b10_Tidle,Tidle),(DcrR_b10_Tpause,Tpause),
      (DcrL_b11_Tidle,Tidle),(Dm1stwp_b10_Tpause,Tpause),(D2ndwp_b10_Tidle,Tidle),
      (DcrR_b10_Trt,Trt),(DcrR_b10_Twt,Twt),(DcrR_b11_Tidle,Tidle),
      (DcrL_b11_Tpause,Tpause),(DcrL1_b11_Tidle,Tidle),(Dm1stwp_b10_Trt,Trt),
      (Dm1stwp_b10_Twt,Twt),(D2ndwp_b10_Tpause,Tpause),(DcrR_b10_Tit,Tit),
      (Dm1stwp_b11_Tidle,Tidle),(DcrR_b11_Tpause,Tpause),(DcrL_b11_Trt,Trt),
      (DcrL_b11_Twt,Twt),(DcrL1_b11_Tpause,Tpause),(D2ndwp_b10_Trt,Trt),
      (Dm1stwp_b10_Tit,Tit),(D2ndwp_b10_Twt,Twt),(Dm1stwp_b11_Tpause,Tpause),
      (D2ndwp_b11_Tidle,Tidle),(DcrR_b11_Trt,Trt),(DcrR_b11_Twt,Twt),
      (DcrL_b10_Tidle,Tidle),(DcrL1_b11_Trt,Trt),(DcrL_b11_Tit,Tit),
      (DcrL1_b11_Twt,Twt),(D2ndwp_b10_Tit,Tit),(Dm1stwp_b11_Trt,Trt),
      (Dm1stwp_b11_Twt,Twt),(D2ndwp_b11_Tpause,Tpause),(DcrR_b11_Tit,Tit),
      (DcrL_b10_Tpause,Tpause),(DcrL1_b10_Tidle,Tidle),(DcrL1_b11_Tit,Tit),
      (D2ndwp_b11_Trt,Trt),(Dm1stwp_b11_Tit,Tit),(D2ndwp_b11_Twt,Twt),
      (DcrL_b10_Trt,Trt),(DcrL_b10_Twt,Twt),(DcrL1_b10_Tpause,Tpause),
      (D2ndwp_b11_Tit,Tit),(DcrL1_b10_Trt,Trt),(DcrL_b10_Tit,Tit),
      (DcrL1_b10_Twt,Twt),(DcrL1_b10_Tit,Tit) }
 &
 /*
  Standard constraints:
     1. The first buffer can be declared free only when the the testing station is idle,
        otherwise it cannot accept a workpiece from the distribution station.
     2. When the first buffer is full, the distribution station cannot deliver a
        workpiece to the testing station.
*/
  BadStates =
// The first buffer is free and the testing station waits the completion of the
// measurement process after the rejection or transfer of a workpiece.
  { x | x : States & buffer1(x) = b10 & testing(x) = Trt} \/
// The first buffer is full and the distribution station is moving a workpiece when
// there are one or two workpieces in the distribution station.
  { x | x : States & buffer1(x) = b11 & distribution(x) : {Dm1stwp,D2ndwp}}
 &
 Qpred = States - BadStates
END

/*
OUTPUT:
The transition structure of the agent
 (i.e.,the solution of the control problem)
Number of states of the exhaustive model: 50
Number of transitions of the exhaustive model: 130
Cardinality of the predicate: 35
Supremal coreachable and controllable predicates (15):
{DcrR_b10_Tidle,Dm1stwp_b10_Tidle,DcrL_b11_Tidle,D2ndwp_b10_Tidle,
 DcrL_b11_Tpause,DcrL1_b11_Tidle,DcrL_b11_Trt,DcrL_b11_Twt,
 DcrL1_b11_Tpause,DcrL_b10_Tidle,DcrL1_b11_Trt,DcrL_b11_Tit,
 DcrL1_b11_Twt,DcrL1_b10_Tidle,DcrL1_b11_Tit}
Disabled events:
{(DcrR_b10_Tidle|->{T_test}),(Dm1stwp_b10_Tidle|->{T_test}),
 (DcrL_b11_Tidle|->{D_release}),(D2ndwp_b10_Tidle|->{T_test}),
 (DcrL_b11_Tpause|->{D_release}),(DcrL1_b11_Tidle|->{D_release}),
 (DcrL_b11_Trt|->{D_release}),(DcrL_b11_Twt|->{D_release}),
 (DcrL1_b11_Tpause|->{D_release}),(DcrL_b10_Tidle|->{T_test}),
 (DcrL1_b11_Trt|->{D_release}),(DcrL_b11_Tit|->{D_release}),
 (DcrL1_b11_Twt|->{D_release}),(DcrL1_b10_Tidle|->{T_test}),
 (DcrL1_b11_Tit|->{D_release})}
Enabled events:
{(DcrR_b10_Tidle|->{D_wpa}),(Dm1stwp_b10_Tidle|->{D_wpa,D_creogl}),
 (DcrL_b11_Tidle|->{D_wpa,T_test}),(D2ndwp_b10_Tidle|->{D_creogl}),
 (DcrL_b11_Tpause|->{D_wpa,T_fail,T_pass}),(DcrL1_b11_Tidle|->{T_test}),
 (DcrL_b11_Trt|->{D_wpa,T_eot}),(DcrL_b11_Twt|->{D_wpa,T_twp}),
 (DcrL1_b11_Tpause|->{T_fail,T_pass}),(DcrL_b10_Tidle|->{D_wpa,D_release}),
 (DcrL1_b11_Trt|->{T_eot}),(DcrL_b11_Tit|->{D_wpa,T_eotwp}),
 (DcrL1_b11_Twt|->{T_twp}),(DcrL1_b10_Tidle|->{D_release}),
 (DcrL1_b11_Tit|->{T_eotwp})}
Closed-loop system:
rec(Xm:{DcrR_b10_Tidle},
    ctrlEvents:{D_release,T_test,T_twp},
    events:{D_wpa,D_release,D_creogl,T_eot,T_eotwp,T_fail,T_pass,T_test,T_twp},
    states:{DcrR_b10_Tidle,Dm1stwp_b10_Tidle,DcrL_b11_Tidle,D2ndwp_b10_Tidle,
            DcrL_b11_Tpause,DcrL1_b11_Tidle,DcrL_b11_Trt,DcrL_b11_Twt,
            DcrL1_b11_Tpause,DcrL_b10_Tidle,DcrL1_b11_Trt,DcrL_b11_Tit,
            DcrL1_b11_Twt,DcrL1_b10_Tidle,DcrL1_b11_Tit},
    tf:{((DcrR_b10_Tidle|->D_wpa)|->Dm1stwp_b10_Tidle),            ((Dm1stwp_b10_Tidle|->D_wpa)|->D2ndwp_b10_Tidle),
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
    x_0:DcrR_b10_Tidle)
*/

/*
  ControlLaw(D_release) = b10 & Tidle
  ControlLaw(T_test) = b11 & Tidle
  ControlLaw(T_twp) = b11 & Twt
*/

/*
% Runtime for SOLUTION for SETUP_CONSTANTS: 100 ms (walltime: 100 ms)
% Finished searching for valid initial states 120 ms walltime (120 ms runtime), since start: 1220 ms
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
finished_random_animate(5,[runtime/740,total_runtime/740,walltime/740])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 920 ms walltime (900 ms runtime), since start: 1970 ms

% Finished process_file (errors=0, warnings=0)
*/