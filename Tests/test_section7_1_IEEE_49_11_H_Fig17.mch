// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL test_section7_1_IEEE_49_11_H_Fig17

SEES
 //---------------------------------------------------------------------
 // Machine for input data
 //---------------------------------------------------------------------
 // not conditionally C&P coobservable and not conditionally D&A coobservable
 // (Sigma_ce = {s, p} and Sigma_cd = {s, p})
 // or
 // conditionally C&P coobservable and conditionally D&A coobservable
 // w.r.t. the general architecture thus conditionally coobservable
 // w.r.t. the general architecture (Sigma_ce = {p} and Sigma_cd = {s}).
 IEEE_49_11_H_Fig17 /*@file "../DataExamples/IEEE_49_11_H_Fig17.mch" */

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 1000000;
 SET_PREF_SMT == TRUE;   // to avoid a warning
 SET_PREF_KODKOD == FALSE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../Theories/SCT_core.def";
 "../Theories/SCT_decentralized.def";
 "LibraryIO.def"

CONSTANTS
 L_G, L_H, T_G, T_H, Lm_G, Lm_H  // Languages for free and legal behaviors

PROPERTIES
 G_fb : Generator_C_O(G_fb'states,G_fb'events) &
 H_lb : FSA(H_lb'states,H_lb'events) &
 T_G = GetTracesNc1stNqcSev(G_fb,bound,{p,s}) &
 L_G = Traces2Strings(T_G) &
 Lm_G = Traces2Tasks(T_G,G_fb'Xm) &
 T_H = GetTracesNc1stNqcSev(H_lb,bound,{p,s}) &
 L_H = Traces2Strings(T_H) &
 Lm_H = Traces2Tasks(T_H,H_lb'Xm)

VARIABLES
 S_d_i, S_dc_i, S_e_i, S_ec_i,    // Local decisions
 S_d, S_e                         // Disabling and enabling global decisions

INITIALISATION
 S_d_i := {};  S_dc_i := {};  S_e_i := {};  S_ec_i := {};
 S_d := {};  S_e := {}

INVARIANT
 S_d_i <: def_Sl_d_dc_e_ec(L_G,L_H,G_fb'events) &
 S_dc_i <: def_Sl_d_dc_e_ec(L_G,L_H,G_fb'events) &
 S_e_i <: def_Sl_d_dc_e_ec(L_G,L_H,G_fb'events) &
 S_ec_i <: def_Sl_d_dc_e_ec(L_G,L_H,G_fb'events) &
 S_d <: def_Sg_e_d(L_G,G_fb'events) &
 S_e <: def_Sg_e_d(L_G,G_fb'events)

OPERATIONS
VerifyLmClosure =
  SELECT Lm_closed(L_H,Lm_H,Lm_G) THEN
   PRINT("K is Lm(G)-closed.") ELSE
   PRINT("K is not Lm(G)-closed.") END;

 VerifyCtrl =
  SELECT Controllability(L_H,L_G,G_fb'events-G_fb'ctrlEvents) THEN
   PRINT("K is controllable.") ELSE
   PRINT("K is uncontrollable.") END;

 VerifyGCCoobservability =
  SELECT GCCoobservability(A_d,A_d'Events_ce,A_d'Events_cd,L_G,L_H) THEN
   PRINT("K is conditionally coobservable.");
   S_e_i := LocalDecisionsEnbl(A_d,L_G,L_H);
   PRINTF("Local decisions (enabled unconditionally): ~n ~w~n",[S_e_i]);
   S_ec_i := LocalDecisionsEnbl_c(A_d,L_G,L_H);
   PRINTF("Local decisions (enabled conditionally): ~n ~w~n",[S_ec_i]);
   S_d_i := LocalDecisionsDsbl(A_d,L_G,L_H);
   PRINTF("Local decisions (disabled unconditionally): ~n ~w~n",[S_d_i]);
   S_dc_i := LocalDecisionsDsbl_c(A_d,L_G,L_H);
   PRINTF("Local decisions (disabled conditionally): ~n ~w~n",[S_dc_i]);
   S_e := GlobalDecisionDA(A_d,S_e_i,S_d_i,S_ec_i);
   PRINTF("Global decisions (enabled): ~n ~w~n",[S_e]);
   S_d := GlobalDecisionCP(A_d,S_e_i,S_d_i,S_dc_i);
   PRINTF("Global decisions (disabled): ~n ~w~n",[S_d]);
   IF CtrlNonconflicting(S_e,S_d) THEN
      PRINT("The control is nonconflicting.") ELSE
      PRINT("The control is conflicting.") END ELSE
    PRINT("K is not conditionally coobservable.") END END