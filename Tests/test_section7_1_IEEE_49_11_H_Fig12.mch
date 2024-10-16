// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL test_section7_1_IEEE_49_11_H_Fig12

SEES
 //---------------------------------------------------------------------
 // Machine for input data
 //---------------------------------------------------------------------
 // L(H) is trivially observable w.r.t. Sigma_o = Sigma and Sigma_c = {c, d},
 // not conditionally C&P coobservable and not conditionally D&A coobservable
 // (Sigma_ce = {c, d} and Sigma_cd = {c, d}) and
 // Sigma_c_1 = {c, d}, Sigma_c_2 = {c, d} and
 // Sigma_o_1 = {a, ap, c, d}, Sigma_o_2 = {b, bp, c, d}.
 // or
 // conditionally C&P coobservable and conditionally D&A coobservable
 // w.r.t. the general architecture thus conditionally coobservable
 // w.r.t. the general architecture (Sigma_ce = {c} and Sigma_cd = {d}) and
 // Sigma_c_1 = {c, d}, Sigma_c_2 = {c, d} and
 // Sigma_o_1 = {a, ap, c, d}, Sigma_o_2 = {b, bp, c, d}.
 // or
 // L(H) is not observable w.r.t. Sigma_o = {} and Sigma_c = {c, d},
 // not conditionally C&P coobservable and not conditionally D&A coobservable
 // w.r.t. the general architecture thus not conditionally coobservable
 // w.r.t. the general architecture (Sigma_ce = {c,d} and Sigma_cd = {c,d}) and
 // Sigma_c_1 = {c, d}, Sigma_c_2 = {} and
 // Sigma_o_1 = {}, Sigma_o_2 = {a, ap, b, bp, c, d}
 IEEE_49_11_H_Fig12 /*@file "../DataExamples/IEEE_49_11_H_Fig12.mch" */

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 1000000;
 SET_PREF_SMT == TRUE;
 SET_PREF_KODKOD == FALSE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../Theories/SCT_core.def";
 "../Theories/SCT_decentralized.def";
 "LibraryIO.def"

CONSTANTS
 L_G, L_H               // Languages for free and legal behaviors

PROPERTIES
 G_fb : Generator_C_O(G_fb'states,G_fb'events) &
 H_lb : FSA(H_lb'states,H_lb'events) &
 L_G = GetStrings(G_fb,bound) &
 L_H = GetStrings(H_lb,bound)

VARIABLES
 S_d_i, S_dc_i, S_e_i, S_ec_i,    // Local decisions
 S_d, S_e                         // Disabling and enabling global decisions

INITIALISATION
 S_d_i := {};  S_dc_i := {};  S_e_i := {};  S_ec_i := {};
 S_d := {};  S_e := {}

INVARIANT
 S_d_i <: def_Sl_d_dc_e_ec(L_G,seq(G_fb'events),G_fb'events) &
 S_dc_i <: def_Sl_d_dc_e_ec(L_G,seq(G_fb'events),G_fb'events) &
 S_e_i <: def_Sl_d_dc_e_ec(L_G,seq(G_fb'events),G_fb'events) &
 S_ec_i <: def_Sl_d_dc_e_ec(L_G,seq(G_fb'events),G_fb'events) &
 S_d <: def_Sg_e_d(L_G,G_fb'events) &
 S_e <: def_Sg_e_d(L_G,G_fb'events)

OPERATIONS
 VerifyObservability1 =
  SELECT Observability(L_H,L_G,G_fb'obsEvents,G_fb'events) THEN
   PRINT("K is observable.") ELSE
   PRINT("K is not observable.") END;

 VerifyObservability2 =
  SELECT Observability(L_H,L_G,{},G_fb'ctrlEvents) THEN
   PRINT("K is observable when all events are unobservable.") ELSE
   PRINT("K is not observable when all events are unobservable.") END;

 VerifyGCCoobservability_ =
  SELECT GCCoobservability_(A_d,A_d'Events_ce,A_d'Events_cd,L_G,L_H) THEN
   PRINT("K is conditionally coobservable _.") ELSE
   PRINT("K is not conditionally coobservable _.") END;

 VerifyGCCoobservability__ =
  SELECT GCCoobservability__(A_d,A_d'Events_ce,A_d'Events_cd,L_G,L_H) THEN
   PRINT("K is conditionally coobservable __.") ELSE
   PRINT("K is not conditionally coobservable __.") END;

 VerifyGCCPCoobservability_ =
  SELECT GCCPCoobservability_(A_d,A_d'Events_ce,L_G,L_H) THEN
   PRINT("K is conditionally C&P coobservable _.") ELSE
   PRINT("K is not conditionally C&P coobservable -.") END;

 VerifyGCCPCoobservability =
  SELECT GCCPCoobservability(A_d,A_d'Events_ce,L_G,L_H) THEN
   PRINT("K is conditionally C&P coobservable.")  ELSE
   PRINT("K is not conditionally C&P coobservable.") END;

 VerifyGCDACoobservability_ =
  SELECT GCDACoobservability_(A_d,A_d'Events_cd,L_G,L_H) THEN
   PRINT("K is conditionally D&A coobservable _.") ELSE
   PRINT("K is not conditionally D&A coobservable _.") END;

 VerifyGCDACoobservability =
  SELECT GCDACoobservability(A_d,A_d'Events_cd,L_G,L_H) THEN
   PRINT("K is conditionally D&A coobservable.") ELSE
   PRINT("K is not conditionally D&A coobservable.") END;

 VerifyGCCoobservability =
  PRE GCCoobservability(A_d,A_d'Events_ce,A_d'Events_cd,L_G,L_H) THEN
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
      PRINT("The control is conflicting.") END END END