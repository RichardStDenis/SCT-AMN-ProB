// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL test_section7_1_DEDS_12_3_H1_H2_Figs16b

SEES
 //---------------------------------------------------------------------
 // Machine for input data
 //---------------------------------------------------------------------
 // L(H1) is D&A coobservable and L(H2) is D&A coobservable but
 // L(H1) /\ L(H2) is not D&A coobservable.
 DEDS_12_3_H1_H2_Fig16 /*@file "../DataExamples/DEDS_12_3_H1_H2_Fig16.mch" */

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 1000000;
 SET_PREF_SMT == FALSE;
 SET_PREF_KODKOD == FALSE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../Theories/SCT_core.def";
 "../Theories/SCT_decentralized.def";
 "LibraryIO.def"

CONSTANTS
 L_G, L_H1, L_H2   // Languages for free and legal behaviors

PROPERTIES
 G_fb : Generator_C_O(G_fb'states,G_fb'events) &
 H1_lb : FSA(H1_lb'states,H1_lb'events) &
 H2_lb : FSA(H2_lb'states,H2_lb'events) &
 L_G = GetStrings(G_fb, bound) &
 L_H1 = GetStrings(H1_lb,bound) & L_H2 = GetStrings(H2_lb,bound)

OPERATIONS
 VerifyDACoobservability_H1 =
  SELECT DACoobservability_(A_d,L_G,L_H1) THEN
   PRINT("L(H1) is D&A coobservable _.") ELSE
   PRINT("L(H1) is not D&A coobservable_ .") END;

 VerifyDACoobservabilityH1 =
  SELECT DACoobservability(A_d,L_G,L_H1) THEN
   PRINT("L(H1) is D&A coobservable.") ELSE
   PRINT("L(H1) is not D&A coobservable.") END;

 VerifyDACoobservability_H2 =
  SELECT DACoobservability_(A_d,L_G,L_H2) THEN
   PRINT("L(H2) is D&A coobservable _.") ELSE
   PRINT("L(H2) is not D&A coobservable _.") END;

 VerifyDACoobservabilityH2 =
  SELECT DACoobservability(A_d,L_G,L_H2) THEN
   PRINT("L(H2) is D&A coobservable.") ELSE
   PRINT("L(H2) is not D&A coobservable.") END;

 VerifyDACoobservability_H1H2 =
  SELECT DACoobservability_(A_d,L_G,L_H1 /\ L_H2) THEN
   PRINT("L(H1) /\\ L(H2) is D&A coobservable _.") ELSE
   PRINT("L(H1) /\\ L(H2) is not D&A coobservable _.") END;

 VerifyDACoobservabilityH1H2 =
  SELECT DACoobservability(A_d,L_G,L_H1 /\ L_H2) THEN
   PRINT("L(H1) /\\ L(H2) is D&A coobservable.") ELSE
   PRINT("L(H1) /\\ L(H2) is not D&A coobservable.") END END