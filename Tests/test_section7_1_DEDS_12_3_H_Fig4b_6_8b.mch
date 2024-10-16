// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL test_section7_1_DEDS_12_3_H_Figs4b_6_8b

SEES
 //---------------------------------------------------------------------
 // Machine for input data
 //---------------------------------------------------------------------
 // Not C&P coobservable and D&A coobservable.
   DEDS_12_3_H_Fig4b /*@file "../DataExamples/DEDS_12_3_H_Fig4b.mch" */
 // C&P coobservable and not D&A coobservable.
 //DEDS_12_3_H_Fig6 /*@file "../DataExamples/DEDS_12_3_H_Fig6.mch" */
 // Not C&P coobservable and not D&A coobservable.
 //DEDS_12_3_H_Fig8b /*@file "../DataExamples/DEDS_12_3_H_Fig8b.mch" */

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
 L_G, L_H          // Languages for free and legal behaviors

PROPERTIES
 G_fb : Generator_C_O(G_fb'states,G_fb'events) &
 H_lb : FSA(H_lb'states,H_lb'events) &
 L_G = GetStrings(G_fb,bound) &
 L_H = GetStrings(H_lb,bound)

VARIABLES
 Sa_P_i, Sp_P_i,   // Antipermissive and permissive local decisions
 Sa_G, Sp_G        // Global decisions

INITIALISATION
 Sa_P_i := {}; Sp_P_i := {};
 Sa_G := {}; Sp_G := {}

INVARIANT
 Sa_P_i <: def_Sa_P_i(seq(G_fb'events),G_fb'events) &
 Sp_P_i <: def_Sp_P_i(seq(G_fb'events),G_fb'events) &
 Sa_G <: def_Sa_G(L_H,G_fb'events) &
 Sp_G <: def_Sp_G(L_H,G_fb'events)

OPERATIONS
 VerifyCPCoobservability_ =
  SELECT CPCoobservability_(A_d,L_G,L_H) THEN
   PRINT("K is C&P coobservable _.") ELSE
   PRINT("K is not C&P coobservable _.") END;

 VerifyCPCoobservability =
  SELECT CPCoobservability(A_d,L_G,L_H) THEN
   PRINT("K is C&P coobservable.");
   Sp_P_i := LocalDecisionsCP(A_d,G_fb'events-G_fb'ctrlEvents,L_H);
   PRINTF("Permissive local decisions:~n ~w~n",[Sp_P_i]);
   Sp_G := GlobalDecisionsCP(A_d,G_fb'events,L_H,Sp_P_i);
   PRINTF("Permissive global decisions:~n ~w~n",[Sp_G]) ELSE
   PRINT("K is not C&P coobservable.") END;

 VerifyDACoobservability_ =
  SELECT DACoobservability_(A_d,L_G,L_H) THEN
   PRINT("K is D&A coobservable. _") ELSE
   PRINT("K is not D&A coobservable. _") END;

 VerifyDACoobservability =
  SELECT DACoobservability(A_d,L_G,L_H) THEN
   PRINT("K is D&A coobservable.");
   Sa_P_i := LocalDecisionsDA(A_d,G_fb'events-G_fb'ctrlEvents,L_G,L_H);
   PRINTF("Antipermissive local decisions:~n ~w~n",[Sa_P_i] );
   Sa_G := GlobalDecisionsDA(A_d,G_fb'events,L_H,Sa_P_i);
   PRINTF("Antipermissive global decisions:~n ~w~n",[Sa_G]) ELSE
    PRINT("K is not D&A coobservable.") END;

 cntrexCPCoobservability =
  PRE CPCoobservability(A_d,L_G,L_H) THEN skip END;

 cntrexDACoobservability =
  PRE DACoobservability(A_d,L_G,L_H) THEN skip END END