// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL test_section7_1_DEDS_12_3_H_Figs8b_11b

SEES
 //---------------------------------------------------------------------
 // Machine for input data
 //---------------------------------------------------------------------
 // Observable,
 // C&P coobservable and D&A coobservable w.r.t. the general architecture,
 // thus coobservable w.r.t. the general architecture.
   DEDS_12_3_H_Fig8b /*@file "../DataExamples/DEDS_12_3_H_Fig8b.mch" */
 // Observable,
 // not C&P coobservable and D&A coobservable w.r.t. the general architecture,
 // thus not coobservable w.r.t. the general architecture;
 // or
 // observable,
 // C&P coobservable and not D&A coobservable w.r.t. the general architecture,
 // thus not coobservable w.r.t. the general architecture.
 //DEDS_12_3_H_Fig11b /*@file "../DataExamples/DEDS_12_3_H_Fig11b.mch" */
 // C&P coobservable and not D&A coobservable w.r.t. the general architecture,
 // thus not coobservable w.r.t. the general architecture.
 //IEEE_49_11_H_Fig12 /*@file "../DataExamples/IEEE_49_11_H_Fig12.mch" */

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
 L_G, L_H     // Languages for free and legal behaviors

PROPERTIES
 G_fb : Generator_C_O(G_fb'states,G_fb'events) &
 H_lb : FSA(H_lb'states,H_lb'events) &
 L_G = GetStrings(G_fb, bound) &
 L_H = GetStrings(H_lb, bound)

VARIABLES
 S_P_i,       // Local decisions
 S_G          // Global decision

INITIALISATION
 S_P_i := {};
 S_G := {}

INVARIANT
 S_P_i <: def_S_P_i(L_H,G_fb'events) &
 S_G <: def_S_G(L_H,G_fb'events)

OPERATIONS
 VerifyObservability =
  SELECT Observability(L_H,L_G,G_fb'obsEvents,G_fb'events) THEN
   PRINT("K is observable.") ELSE
   PRINT("K is not observable.") END;

 VerifyGCPCoobservability_ =
  SELECT GCPCoobservability_(A_d,A_d'Events_ce,L_G,L_H) THEN
   PRINT("K is C&P coobservable _.") ELSE
   PRINT("K is not C&P coobservable -.") END;

 VerifyGCPCoobservability =
  SELECT GCPCoobservability(A_d,A_d'Events_ce,L_G,L_H) THEN
   PRINT("K is C&P coobservable.") ELSE
   PRINT("K is not C&P coobservable.") END;

 VerifyGDACoobservability_ =
  SELECT GDACoobservability_(A_d,A_d'Events_cd,L_G,L_H) THEN
   PRINT("K is D&A coobservable _.") ELSE
   PRINT("K is not D&A coobservable _.") END;

 VerifyGDACoobservability =
  SELECT GDACoobservability(A_d,A_d'Events_cd,L_G,L_H) THEN
   PRINT("K is D&A coobservable.") ELSE
   PRINT("K is not D&A coobservable.") END;

 VerifyCoobservability =
  SELECT Coobservability(A_d,A_d'Events_ce,A_d'Events_cd,L_G,L_H) THEN
   PRINT("K is coobservable.");
   S_P_i  := LocalDecisionsG(A_d,G_fb'events-G_fb'ctrlEvents,L_G,L_H);
   PRINTF("Local decisions for all supervisors: ~n ~w~n",[S_P_i]);
   S_G :=GlobalDecisionsG(A_d,G_fb'events-G_fb'ctrlEvents,L_H,S_P_i); 
   PRINTF("Global decisions: ~n ~w~n",[S_G]) ELSE
    PRINT("K is not coobservable.") END;

 cntrexGCPCoobservability =
  PRE GCPCoobservability(A_d,A_d'Events_ce,L_G,L_H) THEN skip END;

 cntrexGDACoobservability =
  PRE GDACoobservability(A_d,A_d'Events_cd,L_G,L_H) THEN skip END END