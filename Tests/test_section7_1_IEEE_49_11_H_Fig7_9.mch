// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL test_section7_1_IEEE_49_11_H_Fig7_9

SEES
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------
 // Not conditionally C&P coobservable and conditionally D&A coobservable.
 IEEE_49_11_H_Fig7 /*@file "../DataExamples/IEEE_49_11_H_Fig7.mch" */
 // Conditionally C&P coobservable and not conditionally D&A coobservable.
 //IEEE_49_11_H_Fig9 /*@file "../DataExamples/IEEE_49_11_H_Fig9.mch" */

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 1000000;
 SET_PREF_SMT == TRUE;   // to avoid a warning with IEEE_49_11_Th2_H_Fig
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
 S_d_i, S_dc_i, S_e_i, S_ec_i     // Local decisions

INITIALISATION
 S_d_i := {};  S_dc_i := {};  S_e_i := {};  S_ec_i := {}

INVARIANT
 S_d_i <: def_Sl_d_dc_e_ec(L_G,L_H,G_fb'events) &
 S_dc_i <: def_Sl_d_dc_e_ec(L_G,L_H,G_fb'events) &
 S_e_i <: def_Sl_d_dc_e_ec(L_G,L_H,G_fb'events) &
 S_ec_i <: def_Sl_d_dc_e_ec(L_G,L_H,G_fb'events)

OPERATIONS
 VerifyGCCPCoobservability_ =
  SELECT GCCPCoobservability_(A_d,A_d'CtrlEvents,L_G,L_H) THEN
   PRINT("K is conditionally C&P coobservable_ .") ELSE
   PRINT("K is not conditionally C&P coobservable _.") END;

 VerifyGCCPCoobservability =
  SELECT GCCPCoobservability(A_d,A_d'CtrlEvents,L_G,L_H) THEN
   PRINT("K is conditionally C&P coobservable.");
   S_e_i :=  LocalDecisionsEnbl(A_d,L_G,L_H); 
   PRINTF("Local decisions of the supervisors (enable):~n ~w~n",[S_e_i]);
   S_ec_i := LocalDecisionsEnbl_c(A_d,L_G,L_H);
   PRINTF("Local decisions of the supervisor (enable if nobody disable) :~n ~w~n",[S_ec_i]);
   S_d_i := LocalDecisionsDsbl(A_d,L_G,L_H); 
   PRINTF("Local decisions of the supervisors (disable):~n ~w~n",[S_d_i]);
   S_dc_i := LocalDecisionsDsbl_c(A_d,L_G,L_H);
   PRINTF("Local decisions of the supervisors (disable if nobody enable):~n ~w~n",[S_dc_i]) ELSE
    PRINT("K is not conditionally C&P coobservable.") END;

 VerifyGCDACoobservability_ =
  SELECT GCDACoobservability_(A_d,A_d'CtrlEvents,L_G,L_H) THEN
   PRINT("K is D&A conditionally coobservable _.") ELSE
   PRINT("K is not conditionally D&A coobservable _.") END;

 VerifyGCDACoobservability =
  SELECT GCDACoobservability(A_d,A_d'CtrlEvents,L_G,L_H) THEN
   PRINT("K is D&A conditionally coobservable.") ELSE
   PRINT("K is not conditionally D&A coobservable.") END;

 cntrexGCCPCoobservability =
  PRE GCCPCoobservability_(A_d,A_d'CtrlEvents,L_G,L_H) THEN skip END;

 cntrexGCDACoobservability =
  PRE GCDACoobservability_(A_d,A_d'CtrlEvents,L_G,L_H) THEN skip END END