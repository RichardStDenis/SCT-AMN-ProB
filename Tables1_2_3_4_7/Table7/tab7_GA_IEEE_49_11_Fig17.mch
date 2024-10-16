// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL tab7_GA_IEEE_49_11_H_Fig17

SEES
 //---------------------------------------------------------------------
 // Machine for input data
 //---------------------------------------------------------------------
 // C&P coobservable and conditionally D&A coobservable
 // w.r.t. the general architecture thus conditionally coobservable
 // w.r.t. the general architecture (Sigma_ce = {p} and Sigma_cd = {s}).
 IEEE_49_11_H_Fig17 /*@file "../../DataExamples/IEEE_49_11_H_Fig17.mch" */

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
 "../../Theories/SCT_core.def";
 "../../Theories/SCT_decentralized.def";
 "LibraryIO.def"

CONSTANTS
 T_G, L_G, T_H, L_H     // Languages for free and legal behaviors

PROPERTIES
 G_fb : Generator_C_O(G_fb'states,G_fb'events) &
 H_lb : FSA(H_lb'states,H_lb'events) &
 T_G = GetTracesNc1stNqcSev(G_fb,bound,{p,s}) &
 L_G = Traces2Strings(T_G) &
 T_H = GetTracesNc1stNqcSev(H_lb,bound,{p,s}) &
 L_H = Traces2Strings(T_H)

OPERATIONS
 VerifyGCPCoobservability =
  SELECT GCPCoobservability(A_d,A_d'Events_ce,L_G,L_H) THEN
   PRINT("K is C&P coobservable.") ELSE
   PRINT("K is not C&P coobservable.") END
/*
 VerifyGDACoobservability =
  SELECT GDACoobservability(A_d,A_d'Events_cd,L_G,L_H) THEN
   PRINT("K is D&A coobservable.") ELSE
   PRINT("K is not D&A coobservable.") END

 VerifyCoobservability =
  SELECT Coobservability(A_d,A_d'Events_ce,A_d'Events_cd,L_G,L_H) THEN
   PRINT("K is coobservable.") ELSE
    PRINT("K is not coobservable.") END
*/
END

/*
OUTPUT (GCPCoobservability):
% Runtime for SOLUTION for SETUP_CONSTANTS: 200 ms (walltime: 200 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2110 ms
"K is not C&P coobservable."
Infinite loop reached after 4 steps (looping on VerifyGCPCoobservability).
% Runtime: 340 ms (total: 360 ms, walltime: 380 ms)
% Finished execute (errors=0, warnings=0)
% Finished processing file after 430 ms walltime (400 ms runtime), since start: 2270 ms

% Finished process_file (errors=0, warnings=0)
% ProB Finished


----------------------------------------

OUTPUT (GDACoobservability):
% Runtime for SOLUTION for SETUP_CONSTANTS: 200 ms (walltime: 200 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2120 ms
"K is not D&A coobservable."
Infinite loop reached after 4 steps (looping on VerifyGDACoobservability).
% Runtime: 290 ms (total: 290 ms, walltime: 300 ms)
% Finished execute (errors=0, warnings=0)
% Finished processing file after 360 ms walltime (350 ms runtime), since start: 2210 ms

% Finished process_file (errors=0, warnings=0)
% ProB Finished


----------------------------------------

OUTPUT (GCoobservability):
% Runtime for SOLUTION for SETUP_CONSTANTS: 200 ms (walltime: 200 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2120 ms
"K is not coobservable."
Infinite loop reached after 4 steps (looping on VerifyCoobservability).
% Runtime: 350 ms (total: 350 ms, walltime: 360 ms)
% Finished execute (errors=0, warnings=0)
% Finished processing file after 430 ms walltime (400 ms runtime), since start: 2270 ms

% Finished process_file (errors=0, warnings=0)
% ProB Finished
*/