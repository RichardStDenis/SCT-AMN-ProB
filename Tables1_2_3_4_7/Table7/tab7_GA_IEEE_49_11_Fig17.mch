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
 SET_PREF_SMT == FALSE;
 SET_PREF_KODKOD == FALSE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../../Theories/SCT_core.def";
 "../../Theories/SCT_decentralized.def";
 "LibraryIO.def"

CONSTANTS
 Tr_G, L_G, Tr_H, L_H     // Languages for free and legal behaviors

PROPERTIES
 G_fb : Generator_C_O(G_fb'states,G_fb'events) &
 H_lb : FSA(H_lb'states,H_lb'events) &
 Tr_G = GetTracesNc1stNqcSev(G_fb,bound,{pass,stop}) &
 L_G = Traces2Strings(Tr_G) &
 Tr_H = GetTracesNc1stNqcSev(H_lb,bound,{pass,stop}) &
 L_H = Traces2Strings(Tr_H)

OPERATIONS
 VerifyCPCoobservability =
  SELECT CPCoobservability(A_d,L_G,L_H) THEN
   PRINT("K is C&P coobservable.") ELSE
   PRINT("K is not C&P coobservable.");
    PRINTF("E_c: ~w~n",[A_d'CtrlEvents]) END
/*
 VerifyDACoobservability =
  SELECT DACoobservability(A_d,L_G,L_H) THEN
   PRINT("K is D&A coobservable.") ELSE
   PRINT("K is not D&A coobservable.");
    PRINTF("E_c: ~w~n",[A_d'CtrlEvents]) END
  
 VerifyCoobservability =
  SELECT Coobservability(A_d,A_d'Events_ce,A_d'Events_cd,L_G,L_H) THEN
   PRINT("K is coobservable.") ELSE
    PRINT("K is not coobservable.");
    PRINTF("E_ce: ~w~n",[A_d'Events_ce]);
    PRINTF("E_cd: ~w~n",[A_d'Events_cd]) END
*/
END

/*
OUTPUT (GCPCoobservability):
% Runtime for SOLUTION for SETUP_CONSTANTS: 200 ms (walltime: 200 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2110 ms
"K is not C&P coobservable."
E_c: {stop,pass}
Infinite loop reached after 4 steps (looping on VerifyGCPCoobservability).
% Runtime: 340 ms (total: 360 ms, walltime: 380 ms)
% Finished execute (errors=0, warnings=0)
% Finished processing file after 430 ms walltime (400 ms runtime), since start: 2270 ms

% Finished process_file (errors=0, warnings=0)
% ProB Finished

-------------

On MacBook Pro with last version of ProB 20.2.2024:
% Finished processing file after 304 ms walltime (299 ms runtime), since start: 2300 ms


----------------------------------------

OUTPUT (GDACoobservability):
% Runtime for SOLUTION for SETUP_CONSTANTS: 200 ms (walltime: 200 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2120 ms
"K is not D&A coobservable."
E_c: {stop,pass}
Infinite loop reached after 4 steps (looping on VerifyGDACoobservability).
% Runtime: 290 ms (total: 290 ms, walltime: 300 ms)
% Finished execute (errors=0, warnings=0)
% Finished processing file after 360 ms walltime (350 ms runtime), since start: 2210 ms

% Finished process_file (errors=0, warnings=0)
% ProB Finished

-------------

On MacBook Pro with last version of ProB 20.2.2024:
% Finished processing file after 303 ms walltime (298 ms runtime), since start: 2314 ms


----------------------------------------

OUTPUT (GCoobservability):
% Runtime for SOLUTION for SETUP_CONSTANTS: 200 ms (walltime: 200 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2120 ms
"K is not coobservable."
E_ce: {pass}
E_cd: {stop}
Infinite loop reached after 4 steps (looping on VerifyCoobservability).
% Runtime: 350 ms (total: 350 ms, walltime: 360 ms)
% Finished execute (errors=0, warnings=0)
% Finished processing file after 430 ms walltime (400 ms runtime), since start: 2270 ms

% Finished process_file (errors=0, warnings=0)
% ProB Finished

-------------

On MacBook Pro with last version of ProB 20.2.2024:
% Finished processing file after 311 ms walltime (305 ms runtime), since start: 2322 ms
*/