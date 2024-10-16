// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL tab7_GA_IEEE_49_11_Fig12_52_10_Fig1

SEES
 //---------------------------------------------------------------------
 // Machine for input data
 //---------------------------------------------------------------------
 // not C&P coobservable and not D&A coobservable w.r.t. the general architecture,
 // thus not coobservable w.r.t. the general architecture.
   IEEE_49_11_H_Fig12 /*@file "../../DataExamples/IEEE_49_11_H_Fig12.mch" */
 //IEEE_52_10_Ex1_H_Fig1 /*@file "../../DataExamples/IEEE_52_10_Ex1_H_Fig1.mch" */

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
 "../../Theories/SCT_core.def";
 "../../Theories/SCT_decentralized.def";
 "LibraryIO.def"

CONSTANTS
 L_G, L_H    // Languages for free and legal behaviors

PROPERTIES
 G_fb : Generator_C_O(G_fb'states,G_fb'events) &
 H_lb : FSA(H_lb'states,H_lb'events) &
 L_G = GetStrings(G_fb, bound) &
 L_H = GetStrings(H_lb, bound)

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
OUTPUT (GCPCoobservability, IEEE_49_11_H_Fig12.mch):
% Runtime for SOLUTION for SETUP_CONSTANTS: 150 ms (walltime: 160 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2060 ms
"K is not C&P coobservable."
Infinite loop reached after 4 steps (looping on VerifyGCPCoobservability).
% Runtime: 340 ms (total: 340 ms, walltime: 350 ms)
% Finished execute (errors=0, warnings=0)
% Finished processing file after 400 ms walltime (390 ms runtime), since start: 2240 ms

% Finished process_file (errors=0, warnings=0)
% ProB Finished

----------------------------------------

OUTPUT (GDACoobservability, IEEE_49_11_H_Fig12.mch):
% Runtime for SOLUTION for SETUP_CONSTANTS: 160 ms (walltime: 160 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2080 ms
"K is not D&A coobservable."
Infinite loop reached after 4 steps (looping on VerifyGDACoobservability).
% Runtime: 280 ms (total: 280 ms, walltime: 300 ms)
% Finished execute (errors=0, warnings=0)
% Finished processing file after 350 ms walltime (340 ms runtime), since start: 2210 ms

% Finished process_file (errors=0, warnings=0)
% ProB Finished

----------------------------------------

OUTPUT (GCoobservability, IEEE_49_11_H_Fig12.mch):
% Runtime for SOLUTION for SETUP_CONSTANTS: 160 ms (walltime: 160 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2050 ms
"K is not coobservable."
Infinite loop reached after 4 steps (looping on VerifyCoobservability).
% Runtime: 350 ms (total: 350 ms, walltime: 360 ms)
% Finished execute (errors=0, warnings=0)
% Finished processing file after 410 ms walltime (400 ms runtime), since start: 2230 ms

% Finished process_file (errors=0, warnings=0)
% ProB Finished


----------------------------------------


OUTPUT (GCPCoobservability, IEEE_52_10_Ex1_H_Fig1.mch):
% Runtime for SOLUTION for SETUP_CONSTANTS: 160 ms (walltime: 160 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2020 ms
"K is not C&P coobservable."
Infinite loop reached after 4 steps (looping on VerifyGCPCoobservability).
% Runtime: 360 ms (total: 360 ms, walltime: 380 ms)
% Finished execute (errors=0, warnings=0)
% Finished processing file after 430 ms walltime (420 ms runtime), since start: 2230 ms

% Finished process_file (errors=0, warnings=0)
% ProB Finished

----------------------------------------

OUTPUT (GDACoobservability, IEEE_52_10_Ex1_H_Fig1.mch):
% Runtime for SOLUTION for SETUP_CONSTANTS: 160 ms (walltime: 170 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2050 ms
"K is D&A coobservable."   *** because Ecd is empty
Infinite loop reached after 4 steps (looping on VerifyGDACoobservability).
% Runtime: 180 ms (total: 180 ms, walltime: 180 ms)
% Finished execute (errors=0, warnings=0)
% Finished processing file after 230 ms walltime (240 ms runtime), since start: 2060 ms

% Finished process_file (errors=0, warnings=0)
% ProB Finished


----------------------------------------

OUTPUT (GCoobservability, IEEE_52_10_Ex1_H_Fig1.mch):
% Runtime for SOLUTION for SETUP_CONSTANTS: 160 ms (walltime: 160 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2100 ms
"K is not coobservable."
Infinite loop reached after 4 steps (looping on VerifyCoobservability).
% Runtime: 380 ms (total: 380 ms, walltime: 380 ms)
% Finished execute (errors=0, warnings=0)
% Finished processing file after 460 ms walltime (430 ms runtime), since start: 2310 ms

% Finished process_file (errors=0, warnings=0)
% ProB Finished

*/