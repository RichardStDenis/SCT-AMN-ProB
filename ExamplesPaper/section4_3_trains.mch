// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL section4_3_trains
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------
USES
 LBtrains_LB_1_6_2 /*@file "../Trains/LBtrains_LB_1_6_2.mch" */

CONSTANTS
 output_file_name,      // output file name
 T_G_FB,                // Traces of the free behavior
 L_G_FB,                // Prefix-closed and marked languages of the free behavior
 Lm_G_FB,
 T_G_LB,                // Traces of the legale behavior
 L_G_LB,                // Prefix-closed and marked languages of the legal behavior
 Lm_G_LB

ABSTRACT_CONSTANTS GetTracesFB, GetTracesLB

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 5000000;
 SET_PREF_SMT == FALSE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../Theories/SCT_core.def";
 "LibraryIO.def";

PROPERTIES
 G_FB : Generator_C(G_FB'states,G_FB'events) &
 G_LB : FSA(G_LB'states,G_LB'events) &

 output_file_name = "~/Documents/SCTBenchmark/output_trains.txt" &

// This test offers two possibilities since Xm = {x0}:
//  - one with strings instead of task;
//  - one with tasks.
// Comments should be removed and set accordingly.


 GetTracesFB = lambdaGetTraces(G_FB,bound_G_FB,GetTracesFB) &
 T_G_FB = GetTracesFB(baseCaseTr(G_FB),1) &
 L_G_FB = Traces2Strings(T_G_FB) &
 Lm_G_FB = Traces2Tasks(T_G_FB,G_FB'Xm) &

 GetTracesLB = lambdaGetTraces(G_LB,bound_G_LB,GetTracesLB) &
 T_G_LB = GetTracesLB(baseCaseTr(G_LB),1) &
//L_G_LB = Traces2Strings(T_G_LB) &     // select this line or the next one
 L_G_LB = pr(Lm_G_LB) &
 Lm_G_LB = Traces2Tasks(T_G_LB,G_LB'Xm)

VARIABLES
 code,
 i, K_i, K_succ_i
INITIALISATION
  code := 0;
  i := 1;
//K_i := L_G_LB;                       // select this line or the next one
  K_i := supR(Lm_G_LB,Lm_G_FB);
//K_succ_i := iStepSupC_PC(K_i,L_G_FB,G_FB'events-G_FB'ctrlEvents)   // select this line or the next one
  K_succ_i := iStepSupC(K_i,pr(K_i),L_G_FB,G_FB'events-G_FB'ctrlEvents)
INVARIANT
 code >= 0 & code <= 3 &
 i >= 1 &
 K_i <: Lm_G_LB & K_succ_i <: Lm_G_LB

OPERATIONS
 DisplayStatistics = PRE code = 0 THEN
  FPRINTF(output_file_name, "~nRailway with ~w sections and ~w trains: ", [nbr_sections, nbr_trains]);
  FPRINTF(output_file_name, "~w states and ", [card(G_FB'states)]);
  FPRINTF(output_file_name, "~w transitions~n~n", [card(G_FB'tf)]);
  FPRINTF(output_file_name, "Number of traces (free behavior): ~w~n", [card(T_G_FB)]);
  FPRINTF(output_file_name, "Number of words (free behavior): ~w~n", [card(L_G_FB)]);
  FPRINTF(output_file_name, "Number of tasks (free behavior): ~w~n", [card(Lm_G_FB)]);
  FPRINTF(output_file_name, "Number of traces (legal behavior): ~w~n", [card(T_G_LB)]);
  FPRINTF(output_file_name, "Number of words (legal behavior): ~w~n", [card(L_G_LB)]);
  FPRINTF(output_file_name, "Number of tasks (legal behavior): ~w~n", [card(Lm_G_LB)]);
  code := 1 END;

GetSupRC = PRE code = 1 & Lm_G_LB <: Lm_G_FB THEN
//PRINT(K_i);
  WHILE K_i /= K_succ_i DO
// PRINT(K_succ_i);
   i := i+1;
   K_i := K_succ_i;
// K_succ_i := iStepSupC_PC(K_i,L_G_FB,G_FB'events-G_FB'ctrlEvents)  // select this line or the next one
   K_succ_i := iStepSupC(K_i,pr(K_i),L_G_FB,G_FB'events-G_FB'ctrlEvents)
  INVARIANT card(K_i) >= card(K_succ_i)
  VARIANT card(K_i)
  END;
  FPRINTF(output_file_name, "Number of iterations: ~w~n", [i]);
  FPRINTF(output_file_name, "|supRC(K)|: ~w~n", [card(K_i)]);
  FPRINTF(output_file_name, "|pr(supC(K))|: ~w~n", [card(pr(K_i))]);
  FPRINTF(output_file_name, "supRC_(K): ~w~n", [K_i]);
  code := 2
 END;

 GetSupervisors = PRE code = 2 THEN
  ANY phi WHERE
   phi : SRsupervisor___(G_FB'states,G_FB'ctrlEvents) &
// phi = SRsupervisorDISBLlabel_(T_G_FB,K_i,L_G_FB,G_FB'ctrlEvents,bound_G_FB) THEN // select this line or the next one
   phi = SRsupervisorDISBLlabel_(T_G_FB,pr(K_i),L_G_FB,G_FB'ctrlEvents,bound_G_FB) THEN
    FPRINTF(output_file_name,"The state realization of the supervisor (disabled events): ~w~n",[phi]) END;
  ANY phi WHERE
   phi : SRsupervisor___(G_FB'states,G_FB'events) &
// phi = SRsupervisorENBLlabel_(T_G_LB,K_i,G_FB'events,bound_G_FB) THEN             // select this line or the next one
   phi = SRsupervisorENBLlabel_(T_G_LB,pr(K_i),G_FB'events,bound_G_FB) THEN
    FPRINTF(output_file_name,"The state realization of the supervisor (enabled events): ~w~n",[phi]) END;
  code := 3
 END END

/*
OUTPUT:
Railway with 6 sections and 2 trains: 36 states and 72 transitions

Number of traces (free behavior): 8191
Number of words (free behavior): 8191
Number of tasks (free behavior): 929
Number of traces (legal behavior): 1821
Number of words (legal behavior): 1336
Number of tasks (legal behavior): 366
Number of iterations: 2
|supRC(K)|: 100
|pr(supC(K))|: 386
supRC_(K):
{{},[t2_4t5,t2_5t6,t1_2t3,t2_6t1,t2_1t2,t1_3t4,t1_4t5,t2_2t3,t1_5t6,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t2_1t2,t1_3t4,t1_4t5,t2_2t3,t1_5t6,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t2_1t2,t1_3t4,t1_4t5,t2_2t3,t1_5t6,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t2_1t2,t1_3t4,t1_4t5,t1_5t6,t2_2t3,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t2_1t2,t1_3t4,t1_4t5,t1_5t6,t2_2t3,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t2_1t2,t1_3t4,t1_4t5,t1_5t6,t2_2t3,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t2_1t2,t1_3t4,t1_4t5,t2_2t3,t2_3t4,t1_5t6,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t2_1t2,t1_3t4,t1_4t5,t2_2t3,t1_5t6,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t2_1t2,t1_3t4,t1_4t5,t2_2t3,t1_5t6,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t2_1t2,t1_3t4,t1_4t5,t2_2t3,t1_5t6,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t2_1t2,t1_3t4,t1_4t5,t1_5t6,t2_2t3,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t2_1t2,t1_3t4,t1_4t5,t1_5t6,t2_2t3,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t2_1t2,t1_3t4,t1_4t5,t1_5t6,t2_2t3,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t2_1t2,t1_3t4,t1_4t5,t2_2t3,t2_3t4,t1_5t6,t1_6t1,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t1_3t4,t2_6t1,t1_4t5,t2_1t2,t2_2t3,t1_5t6,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t2_5t6,t1_2t3,t1_3t4,t2_6t1,t1_4t5,t2_1t2,t2_2t3,t1_5t6,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t1_3t4,t2_6t1,t1_4t5,t2_1t2,t2_2t3,t1_5t6,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t1_3t4,t2_6t1,t1_4t5,t1_5t6,t2_1t2,t2_2t3,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t2_5t6,t1_2t3,t1_3t4,t2_6t1,t1_4t5,t1_5t6,t2_1t2,t2_2t3,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t1_3t4,t2_6t1,t1_4t5,t1_5t6,t2_1t2,t2_2t3,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t1_3t4,t2_6t1,t1_4t5,t2_1t2,t1_5t6,t2_2t3,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t2_5t6,t1_2t3,t1_3t4,t2_6t1,t1_4t5,t2_1t2,t1_5t6,t2_2t3,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t1_3t4,t2_6t1,t1_4t5,t2_1t2,t1_5t6,t2_2t3,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t1_3t4,t2_6t1,t1_4t5,t2_1t2,t2_2t3,t2_3t4,t1_5t6,t1_6t1,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t1_3t4,t1_4t5,t2_1t2,t2_2t3,t1_5t6,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t1_3t4,t1_4t5,t2_1t2,t2_2t3,t1_5t6,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t1_3t4,t1_4t5,t2_1t2,t2_2t3,t1_5t6,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t1_3t4,t1_4t5,t1_5t6,t2_1t2,t2_2t3,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t1_3t4,t1_4t5,t1_5t6,t2_1t2,t2_2t3,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t1_3t4,t1_4t5,t1_5t6,t2_1t2,t2_2t3,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t1_3t4,t1_4t5,t2_1t2,t1_5t6,t2_2t3,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t1_3t4,t1_4t5,t2_1t2,t1_5t6,t2_2t3,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t1_3t4,t1_4t5,t2_1t2,t1_5t6,t2_2t3,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t1_3t4,t1_4t5,t2_1t2,t2_2t3,t2_3t4,t1_5t6,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t1_3t4,t2_5t6,t2_6t1,t1_4t5,t2_1t2,t2_2t3,t1_5t6,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t1_2t3,t1_3t4,t2_5t6,t2_6t1,t1_4t5,t2_1t2,t2_2t3,t1_5t6,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t1_3t4,t2_5t6,t2_6t1,t1_4t5,t2_1t2,t2_2t3,t1_5t6,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t1_2t3,t1_3t4,t2_5t6,t2_6t1,t1_4t5,t1_5t6,t2_1t2,t2_2t3,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t1_2t3,t1_3t4,t2_5t6,t2_6t1,t1_4t5,t1_5t6,t2_1t2,t2_2t3,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t1_3t4,t2_5t6,t2_6t1,t1_4t5,t1_5t6,t2_1t2,t2_2t3,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t1_2t3,t1_3t4,t2_5t6,t2_6t1,t1_4t5,t2_1t2,t1_5t6,t2_2t3,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t1_2t3,t1_3t4,t2_5t6,t2_6t1,t1_4t5,t2_1t2,t1_5t6,t2_2t3,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t1_3t4,t2_5t6,t2_6t1,t1_4t5,t2_1t2,t1_5t6,t2_2t3,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t1_2t3,t1_3t4,t2_5t6,t2_6t1,t1_4t5,t2_1t2,t2_2t3,t2_3t4,t1_5t6,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t1_3t4,t2_6t1,t1_4t5,t2_1t2,t2_2t3,t1_5t6,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t1_2t3,t2_5t6,t1_3t4,t2_6t1,t1_4t5,t2_1t2,t2_2t3,t1_5t6,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t1_3t4,t2_6t1,t1_4t5,t2_1t2,t2_2t3,t1_5t6,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t1_3t4,t2_6t1,t1_4t5,t1_5t6,t2_1t2,t2_2t3,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t1_2t3,t2_5t6,t1_3t4,t2_6t1,t1_4t5,t1_5t6,t2_1t2,t2_2t3,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t1_3t4,t2_6t1,t1_4t5,t1_5t6,t2_1t2,t2_2t3,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t1_3t4,t2_6t1,t1_4t5,t2_1t2,t1_5t6,t2_2t3,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t1_2t3,t2_5t6,t1_3t4,t2_6t1,t1_4t5,t2_1t2,t1_5t6,t2_2t3,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t1_3t4,t2_6t1,t1_4t5,t2_1t2,t1_5t6,t2_2t3,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t1_3t4,t2_6t1,t1_4t5,t2_1t2,t2_2t3,t2_3t4,t1_5t6,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t1_3t4,t1_4t5,t2_1t2,t2_2t3,t1_5t6,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t1_3t4,t1_4t5,t2_1t2,t2_2t3,t1_5t6,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t1_3t4,t1_4t5,t2_1t2,t2_2t3,t1_5t6,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t1_3t4,t1_4t5,t1_5t6,t2_1t2,t2_2t3,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t1_3t4,t1_4t5,t1_5t6,t2_1t2,t2_2t3,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t1_3t4,t1_4t5,t1_5t6,t2_1t2,t2_2t3,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t1_3t4,t1_4t5,t2_1t2,t1_5t6,t2_2t3,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t1_3t4,t1_4t5,t2_1t2,t1_5t6,t2_2t3,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t1_3t4,t1_4t5,t2_1t2,t1_5t6,t2_2t3,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t1_3t4,t1_4t5,t2_1t2,t2_2t3,t2_3t4,t1_5t6,t1_6t1,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t1_3t4,t2_6t1,t2_1t2,t1_4t5,t2_2t3,t1_5t6,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t2_5t6,t1_2t3,t1_3t4,t2_6t1,t2_1t2,t1_4t5,t2_2t3,t1_5t6,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t1_3t4,t2_6t1,t2_1t2,t1_4t5,t2_2t3,t1_5t6,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t1_3t4,t2_6t1,t2_1t2,t1_4t5,t1_5t6,t2_2t3,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t2_5t6,t1_2t3,t1_3t4,t2_6t1,t2_1t2,t1_4t5,t1_5t6,t2_2t3,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t1_3t4,t2_6t1,t2_1t2,t1_4t5,t1_5t6,t2_2t3,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t1_3t4,t2_6t1,t2_1t2,t1_4t5,t2_2t3,t2_3t4,t1_5t6,t1_6t1,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t1_3t4,t2_1t2,t1_4t5,t2_2t3,t1_5t6,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t1_3t4,t2_1t2,t1_4t5,t2_2t3,t1_5t6,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t1_3t4,t2_1t2,t1_4t5,t2_2t3,t1_5t6,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t1_3t4,t2_1t2,t1_4t5,t1_5t6,t2_2t3,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t1_3t4,t2_1t2,t1_4t5,t1_5t6,t2_2t3,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t1_3t4,t2_1t2,t1_4t5,t1_5t6,t2_2t3,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t2_5t6,t1_2t3,t2_6t1,t1_3t4,t2_1t2,t1_4t5,t2_2t3,t2_3t4,t1_5t6,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t1_3t4,t2_5t6,t2_6t1,t2_1t2,t1_4t5,t2_2t3,t1_5t6,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t1_2t3,t1_3t4,t2_5t6,t2_6t1,t2_1t2,t1_4t5,t2_2t3,t1_5t6,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t1_3t4,t2_5t6,t2_6t1,t2_1t2,t1_4t5,t2_2t3,t1_5t6,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t1_2t3,t1_3t4,t2_5t6,t2_6t1,t2_1t2,t1_4t5,t1_5t6,t2_2t3,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t1_2t3,t1_3t4,t2_5t6,t2_6t1,t2_1t2,t1_4t5,t1_5t6,t2_2t3,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t1_3t4,t2_5t6,t2_6t1,t2_1t2,t1_4t5,t1_5t6,t2_2t3,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t1_2t3,t1_3t4,t2_5t6,t2_6t1,t2_1t2,t1_4t5,t2_2t3,t2_3t4,t1_5t6,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t1_3t4,t2_6t1,t2_1t2,t1_4t5,t2_2t3,t1_5t6,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t1_2t3,t2_5t6,t1_3t4,t2_6t1,t2_1t2,t1_4t5,t2_2t3,t1_5t6,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t1_3t4,t2_6t1,t2_1t2,t1_4t5,t2_2t3,t1_5t6,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t1_3t4,t2_6t1,t2_1t2,t1_4t5,t1_5t6,t2_2t3,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t1_2t3,t2_5t6,t1_3t4,t2_6t1,t2_1t2,t1_4t5,t1_5t6,t2_2t3,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t1_3t4,t2_6t1,t2_1t2,t1_4t5,t1_5t6,t2_2t3,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t1_3t4,t2_6t1,t2_1t2,t1_4t5,t2_2t3,t2_3t4,t1_5t6,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t1_3t4,t2_1t2,t1_4t5,t2_2t3,t1_5t6,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t1_3t4,t2_1t2,t1_4t5,t2_2t3,t1_5t6,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t1_3t4,t2_1t2,t1_4t5,t2_2t3,t1_5t6,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t1_3t4,t2_1t2,t1_4t5,t1_5t6,t2_2t3,t1_6t1,t1_1t2,t2_3t4],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t1_3t4,t2_1t2,t1_4t5,t1_5t6,t2_2t3,t2_3t4,t1_6t1,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t1_3t4,t2_1t2,t1_4t5,t1_5t6,t2_2t3,t1_6t1,t2_3t4,t1_1t2],
 [t2_4t5,t1_2t3,t2_5t6,t2_6t1,t1_3t4,t2_1t2,t1_4t5,t2_2t3,t2_3t4,t1_5t6,t1_6t1,t1_1t2]}
The state realization of the supervisor (disabled events):
{(("sure"|->s3_s2)|->{t2_2t3}),(("sure"|->s4_s2)|->{t2_2t3}),(("sure"|->s2_s4)|->{t1_2t3}),
 (("sure"|->s6_s1)|->{t1_6t1}),(("sure"|->s6_s2)|->{t1_6t1}),(("sure"|->s2_s6)|->{t2_6t1}),
 (("sure"|->s5_s4)|->{t2_4t5}),(("sure"|->s4_s5)|->{t1_4t5}),(("sure"|->s6_s4)|->{t2_4t5}),
 (("sure"|->s4_s6)|->{t1_4t5}),
 (("uncertain"|->s2_s3)|->{t1_2t3}),(("uncertain"|->s1_s4)|->{t2_4t5})}
The state realization of the supervisor (enabled events):
{(("sure"|->s3_s1)|->{t1_3t4,t2_1t2}),(("sure"|->s1_s3)|->{t1_1t2,t2_3t4}),
 (("sure"|->s4_s1)|->{t1_4t5,t2_1t2}),(("sure"|->s3_s2)|->{t1_3t4}),
 (("sure"|->s5_s1)|->{t1_5t6,t2_1t2}),(("sure"|->s4_s2)|->{t1_4t5}),
 (("sure"|->s2_s4)|->{t2_4t5}),(("sure"|->s6_s1)|->{t2_1t2}),
 (("sure"|->s5_s2)|->{t1_5t6,t2_2t3}),(("sure"|->s2_s5)|->{t1_2t3,t2_5t6}),
 (("sure"|->s6_s2)|->{t2_2t3}),(("sure"|->s5_s3)|->{t1_5t6,t2_3t4}),
 (("sure"|->s3_s5)|->{t1_3t4,t2_5t6}),(("sure"|->s2_s6)|->{t1_2t3}),
 (("sure"|->s6_s3)|->{t1_6t1,t2_3t4}),(("sure"|->s5_s4)|->{t1_5t6}),
 (("sure"|->s4_s5)|->{t2_5t6}),(("sure"|->s3_s6)|->{t1_3t4,t2_6t1}),
 (("sure"|->s6_s4)|->{t1_6t1}),(("sure"|->s4_s6)|->{t2_6t1}),
 (("uncertain"|->s2_s3)|->{t2_3t4}),(("uncertain"|->s1_s4)|->{t1_1t2})}
*/