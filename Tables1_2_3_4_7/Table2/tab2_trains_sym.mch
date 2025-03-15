// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL tab2_trains_sym
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------
USES
//LBtrains_LB_1_4_2  /*@file "../../Trains/LBtrains_LB_1_4_2.mch" */
//LBtrains_LB_2_4_2  /*@file "../../Trains/LBtrains_LB_2_4_2.mch" */
//LBtrains_LB_1_6_2  /*@file "../../Trains/LBtrains_LB_1_6_2.mch" */
//LBtrains_LB_2_6_2  /*@file "../../Trains/LBtrains_LB_2_6_2.mch" */
  LBtrains_LB_1_8_2  /*@file "../../Trains/LBtrains_LB_1_8_2.mch" */
//LBtrains_LB_2_8_2  /*@file "../../Trains/LBtrains_LB_2_8_2.mch" */
//LBtrains_LB_1_10_2 /*@file "../../Trains/LBtrains_LB_1_10_2.mch" */
//LBtrains_LB_2_10_2 /*@file "../../Trains/LBtrains_LB_2_10_2.mch" */
// LBtrains_LB_1_12_2 /*@file "../../Trains/LBtrains_LB_1_12_2.mch" */
//LBtrains_LB_2_12_2 /*@file "../../Trains/LBtrains_LB_2_12_2.mch" */

CONSTANTS
 output_file_name,      // output file name
 T_G_FB,                // Traces of the free behavior
 L_G_FB,                // Prefix-closed of the free behavior
 T_G_LB,                // Traces of the legale behavior
 L_G_LB                 // Prefix-closed of the legal behavior

ABSTRACT_CONSTANTS GetTracesFB, GetTracesLB

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 100000000;
 SET_PREF_SMT == FALSE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../../Theories/SCT_core.def";
 "LibraryIO.def";

PROPERTIES
 G_FB : Generator_C(G_FB'states,G_FB'events) &
 G_LB : FSA(G_LB'states,G_LB'events) &

 output_file_name = "~/Documents/SCTBenchmark/output_trains.txt" &

 GetTracesFB = lambdaGetTraces(G_FB,bound_G_FB,GetTracesFB) &
 T_G_FB = GetTracesFB(baseCaseTr(G_FB),0) &
 L_G_FB = Traces2Strings(T_G_FB) &

 GetTracesLB = lambdaGetTraces(G_LB,bound_G_LB,GetTracesLB) &
 T_G_LB = GetTracesLB(baseCaseTr(G_LB),0) &
 L_G_LB = Traces2Strings(T_G_LB)

VARIABLES
 code,
 i, K_i, K_succ_i, T_supK
INITIALISATION
 code := 0;
 i := 1;
 K_i := L_G_LB;
 K_succ_i := iStepSupC_PC(K_i,L_G_FB,G_FB'events-G_FB'ctrlEvents);
 T_supK := {}
INVARIANT
 code >= 0 & code <= 4 &
 i >= 1 &
 K_i <: L_G_LB & K_succ_i <: L_G_LB &
 T_supK <: T_G_LB

OPERATIONS
 DisplayStatistics = PRE code = 0 THEN
  PRINT("Display statistics...");
  FPRINTF(output_file_name, "~nRailway with ~w sections and ~w trains: ", [nbr_sections, nbr_trains]);
  FPRINTF(output_file_name, "~w states and ", [card(G_FB'states)]);
  FPRINTF(output_file_name, "~w transitions~n~n", [card(G_FB'tf)]);
  FPRINTF(output_file_name, "Bounds: ~w and ~w~n", [bound_G_FB, bound_G_LB]);
  FPRINTF(output_file_name, "Number of traces (free behavior): ~w~n", [card(T_G_FB)]);
  FPRINTF(output_file_name, "Number of words (free behavior): ~w~n", [card(L_G_FB)]);
  FPRINTF(output_file_name, "Number of traces (legal behavior): ~w~n", [card(T_G_LB)]);
  FPRINTF(output_file_name, "Number of words (legal behavior): ~w~n", [card(L_G_LB)]);
  code := 1 END;

 GetSupRC = PRE code = 1 THEN //& L_G_LB <: L_G_FB THEN
  PRINT("Calculate supC_PC...");
  WHILE K_i /= K_succ_i DO
   i := i+1;
   K_i := K_succ_i;
   K_succ_i := iStepSupC_PC(K_i,L_G_FB,G_FB'events-G_FB'ctrlEvents)
  INVARIANT card(K_i) >= card(K_succ_i)
  VARIANT card(K_i)
  END;
  FPRINTF(output_file_name, "Number of iterations: ~w~n", [i]);
  FPRINTF(output_file_name, "|supC(K)|: ~w~n", [card(K_i)]);
  FPRINTF(output_file_name, "|pr(supC_PC(K))|: ~w~n", [card(pr(K_i))]);
//FPRINTF(output_file_name, "supC(K): ~w~n", [K_i]);
  code := 2
 END;

GetSupervisors = PRE code = 2 THEN
  PRINT("Calculate supervisor...");
  T_supK := Strings2Traces(K_i,T_G_LB);
  LET sp BE sp = {s_ | s_ : G_LB'states & #t_.(t_ : T_supK & s_ = t_'state)} IN
   FPRINTF(output_file_name, "States of supC(K): ~w~n", [sp]) END;
  ANY phi WHERE
   phi : SRsupervisor_(G_FB'states,G_FB'events) &
   phi = SRsupervisorTrENBL_(T_supK,G_FB'ctrlEvents) THEN
    FPRINTF(output_file_name,"The state realization of the supervisor (enabled events): ~w~n",[phi]) END;
  ANY phi WHERE
   phi : SRsupervisor_(G_FB'states,G_FB'events) &
   phi = SRsupervisorTrENBLuncertain_(T_supK,G_FB'events,bound_G_FB) THEN
    FPRINTF(output_file_name,"The state realization of the supervisor (uncertain enabled events): ~w~n",[phi]) END;
  code := 3
 END;

 DisplayEnd = PRE code = 3 THEN
  PRINT("End of processing.");
  code := 4
 END
END