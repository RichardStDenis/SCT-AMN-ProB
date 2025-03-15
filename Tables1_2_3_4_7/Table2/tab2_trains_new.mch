// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL tab2_trains
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------
USES
//LBtrains_LB_1_4_2  /*@file "../../Trains/LBtrains_LB_1_4_2.mch" */
//LBtrains_LB_2_4_2  /*@file "../../Trains/LBtrains_LB_2_4_2.mch" */
  LBtrains_LB_1_6_2  /*@file "../../Trains/LBtrains_LB_1_6_2.mch" */
//LBtrains_LB_2_6_2  /*@file "../../Trains/LBtrains_LB_2_6_2.mch" */
// LBtrains_LB_1_8_2  /*@file "../../Trains/LBtrains_LB_1_8_2.mch" */
//LBtrains_LB_2_8_2  /*@file "../../Trains/LBtrains_LB_2_8_2.mch" */
//LBtrains_LB_1_10_2 /*@file "../../Trains/LBtrains_LB_1_10_2.mch" */
//LBtrains_LB_2_10_2 /*@file "../../Trains/LBtrains_LB_2_10_2.mch" */

CONSTANTS
 output_file_name,      // output file name
 T_G_FB,                // Traces of the free behavior
 L_G_FB,                // Prefix-closed and marked languages of the free behavior
 Lm_G_FB,
 T_G_LB,                // Traces of the legale behavior
 Lm_G_LB                // PMarked languages of the legal behavior

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
 "../../Theories/SCT_core.def";
 "LibraryIO.def";
 
  SRsupervisorTrENBL__(X,E,T,prK) == UNION(x_).(x_ : X | 
   {(x_,{e_ | e_: E & #(t_).(t_ : T & t_'state = x_ & t_'string <-e_ : prK)})} )

PROPERTIES
 G_FB : Generator_C(G_FB'states,G_FB'events) &
 G_LB : FSA(G_LB'states,G_LB'events) &

 output_file_name = "~/Documents/SCTBenchmark/output_trains.txt" &

 GetTracesFB = lambdaGetTraces(G_FB,bound_G_FB,GetTracesFB) &
 T_G_FB = GetTracesFB(baseCaseTr(G_FB),0) &
 L_G_FB = Traces2Strings(T_G_FB) &
 Lm_G_FB = Traces2Tasks(T_G_FB,G_FB'Xm) &

 GetTracesLB = lambdaGetTraces(G_LB,bound_G_LB,GetTracesLB) &
 T_G_LB = GetTracesLB(baseCaseTr(G_LB),0) &
 Lm_G_LB = Traces2Tasks(T_G_LB,G_LB'Xm)

VARIABLES
 code,
 i, K_i, K_succ_i, T_supK, X_sup
INITIALISATION
 code := 0;
 i := 1;
 K_i := supR(Lm_G_LB,Lm_G_FB);
 K_succ_i := iStepSupC(K_i,pr(K_i),L_G_FB,G_FB'events-G_FB'ctrlEvents);
 T_supK := {};
 X_sup := {}
INVARIANT
 code >= 0 & code <= 4 &
 i >= 1 &
 K_i <: Lm_G_LB & K_succ_i <: Lm_G_LB &
 T_supK <: T_G_LB &
 X_sup <: G_LB'states

OPERATIONS
 DisplayStatistics = PRE code = 0 THEN
  PRINT("Display statistics...");
  FPRINTF(output_file_name, "~nRailway with ~w sections and ~w trains: ", [nbr_sections, nbr_trains]);
  FPRINTF(output_file_name, "~w states and ", [card(G_FB'states)]);
  FPRINTF(output_file_name, "~w transitions~n~n", [card(G_FB'tf)]);
  FPRINTF(output_file_name, "Bounds: ~w and ~w~n", [bound_G_FB, bound_G_LB]);
  FPRINTF(output_file_name, "Number of traces (free behavior): ~w~n", [card(T_G_FB)]);
  FPRINTF(output_file_name, "Number of words (free behavior): ~w~n", [card(L_G_FB)]);
  FPRINTF(output_file_name, "Number of tasks (free behavior): ~w~n", [card(Lm_G_FB)]);
  FPRINTF(output_file_name, "Number of traces (legal behavior): ~w~n", [card(T_G_LB)]);
  FPRINTF(output_file_name, "Number of tasks (legal behavior): ~w~n", [card(Lm_G_LB)]);
  FPRINTF(output_file_name, "Number of prefixes (legal behavior): ~w~n", [card(pr(Lm_G_LB))]);
  code := 1 END;

 GetSupRC = PRE code = 1 & Lm_G_LB <: Lm_G_FB THEN
  PRINT("Calculate supRC...");
  WHILE K_i /= K_succ_i DO
   i := i+1;
   K_i := K_succ_i;
   K_succ_i := iStepSupC(K_i,pr(K_i),L_G_FB,G_FB'events-G_FB'ctrlEvents)
  INVARIANT card(K_i) >= card(K_succ_i)
  VARIANT card(K_i)
  END;
  FPRINTF(output_file_name, "Number of iterations: ~w~n", [i]);
  FPRINTF(output_file_name, "|supRC(K)|: ~w~n", [card(K_i)]);
  FPRINTF(output_file_name, "|pr(supC(K))|: ~w~n", [card(pr(K_i))]);
//FPRINTF(output_file_name, "supRC_(K): ~w~n", [K_i]);
  code := 2
 END;

 GetSupervisors = PRE code = 2 THEN
  PRINT("Calculate supervisor...");
  T_supK := {t_ | t_ : T_G_LB & t_'string : pr(K_i)};
  X_sup := {x_ | x_ : G_LB'states & #t_.(t_ : T_supK & x_ = t_'state)};
  FPRINTF(output_file_name, "States of supC(K): ~w~n", [X_sup]);
//  LET sp BE sp = {s_ | s_ : G_LB'states & #t_.(t_ : T_supK & s_ = t_'state)} IN
//   FPRINTF(output_file_name, "States of supC(K): ~w~n", [sp]) END;
  ANY phi WHERE
   phi : SRsupervisor_(G_FB'states,G_FB'events) &
//   phi = SRsupervisorTrENBL_(T_supK,G_FB'ctrlEvents) THEN
   phi = SRsupervisorTrENBL__(X_sup,G_FB'ctrlEvents,T_supK,pr(K_i)) THEN
    LET phi_ BE phi_ = {x_,E_ | (x_,E_) : phi & E_ /= {}} IN

    FPRINTF(output_file_name,"The state realization of the supervisor (enabled events): ~w~n",[phi_]) END END;
//  ANY phi WHERE
//   phi : SRsupervisor_(G_FB'states,G_FB'events) &
//   phi = SRsupervisorTrENBLuncertain_(T_supK,G_FB'events,bound_G_FB) THEN
//    FPRINTF(output_file_name,"The state realization of the supervisor (uncertain enabled events): ~w~n",[phi]) END;
  code := 3
 END;

 DisplayEnd = PRE code = 3 THEN
  PRINT("End of processing.");
  code := 4
 END
END