// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL tab2_maze
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------
USES
  LBmaze_LB_5rooms /*@file "../../Maze/LBmaze_LB_5rooms.mch" */
//LBmaze_LB_8rooms /*@file "../../Maze/LBmaze_LB_8rooms.mch" */

CONSTANTS
 output_file_name,      // output file name
 T_G_FB,                // traces of the free behavior
 L_G_FB,                // strings of the free behavior
 T_G_LB,                // traces of the legal behavior
 L_G_LB                 // strings of the legal behavior

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 10000000;
 SET_PREF_SMT == FALSE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../../Theories/SCT_core.def";
 "LibraryIO.def";

PROPERTIES
 G_FB : Generator_C(G_FB'states,G_FB'events) &
 G_LB : FSA(G_LB'states,G_LB'events) &

 output_file_name = "~/Documents/SCTbenchmark/output_maze.txt" &

 T_G_FB = GetTraces(G_FB,bound_G_FB) &
 L_G_FB = Traces2Strings(T_G_FB) &
 T_G_LB = GetTraces(G_LB,bound_G_LB) &
 L_G_LB = Traces2Strings(T_G_LB)

VARIABLES
 code,
 i, K_i, K_succ_i
INITIALISATION
 code := 0;
 i := 1;
 K_i := L_G_LB;
 K_succ_i := iStepSupC_PC(K_i,L_G_FB,G_FB'events-G_FB'ctrlEvents)
INVARIANT
 code >= 0 & code <= 3 &
 i >= 1 &
 K_i <: L_G_LB & K_succ_i <: L_G_LB

OPERATIONS
 DisplayStatistics = PRE code = 0 THEN
  PRINT("Display statistics...");
  FPRINTF(output_file_name, "~nMaze with ~w rooms: ", [nbr_rooms]);
  FPRINTF(output_file_name, "~w states and ", [card(G_FB'states)]);
  FPRINTF(output_file_name, "~w transitions~n~n", [card(G_FB'tf)]);
  FPRINTF(output_file_name, "Bounds: ~w and ~w~n", [bound_G_FB, bound_G_LB]);
  FPRINTF(output_file_name, "Number of traces (free behavior): ~w~n", [card(T_G_FB)]);
  FPRINTF(output_file_name, "Number of strings (free behavior): ~w~n", [card(L_G_FB)]);
  FPRINTF(output_file_name, "Number of traces (legal behavior): ~w~n", [card(T_G_LB)]);
  FPRINTF(output_file_name, "Number of strings (legal behavior): ~w~n", [card(L_G_LB)]);
  code := 1 END;

 GetSupC = PRE code = 1 THEN
  PRINT("Calculate supRC...");
  WHILE K_i /= K_succ_i DO
   i := i+1;
   K_i := K_succ_i;
   K_succ_i := iStepSupC_PC(K_i,L_G_FB,G_FB'events-G_FB'ctrlEvents)
  INVARIANT card(K_i) >= card(K_succ_i)
  VARIANT card(K_i)
  END;
  FPRINTF(output_file_name, "Number of iterations: ~w~n", [i]);
  FPRINTF(output_file_name, "|supRC(K)|: ~w~n", [card(K_i)]);
  FPRINTF(output_file_name, "supC_(K): ~w~n", [K_i]);
  K_i := UNION(t_).(t_ : T_G_LB & t_'string : K_i & t_'state = r2_r4 | {t_'string});
  code := 2
 END;

 GetSupervisors = PRE code = 2 THEN
  PRINT("Calculate supervisor...");
  ANY phi WHERE
   phi : SRsupervisor___(G_FB'states,G_FB'ctrlEvents) &
   phi = SRsupervisorDISBLlabel_(T_G_LB,pr(K_i),L_G_FB,G_FB'ctrlEvents,bound_G_FB) THEN
    FPRINTF(output_file_name,"The state realization of the supervisor (disabled events): ~w~n",[phi]) END;
  ANY phi WHERE
   phi : SRsupervisor___(G_FB'states,G_FB'events) &
   phi = SRsupervisorENBLlabel_(T_G_LB,pr(K_i),G_FB'events,bound_G_FB) THEN
    FPRINTF(output_file_name,"The state realization of the supervisor (enabled events): ~w~n",[phi]) END;
  code := 3
 END
END