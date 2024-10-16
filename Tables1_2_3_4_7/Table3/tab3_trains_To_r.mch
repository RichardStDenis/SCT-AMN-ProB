// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL tab3_trains_To_r

SEES
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------

  SBtrains_4_2 /*@file "../../Trains/SBtrains_4_2.mch" */
//SBtrains_6_2 /*@file "../../Trains/SBtrains_6_2.mch" */
//SBtrains_6_3 /*@file "../../Trains/SBtrains_6_3.mch" */ 
//SBtrains_8_2 /*@file "../../Trains/SBtrains_8_2.mch" */
//SBtrains_8_3 /*@file "../../Trains/SBtrains_8_3.mch" */
//SBtrains_8_4 /*@file "../../Trains/SBtrains_8_4.mch" */
//SBtrains_10_2 /*@file "../../Trains/SBtrains_10_2.mch" */
//SBtrains_10_3 /*@file "../../Trains/SBtrains_10_3.mch" */
//SBtrains_12_2 /*@file "../../Trains/SBtrains_12_2.mch" */
//SBtrains_12_3 /*@file "../../Trains/SBtrains_12_3.mch" */

CONSTANTS
 output_file_name,           // output file name
 Qpred                       // set of good states

ABSTRACT_CONSTANTS GetsupCCP      // Lambda function to compute the supremal
                                  // coreachable controllable predicate

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 50000000;
 SET_PREF_SMT == FALSE;
 SET_PREF_KODKOD == FALSE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../../Theories/SBCDES.def";
 "LibraryIO.def"

PROPERTIES
 output_file_name = "~/Documents/SCTBenchmark/output_trains.txt" &

 GetsupCCP = lambdaGetsupCCP(G_FB,GetsupCCP) &
 G_FB : Generator_C(States,Events) &
  Qpred = Qpred1
//Qpred = Qpred2

VARIABLES
 code,
 supCCP,      // supremal controllable and coreachable predicate stronger than Q (recursive solution)
 sfbc_d,      // SFBC function for the set of disabled events at any state x
 sfbc_e       // SFBC function for the set of enabled events at any state x

INITIALISATION
 code := 0;
 supCCP := {}; sfbc_d := {}; sfbc_e := {}

INVARIANT
 code >= 0 & code <= 5 &
 supCCP <: Qpred &
 sfbc_d : SFBCf(G_FB'states,G_FB'ctrlEvents) &
 sfbc_e : SFBCf(G_FB'states,G_FB'events)

OPERATIONS
 DisplayStatistics = PRE code = 0 THEN
  PRINT("Display statistics...");
  FPRINTF(output_file_name, "~w sections, ~w trains: ", [card(LocalStates), card(G_FB'events)/card(LocalStates)]);
  FPRINTF(output_file_name, "~w states and ", [card(G_FB'states)]);
  FPRINTF(output_file_name, "~w transitions~n~n", [card(G_FB'tf)]);
  FPRINTF(output_file_name, "Predicate (~w states):~n", [card(Qpred)]);
  code := 1 END;

 ComputeSupCCP = PRE code = 1 THEN
  PRINT("Compute recursively the supremal coreachable controllable predicate...");
  supCCP := GetsupCCP(Qpred);
  FPRINTF(output_file_name, "Recursive solution~nsupCCP (~w reachable states):~n", [card(supCCP)]);
  FPRINTF(output_file_name, "~w~n~n", [supCCP]);
  code := 2 END;

 ComputeSFBC_d = PRE code = 2 THEN
  PRINT("Compute SFBC (disabled events)...");
  sfbc_d := SFBC(G_FB,supCCP);
  FPRINTF(output_file_name, "Disabled events: ~w~n", [sfbc_d]);
  code := 3 END;

 ComputeSFBC_e = PRE code = 3 THEN
  PRINT("Compute SFBC (enabled events)...");
  sfbc_e := SFBCEnabled(G_FB,supCCP);
  FPRINTF(output_file_name,"Enabled events: ~w~n",[sfbc_e]);
  code := 4 END;

 CheckComplementarity_ = PRE code = 4 &
  !x_.(x_ : supCCP => sfbc_d (x_) /\ sfbc_e(x_) = {}) THEN
   PRINT("The SFBC functions are complementary.");
   code := 5 END END