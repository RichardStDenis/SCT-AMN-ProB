// Richard St-Denis, Universite de Sherbrooke, 2023.
MODEL test_sbc_trains_To_r

SEES
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------

  SBtrains_8_2 /*@file "../Trains/SBtrains_8_2.mch" */

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
 "../Theories/SBCDES.def";
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
  code := 3 END /*;

 ComputeSFBC_e = PRE code = 3 THEN
  PRINT("Compute SFBC (enabled events)...");
  sfbc_e := SFBCEnabled(G_FB,supCCP);
  FPRINTF(output_file_name,"Enabled events: ~w~n",[sfbc_e]);
  code := 4 END;

 CheckComplementarity_ = PRE code = 4 &
  !x_.(x_ : supCCP => sfbc_d (x_) /\ sfbc_e(x_) = {}) THEN
   PRINT("The SFBC functions are complementary.");
   code := 5 END */ END

/*
OUTPUT:
"Display statistics..."
cover(DisplayStatistics)
"Compute recursively the supremal coreachable controllable predicate..."
cover(ComputeSupCCP)
"Compute SFBC (disabled events)..."
cover(ComputeSFBC_d)

ALL OPERATIONS COVERED

8 sections, 2 trains: 64 states and 128 transitions

Predicate (56 states):
Recursive solution
supCCP (48 reachable states):
{s3_s1,s1_s3,s4_s1,s3_s2,s2_s3,s1_s4,s5_s1,s4_s2,s2_s4,s1_s5,s6_s1,s5_s2,s2_s5,s1_s6,
 s7_s1,s6_s2,s5_s3,s3_s5,s2_s6,s1_s7,s8_s1,s7_s2,s6_s3,s5_s4,s4_s5,s3_s6,s2_s7,s1_s8,
 s8_s2,s7_s3,s6_s4,s4_s6,s3_s7,s2_s8,s8_s3,s7_s4,s4_s7,s3_s8,s8_s4,s7_s5,s5_s7,s4_s8,
 s8_s5,s7_s6,s6_s7,s5_s8,s8_s6,s6_s8}

Disabled events:
{(s3_s1|->{}),(s1_s3|->{}),(s4_s1|->{}),(s3_s2|->{t2_2t3}),(s2_s3|->{t1_2t3}),
 (s1_s4|->{}),(s5_s1|->{}),(s4_s2|->{t2_2t3}),(s2_s4|->{t1_2t3}),(s1_s5|->{}),
 (s6_s1|->{}),(s5_s2|->{}),(s2_s5|->{}),(s1_s6|->{}),(s7_s1|->{}),(s6_s2|->{}),
 (s5_s3|->{}),(s3_s5|->{}),(s2_s6|->{}),(s1_s7|->{}),(s8_s1|->{t1_8t1}),(s7_s2|->{}),
 (s6_s3|->{}),(s5_s4|->{t2_4t5}),(s4_s5|->{t1_4t5}),(s3_s6|->{}),(s2_s7|->{}),
 (s1_s8|->{t2_8t1}),(s8_s2|->{t1_8t1}),(s7_s3|->{}),(s6_s4|->{t2_4t5}),
 (s4_s6|->{t1_4t5}),(s3_s7|->{}),(s2_s8|->{t2_8t1}),(s8_s3|->{}),(s7_s4|->{}),
 (s4_s7|->{}),(s3_s8|->{}),(s8_s4|->{}),(s7_s5|->{}),(s5_s7|->{}),(s4_s8|->{}),
 (s8_s5|->{}),(s7_s6|->{t2_6t7}),(s6_s7|->{t1_6t7}),(s5_s8|->{}),
 (s8_s6|->{t2_6t7}),(s6_s8|->{t1_6t7})}
*/