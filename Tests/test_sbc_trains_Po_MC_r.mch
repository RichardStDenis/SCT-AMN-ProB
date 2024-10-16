// Richard St-Denis, Universite de Sherbrooke, 2023.
MODEL test_sbc_trains_Po_MC_r

SEES
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------

  tunnel_8_2 /*@file "../Trains/tunnel_8_2.mch" */

CONSTANTS
 output_file_name,           // output file name
 Qpred                       // set of good states

ABSTRACT_CONSTANTS GetmaxMCP    // Lambda function to compute a maximal
                                //  M-controllable predicate

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
 "LibraryIO.def";

PROPERTIES
 output_file_name = "~/Documents/SCTBenchmark/output_trains.txt" &

 GetmaxMCP = lambdaGetmaxMCP(G_FB,tunnel,GetmaxMCP) &
 G_FB : Generator_C(States,Events) &
 Qpred = Qpred1
 //Qpred = Qpred2

VARIABLES
 code,
 maxMCP,      // maximal M-controllable apredicate stronger than Q (recursive solution)
 sfbc_d,      // SFBC function for the set of disabled events at any state x
 sfbc_e       // SFBC function for the set of enabled events at any state x

INITIALISATION
 code := 0;
 maxMCP := {}; sfbc_d := {}; sfbc_e :=  {}

INVARIANT
 code >= 0 & code <= 4 &
 maxMCP <: Qpred &
 sfbc_d : SFBCf(G_FB'states,G_FB'ctrlEvents) &
 sfbc_e : SFBCf(G_FB'states,G_FB'events)

OPERATIONS
 DisplayStatistics = PRE code = 0 THEN
  PRINT("Display statistics...");
  FPRINTF(output_file_name,"~w sections, ~w trains: ",[card(LocalStates),card(G_FB'events)/card(LocalStates)]);
  FPRINTF(output_file_name,"~w states and ",[card(G_FB'states)]);
  FPRINTF(output_file_name,"~w transitions~n~n",[card(G_FB'tf)]);
  FPRINTF(output_file_name,"Predicate (~w states):~n",[card(Qpred)]);
  code := 1 END;

ComputeMaxCMCP = PRE code = 1 THEN
  PRINT("Compute recursively a maximal coreachable M-controllable predicate...");
  maxMCP := GetmaxMCP(Qpred);
  FPRINTF(output_file_name, "Recursive solution~nmaxCMCP (~w reachable states):~n",[card(maxMCP)]);
  FPRINTF(output_file_name, "~w~n~n",[maxMCP]);
  code := 2 END;

 ComputeSFBCd = PRE code = 2 THEN
  PRINT("Compute SFBC (disabled events)...");
  sfbc_d := SFBC_breve(G_FB,tunnel,maxMCP);
  FPRINTF(output_file_name, "Disabled events: ~w~n", [sfbc_d]);
  code := 3 END;

 ComputeSFBCe = PRE code = 3 THEN
  PRINT("Compute SFBC (enabled events)...");
  sfbc_e := SFBC_breveEnabled(G_FB,tunnel,maxMCP);
  FPRINTF(output_file_name,"Enabled events: ~w~n",[sfbc_e]);
  code := 4 END END

/*
OUTPUT:
"Display statistics..."
cover(DisplayStatistics)
"Compute recursively a maximal coreachable M-controllable predicate..."
cover(ComputeMaxCMCP)
"Compute SFBC (disabled events)..."
cover(ComputeSFBCd)
"Compute SFBC (enabled events)..."
cover(ComputeSFBCe)

ALL OPERATIONS COVERED

8 sections, 2 trains: 64 states and 128 transitions

Predicate (56 states):
Recursive solution
maxCMCP (44 reachable states):
{s3_s1,s1_s3,s4_s1,s3_s2,s2_s3,s1_s4,s5_s1,s4_s2,s2_s4,s1_s5,s6_s1,s5_s2,s2_s5,s1_s6,
 s7_s1,s6_s2,s2_s6,s1_s7,s8_s1,s7_s2,s6_s3,s3_s6,s2_s7,s1_s8,s8_s2,s7_s3,s6_s4,s4_s6,
 s3_s7,s2_s8,s8_s3,s7_s4,s4_s7,s3_s8,s8_s4,s7_s5,s5_s7,s4_s8,s8_s5,s7_s6,s6_s7,s5_s8,
 s8_s6,s6_s8}

Disabled events:
{(s3_s1|->{}),(s1_s3|->{}),(s4_s1|->{}),(s3_s2|->{t2_2t3}),
 (s2_s3|->{t1_2t3}),(s1_s4|->{}),(s5_s1|->{}),(s4_s2|->{t2_2t3}),
 (s2_s4|->{t1_2t3}),(s1_s5|->{}),(s6_s1|->{}),(s5_s2|->{t2_2t3}),
 (s2_s5|->{t1_2t3}),(s1_s6|->{}),(s7_s1|->{}),(s6_s2|->{}),
 (s2_s6|->{}),(s1_s7|->{}),(s8_s1|->{t1_8t1}),(s7_s2|->{}),
 (s6_s3|->{t2_4t5}),(s3_s6|->{t1_4t5}),(s2_s7|->{}),
 (s1_s8|->{t2_8t1}),(s8_s2|->{t1_8t1}),(s7_s3|->{}),
 (s6_s4|->{t2_4t5}),(s4_s6|->{t1_4t5}),(s3_s7|->{}),
 (s2_s8|->{t2_8t1}),(s8_s3|->{}),(s7_s4|->{}),(s4_s7|->{}),(s3_s8|->{}),(s8_s4|->{}),
 (s7_s5|->{}),(s5_s7|->{}),(s4_s8|->{}),(s8_s5|->{}),(s7_s6|->{t2_6t7}),
 (s6_s7|->{t1_6t7}),(s5_s8|->{}),(s8_s6|->{t2_6t7}),(s6_s8|->{t1_6t7})}
Enabled events:
{(s3_s1|->{t1_3t4,t2_1t2}),(s1_s3|->{t1_1t2,t2_3t4}),(s4_s1|->{t1_4t5,t2_1t2}),
 (s3_s2|->{t1_3t4}),(s2_s3|->{t2_3t4}),(s1_s4|->{t1_1t2,t2_4t5}),
 (s5_s1|->{t1_5t6,t2_1t2}),(s4_s2|->{t1_4t5}),(s2_s4|->{t2_4t5}),
 (s1_s5|->{t1_1t2,t2_5t6}),(s6_s1|->{t1_6t7,t2_1t2}),(s5_s2|->{t1_5t6}),
 (s2_s5|->{t2_5t6}),(s1_s6|->{t1_1t2,t2_6t7}),(s7_s1|->{t1_7t8,t2_1t2}),
 (s6_s2|->{t1_6t7,t2_2t3}),(s2_s6|->{t1_2t3,t2_6t7}),(s1_s7|->{t1_1t2,t2_7t8}),
 (s8_s1|->{t2_1t2}),(s7_s2|->{t1_7t8,t2_2t3}),(s6_s3|->{t1_6t7,t2_3t4}),
 (s3_s6|->{t1_3t4,t2_6t7}),(s2_s7|->{t1_2t3,t2_7t8}),(s1_s8|->{t1_1t2}),
 (s8_s2|->{t2_2t3}),(s7_s3|->{t1_7t8,t2_3t4}),(s6_s4|->{t1_6t7}),
 (s4_s6|->{t2_6t7}),(s3_s7|->{t1_3t4,t2_7t8}),(s2_s8|->{t1_2t3}),
 (s8_s3|->{t1_8t1,t2_3t4}),(s7_s4|->{t1_7t8,t2_4t5}),(s4_s7|->{t1_4t5,t2_7t8}),
 (s3_s8|->{t1_3t4,t2_8t1}),(s8_s4|->{t1_8t1,t2_4t5}),(s7_s5|->{t1_7t8,t2_5t6}),
 (s5_s7|->{t1_5t6,t2_7t8}),(s4_s8|->{t1_4t5,t2_8t1}),(s8_s5|->{t1_8t1,t2_5t6}),
 (s7_s6|->{t1_7t8}),(s6_s7|->{t2_7t8}),(s5_s8|->{t1_5t6,t2_8t1}),(s8_s6|->{t1_8t1}),
 (s6_s8|->{t2_8t1})}
*/
