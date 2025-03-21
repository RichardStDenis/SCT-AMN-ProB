// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL section5_3_ex4_MC_app

SEES
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------
  tunnels_8_2 /*@file "../Trains/tunnels_8_2.mch" */
// IMPORTANT: The initial state and marked state in the file LBtrains_FB_8_2
//            must be s2_s6.
//            The predicate Qpred2 must be selected in the file SBtrains_8_2.

CONSTANTS
 output_file_name,           // output file name
 Qpred                       // set of good states

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 50000000;
 SET_PREF_SMT == FALSE;
 SET_PREF_KODKOD == FALSE;
 SET_PREF_ALLOW_COMPLEX_LETS ==TRUE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../Theories/SBCDES.def";
 "LibraryIO.def";

PROPERTIES
 output_file_name = "~/Documents/SCTBenchmark/output_trains.txt" &

 G_FB : Generator_C(States,Events) &
 tunnel : obsMask(G_FB'states,ObservationSpace) &
 Qpred = Qpred2

VARIABLES
 code,
 supWCP,           // supremal controllable predicate stronger than Q
 appMCP,           // M-controllable predicate stronger than Q
 sfbc_e            // SFBC function for the set of enabled events when partial observation

INITIALISATION
 code := 0;
 supWCP := CtrlField(G_FB,Qpred); 
 appMCP := {};
 sfbc_e :=  {}

INVARIANT
 code >= 0 & code <= 4 &
 supWCP <: Qpred &
 appMCP <: Qpred &
 sfbc_e : SFBCf(G_FB'states,G_FB'events)

OPERATIONS
 DisplayStatistics = PRE code = 0 THEN
  PRINT("Display statistics...");
  FPRINTF(output_file_name,"~w sections, ~w trains: ",[card(LocalStates),card(G_FB'events)/card(LocalStates)]);
  FPRINTF(output_file_name,"~w states and ",[card(G_FB'states)]);
  FPRINTF(output_file_name,"~w transitions~n~n",[card(G_FB'tf)]);
  FPRINTF(output_file_name,"Predicate (~w states):~n",[card(Qpred)]);
  code := 1 END;

ComputeApproxQbyMCpred = PRE code = 1 THEN
  PRINT("Compute a M-controllable predicate...");
  appMCP := ApproxQbyMCpred(G_FB,tunnel,Qpred);
  FPRINTF(output_file_name,"Solution~nappMCP (~w reachable states):~n",[card(appMCP)]);
  FPRINTF(output_file_name,"~w~n~n",[appMCP]);
  code := 2 END;

 ComputeSFBCe = PRE code = 2 THEN
  PRINT("Compute SFBC (enabled events partial observation)...");
  sfbc_e := appMCP<|f_breve(G_FB,tunnel,supWCP);
  FPRINTF(output_file_name,"Enabled events (partiel observation): ~w~n",[sfbc_e]);
  code := 3 END;

 CheckNonblocking = PRE code = 3 THEN
  PRINTF("Cardinality of supSMCP: ~w~n",[card(appMCP)]);
  LET closed_loop_f_hat, crStates BE
   closed_loop_f_hat = InducedAutomaton(G_FB,sfbc_e,appMCP) &
   crStates = CR(closed_loop_f_hat,appMCP) IN
    PRINTF("Number of states coreachable in the closed-loop system: ~w~n",
     [card(crStates)]);
    IF card(appMCP) = card(crStates) THEN
     PRINT("The closed-loop system is nonblocking.") ELSE
     PRINT("The closed-loop system is blocking.") END END;
  code := 4 END END

/*
OUTPUT:
"Display statistics..."
cover(DisplayStatistics)
"Compute a M-controllable predicate..."
cover(ComputeApproxQbyMCpred)
"Compute SFBC (enabled events partial observation)..."
cover(ComputeSFBCe)
Cardinality of supSMCP: 30
Number of states coreachable in the closed-loop system: 30
"The closed-loop system is nonblocking."
cover(CheckNonblocking)

ALL OPERATIONS COVERED


8 sections, 2 trains: 64 states and 128 transitions

Predicate (40 states):
Solution
appMCP (30 reachable states):
{s4_s1,s1_s4,s5_s1,s4_s2,s2_s4,s1_s5,s6_s1,s5_s2,s2_s5,s1_s6,
 s6_s2,s2_s6,s7_s2,s6_s3,s3_s6,s8_s2,s7_s3,s6_s4,s4_s6,s3_s7,
 s8_s3,s7_s4,s4_s7,s3_s8,s8_s4,s4_s8,s8_s5,s5_s8,s8_s6,s6_s8}

Enabled events (partiel observation):
{(s4_s1|->{t1_4t5,t2_1t2}),(s1_s4|->{t1_1t2,t2_4t5}),(s5_s1|->{t1_5t6,t2_1t2}),
 (s4_s2|->{t1_4t5}),(s2_s4|->{t2_4t5}),(s1_s5|->{t1_1t2,t2_5t6}),(s6_s1|->{t2_1t2}),
 (s5_s2|->{t1_5t6}),(s2_s5|->{t2_5t6}),(s1_s6|->{t1_1t2}),(s6_s2|->{t1_6t7,t2_2t3}),
 (s2_s6|->{t1_2t3}),(s7_s2|->{t1_7t8,t2_2t3}),(s6_s3|->{t1_6t7,t2_3t4}),
 (s3_s6|->{t1_3t4,t2_6t7}),(s8_s2|->{t2_2t3}),(s7_s3|->{t1_7t8,t2_3t4}),
 (s6_s4|->{t1_6t7}),(s4_s6|->{t2_6t7}),(s3_s7|->{t1_3t4,t2_7t8}),(s8_s3|->{t2_3t4}),
 (s7_s4|->{t1_7t8}),(s4_s7|->{t2_7t8}),(s3_s8|->{t1_3t4}),(s8_s4|->{t1_8t1,t2_4t5}),
 (s4_s8|->{t1_4t5,t2_8t1}),(s8_s5|->{t1_8t1,t2_5t6}),(s5_s8|->{t1_5t6,t2_8t1}),
 (s8_s6|->{t1_8t1}),(s6_s8|->{t2_8t1})}
*/
