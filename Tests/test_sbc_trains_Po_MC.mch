// Richard St-Denis, Universite de Sherbrooke, 2023.
MODEL test_sbc_trains_Po_MC

SEES
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------

  tunnel_8_2 /*@file "../Trains/tunnel_8_2.mch" */

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
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../Theories/SBCDES.def";
 "LibraryIO.def";

PROPERTIES
 output_file_name = "~/Documents/SCTBenchmark/output_trains.txt" &

 G_FB : Generator_C(States,Events) &
 tunnel : obsMask(G_FB'states,ObservationSpace) &
 Qpred = Qpred1
 

//Qpred = Qpred2

VARIABLES
 code,
 supWCP,           // supremal controllable predicate stronger than Q
 sfbc_d_to,        // SFBC function for the set of disabled events when total observation
 sfbc_e_to,        // SFBC function for the set of enabled events when total observation
 maxMCP,           // M-controllable predicate stronger than Q
 sfbc_d,           // SFBC function for the set of disabled events when partial observation
 sfbc_e,           // SFBC function for the set of enabled events when partial observation
 sfbc_d_to2po,     // SFBC function for the set of disabled events from that of total observation
 sfbc_e_to2po      // SFBC function for the set of enabled events from that of total observation

INITIALISATION
 code := 0;
 supWCP := CtrlField(G_FB,Qpred); 
 sfbc_d_to := f_d(G_FB,supWCP);
 sfbc_e_to := f_a(G_FB,supWCP);
 maxMCP := {};
 sfbc_d := {};
 sfbc_e :=  {};
 sfbc_d_to2po := {};
 sfbc_e_to2po := {}

INVARIANT
 code >= 0 & code <= 10 &
 supWCP <: Qpred &
 sfbc_e_to : SFBCf(G_FB'states,G_FB'ctrlEvents) &
 sfbc_d_to : SFBCf(G_FB'states,G_FB'ctrlEvents) &
 maxMCP <: Qpred &
 sfbc_d : SFBCf(G_FB'states,G_FB'ctrlEvents) &
 sfbc_e : SFBCf(G_FB'states,G_FB'events) &
 sfbc_d_to2po : SFBCf(G_FB'states,G_FB'ctrlEvents) &
 sfbc_e_to2po : SFBCf(G_FB'states,G_FB'ctrlEvents)

OPERATIONS
 DisplayStatistics = PRE code = 0 THEN
  PRINT("Display statistics...");
  FPRINTF(output_file_name,"~w sections, ~w trains: ",[card(LocalStates),card(G_FB'events)/card(LocalStates)]);
  FPRINTF(output_file_name,"~w states and ",[card(G_FB'states)]);
  FPRINTF(output_file_name,"~w transitions~n~n",[card(G_FB'tf)]);
  FPRINTF(output_file_name,"Predicate (~w states):~n",[card(Qpred)]);
  code := 1 END;

ComputeMaxMCP = PRE code = 1 THEN
  PRINT("Compute a M-controllable predicate...");
  maxMCP := ApproxQbyMCpred(G_FB,tunnel,Qpred);
  FPRINTF(output_file_name,"Solution~nmaxMCP (~w reachable states):~n",[card(maxMCP)]);
  FPRINTF(output_file_name,"~w~n~n",[maxMCP]);
  IF Nonblocking(G_FB,maxMCP) THEN
    PRINT("The maximal predicate is nonblocking.") ELSE
    PRINT("The maximal predicate is blocking.") END;
  IF Coreachable(G_FB,maxMCP) THEN
    PRINT("The maximal predicate is coreachable.") ELSE
    PRINT("The maximal predicate is coreachable.") END;
  code := 2 END;

 ComputeSFBCe = PRE code = 2 THEN
  PRINT("Compute SFBC (enabled events partial observation)...");
  sfbc_e := maxMCP<|f_breve(G_FB,tunnel,supWCP);
  FPRINTF(output_file_name,"Enabled events (partiel observation): ~w~n",[sfbc_e]);
  LET Gf BE Gf = InducedAutomaton(G_FB,sfbc_e,maxMCP) IN
   FPRINTF(output_file_name,"Automaton of the closed-loop system: ~w~n",[Gf]);
   IF Nonblocking(Gf,maxMCP) THEN
    PRINT("The closed-loop system is nonblocking.") ELSE
    PRINT("The closed-loop system is blocking.") END;
    FPRINTF(output_file_name,"Coreachable states: ~w~n",[CR(Gf,maxMCP)]) END;
  code := 3 END;

 ComputeSFBCd = PRE code = 3 THEN
  PRINT("Compute SFBC (disabled events partial observation)...");
  sfbc_d := maxMCP<|SFBC_breve(G_FB,tunnel,supWCP);
  FPRINTF(output_file_name,"Disabled events (partiel observation): ~w~n",[sfbc_d]);
  code := 4 END;

 CheckComplementarity = PRE code = 4 &
  !x_.(x_ : maxMCP => sfbc_d(x_) /\ sfbc_e(x_) = {}) THEN
  PRINT("The SFBC functions are complementary.");
  code := 5 END;

 ComputeSFBCdPoFromTo = PRE code = 5 THEN
  PRINT("Compute SFBC (disabled events from those of total observation)...");
  sfbc_d_to2po := maxMCP<|f_breve_d(G_FB,tunnel,supWCP,sfbc_d_to);
  FPRINTF(output_file_name,"Disabled events (from those of total observation): ~w~n",[sfbc_d_to2po]);
  code := 6 END;

 CheckConsistency = PRE code = 6 & sfbc_d_to2po = sfbc_d THEN
  PRINT("The SFBC functions are consistent.");
  code := 7 END;

 ComputeSFBCePoFromTo = PRE code = 7 THEN
  PRINT("Compute SFBC (enabled events from those of total observation)...");
  sfbc_e_to2po := maxMCP<|f_breve_a(G_FB,tunnel,supWCP,sfbc_e_to);
  FPRINTF(output_file_name,"Enabled events (from those of total observation): ~w~n",[sfbc_e_to2po]);
  code := 8 END;

 CheckConsistency_ = PRE code = 8 &
  !x_.(x_ : maxMCP => sfbc_e(x_) <: sfbc_e_to2po(x_)) THEN
   PRINT("The SFBC functions are consistent.");
   code := 9 END;

 CheckComplementarity_ = PRE code = 9 &
  !x_.(x_ : maxMCP => sfbc_d_to2po (x_) /\ sfbc_e_to2po(x_) = {}) THEN
   PRINT("The SFBC functions are complementary.");
   code := 10 END;

 CheckFo = PRE code = 10 & F_o(G_FB,maxMCP,tunnel,sfbc_d_to2po) &
  F_o(G_FB,maxMCP,tunnel,sfbc_e_to2po) THEN
   PRINT("The SFBC functions satisfy F_0.");
   code := 11 END
 END

/*
OUTPUT:
"Display statistics..."
cover(DisplayStatistics)
"Compute a M-controllable predicate..."
"The maximal predicate is nonblocking."
"The maximal predicate is coreachable."
cover(ComputeMaxMCP)
"Compute SFBC (enabled events partial observation)..."
"The closed-loop system is nonblocking."
cover(ComputeSFBCe)
"Compute SFBC (disabled events partial observation)..."
cover(ComputeSFBCd)
"The SFBC functions are complementary."
cover(CheckComplementarity)
"Compute SFBC (disabled events from those of total observation)..."
cover(ComputeSFBCdPoFromTo)
"The SFBC functions are consistent."
cover(CheckConsistency)
"Compute SFBC (enabled events from those of total observation)..."
cover(ComputeSFBCePoFromTo)
"The SFBC functions are consistent."
cover(CheckConsistency_)
"The SFBC functions are complementary."
cover(CheckComplementarity_)
"The SFBC functions satisfy F_0."
cover(CheckFo)

ALL OPERATIONS COVERED

8 sections, 2 trains: 64 states and 128 transitions

Predicate (56 states):
Solution
maxMCP (44 reachable states):
{s3_s1,s1_s3,s4_s1,s3_s2,s2_s3,s1_s4,s5_s1,s4_s2,s2_s4,s1_s5,s6_s1,s5_s2,s2_s5,
 s1_s6,s7_s1,s6_s2,s2_s6,s1_s7,s8_s1,s7_s2,s6_s3,s3_s6,s2_s7,s1_s8,s8_s2,s7_s3,
 s6_s4,s4_s6,s3_s7,s2_s8,s8_s3,s7_s4,s4_s7,s3_s8,s8_s4,s7_s5,s5_s7,s4_s8,s8_s5,
 s7_s6,s6_s7,s5_s8,s8_s6,s6_s8}

Enabled events (partiel observation):
{(s3_s1|->{t1_3t4,t2_1t2}),(s1_s3|->{t1_1t2,t2_3t4}),(s4_s1|->{t1_4t5,t2_1t2}),(s3_s2|->{t1_3t4}),
 (s2_s3|->{t2_3t4}),(s1_s4|->{t1_1t2,t2_4t5}),(s5_s1|->{t1_5t6,t2_1t2}),(s4_s2|->{t1_4t5}),
 (s2_s4|->{t2_4t5}),(s1_s5|->{t1_1t2,t2_5t6}),(s6_s1|->{t1_6t7,t2_1t2}),(s5_s2|->{t1_5t6}),
 (s2_s5|->{t2_5t6}),(s1_s6|->{t1_1t2,t2_6t7}),(s7_s1|->{t1_7t8,t2_1t2}),(s6_s2|->{t1_6t7,t2_2t3}),
 (s2_s6|->{t1_2t3,t2_6t7}),(s1_s7|->{t1_1t2,t2_7t8}),(s8_s1|->{t2_1t2}),(s7_s2|->{t1_7t8,t2_2t3}),
 (s6_s3|->{t1_6t7,t2_3t4}),(s3_s6|->{t1_3t4,t2_6t7}),(s2_s7|->{t1_2t3,t2_7t8}),(s1_s8|->{t1_1t2}),
 (s8_s2|->{t2_2t3}),(s7_s3|->{t1_7t8,t2_3t4}),(s6_s4|->{t1_6t7}),(s4_s6|->{t2_6t7}),
 (s3_s7|->{t1_3t4,t2_7t8}),(s2_s8|->{t1_2t3}),(s8_s3|->{t1_8t1,t2_3t4}),(s7_s4|->{t1_7t8,t2_4t5}),
 (s4_s7|->{t1_4t5,t2_7t8}),(s3_s8|->{t1_3t4,t2_8t1}),(s8_s4|->{t1_8t1,t2_4t5}),(s7_s5|->{t1_7t8,t2_5t6}),
 (s5_s7|->{t1_5t6,t2_7t8}),(s4_s8|->{t1_4t5,t2_8t1}),(s8_s5|->{t1_8t1,t2_5t6}),(s7_s6|->{t1_7t8}),
 (s6_s7|->{t2_7t8}),(s5_s8|->{t1_5t6,t2_8t1}),(s8_s6|->{t1_8t1}),(s6_s8|->{t2_8t1})}
Automaton of the closed-loop system: 
rec(Xm:{s2_s4},
    ctrlEvents:{t1_2t3,t1_4t5,t1_6t7,t1_8t1,t2_2t3,t2_4t5,t2_6t7,t2_8t1},
    events:{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,
            t2_6t7,t2_7t8,t2_8t1},
    states:{s3_s1,s1_s3,s4_s1,s3_s2,s2_s3,s1_s4,s5_s1,s4_s2,s2_s4,s1_s5,s6_s1,s5_s2,s2_s5,s1_s6,s7_s1,
            s6_s2,s2_s6,s1_s7,s8_s1,s7_s2,s6_s3,s3_s6,s2_s7,s1_s8,s8_s2,s7_s3,s6_s4,s4_s6,s3_s7,s2_s8,
            s8_s3,s7_s4,s4_s7,s3_s8,s8_s4,s7_s5,s5_s7,s4_s8,s8_s5,s7_s6,s6_s7,s5_s8,s8_s6,s6_s8},
    tf:{((s3_s1|->t1_3t4)|->s4_s1),((s3_s1|->t2_1t2)|->s3_s2),((s1_s3|->t1_1t2)|->s2_s3),
        ((s1_s3|->t2_3t4)|->s1_s4),((s4_s1|->t1_4t5)|->s5_s1),((s4_s1|->t2_1t2)|->s4_s2),
        ((s3_s2|->t1_3t4)|->s4_s2),((s2_s3|->t2_3t4)|->s2_s4),((s1_s4|->t1_1t2)|->s2_s4),
        ((s1_s4|->t2_4t5)|->s1_s5),((s5_s1|->t1_5t6)|->s6_s1),((s5_s1|->t2_1t2)|->s5_s2),
        ((s4_s2|->t1_4t5)|->s5_s2),((s2_s4|->t2_4t5)|->s2_s5),((s1_s5|->t1_1t2)|->s2_s5),
        ((s1_s5|->t2_5t6)|->s1_s6),((s6_s1|->t1_6t7)|->s7_s1),((s6_s1|->t2_1t2)|->s6_s2),
        ((s5_s2|->t1_5t6)|->s6_s2),((s2_s5|->t2_5t6)|->s2_s6),((s1_s6|->t1_1t2)|->s2_s6),
        ((s1_s6|->t2_6t7)|->s1_s7),((s7_s1|->t1_7t8)|->s8_s1),((s7_s1|->t2_1t2)|->s7_s2),
        ((s6_s2|->t1_6t7)|->s7_s2),((s6_s2|->t2_2t3)|->s6_s3),((s2_s6|->t1_2t3)|->s3_s6),
        ((s2_s6|->t2_6t7)|->s2_s7),((s1_s7|->t1_1t2)|->s2_s7),((s1_s7|->t2_7t8)|->s1_s8),
        ((s8_s1|->t2_1t2)|->s8_s2),((s7_s2|->t1_7t8)|->s8_s2),((s7_s2|->t2_2t3)|->s7_s3),
        ((s6_s3|->t1_6t7)|->s7_s3),((s6_s3|->t2_3t4)|->s6_s4),((s3_s6|->t1_3t4)|->s4_s6),
        ((s3_s6|->t2_6t7)|->s3_s7),((s2_s7|->t1_2t3)|->s3_s7),((s2_s7|->t2_7t8)|->s2_s8),
        ((s1_s8|->t1_1t2)|->s2_s8),((s8_s2|->t2_2t3)|->s8_s3),((s7_s3|->t1_7t8)|->s8_s3),
        ((s7_s3|->t2_3t4)|->s7_s4),((s6_s4|->t1_6t7)|->s7_s4),((s4_s6|->t2_6t7)|->s4_s7),
        ((s3_s7|->t1_3t4)|->s4_s7),((s3_s7|->t2_7t8)|->s3_s8),((s2_s8|->t1_2t3)|->s3_s8),
        ((s8_s3|->t1_8t1)|->s1_s3),((s8_s3|->t2_3t4)|->s8_s4),((s7_s4|->t1_7t8)|->s8_s4),
        ((s7_s4|->t2_4t5)|->s7_s5),((s4_s7|->t1_4t5)|->s5_s7),((s4_s7|->t2_7t8)|->s4_s8),
        ((s3_s8|->t1_3t4)|->s4_s8),((s3_s8|->t2_8t1)|->s3_s1),((s8_s4|->t1_8t1)|->s1_s4),
        ((s8_s4|->t2_4t5)|->s8_s5),((s7_s5|->t1_7t8)|->s8_s5),((s7_s5|->t2_5t6)|->s7_s6),
        ((s5_s7|->t1_5t6)|->s6_s7),((s5_s7|->t2_7t8)|->s5_s8),((s4_s8|->t1_4t5)|->s5_s8),
        ((s4_s8|->t2_8t1)|->s4_s1),((s8_s5|->t1_8t1)|->s1_s5),((s8_s5|->t2_5t6)|->s8_s6),
        ((s7_s6|->t1_7t8)|->s8_s6),((s6_s7|->t2_7t8)|->s6_s8),((s5_s8|->t1_5t6)|->s6_s8),
        ((s5_s8|->t2_8t1)|->s5_s1),((s8_s6|->t1_8t1)|->s1_s6),((s6_s8|->t2_8t1)|->s6_s1)},
    x_0:s2_s4)
Coreachable states:
{s3_s1,s1_s3,s4_s1,s3_s2,s2_s3,s1_s4,s5_s1,s4_s2,s2_s4,s1_s5,s6_s1,s5_s2,s2_s5,s1_s6,s7_s1,
 s6_s2,s2_s6,s1_s7,s8_s1,s7_s2,s6_s3,s3_s6,s2_s7,s1_s8,s8_s2,s7_s3,s6_s4,s4_s6,s3_s7,s2_s8,
 s8_s3,s7_s4,s4_s7,s3_s8,s8_s4,s7_s5,s5_s7,s4_s8,s8_s5,s7_s6,s6_s7,s5_s8,s8_s6,s6_s8}
Disabled events (partiel observation):
{(s3_s1|->{}),(s1_s3|->{}),(s4_s1|->{}),(s3_s2|->{t2_2t3}),(s2_s3|->{t1_2t3}),(s1_s4|->{}),
 (s5_s1|->{}),(s4_s2|->{t2_2t3}),(s2_s4|->{t1_2t3}),(s1_s5|->{}),(s6_s1|->{}),(s5_s2|->{t2_2t3}),
 (s2_s5|->{t1_2t3}),(s1_s6|->{}),(s7_s1|->{}),(s6_s2|->{}),(s2_s6|->{}),(s1_s7|->{}),
 (s8_s1|->{t1_8t1}),(s7_s2|->{}),(s6_s3|->{t2_4t5}),(s3_s6|->{t1_4t5}),(s2_s7|->{}),
 (s1_s8|->{t2_8t1}),(s8_s2|->{t1_8t1}),(s7_s3|->{}),(s6_s4|->{t2_4t5}),(s4_s6|->{t1_4t5}),
 (s3_s7|->{}),(s2_s8|->{t2_8t1}),(s8_s3|->{}),(s7_s4|->{}),(s4_s7|->{}),(s3_s8|->{}),
 (s8_s4|->{}),(s7_s5|->{}),(s5_s7|->{}),(s4_s8|->{}),(s8_s5|->{}),(s7_s6|->{t2_6t7}),
 (s6_s7|->{t1_6t7}),(s5_s8|->{}),(s8_s6|->{t2_6t7}),(s6_s8|->{t1_6t7})}
Disabled events (from those of total observation):
{(s3_s1|->{}),(s1_s3|->{}),(s4_s1|->{}),(s3_s2|->{t2_2t3}),(s2_s3|->{t1_2t3}),(s1_s4|->{}),
 (s5_s1|->{}),(s4_s2|->{t2_2t3}),(s2_s4|->{t1_2t3}),(s1_s5|->{}),(s6_s1|->{}),(s5_s2|->{t2_2t3}),
 (s2_s5|->{t1_2t3}),(s1_s6|->{}),(s7_s1|->{}),(s6_s2|->{}),(s2_s6|->{}),(s1_s7|->{}),
 (s8_s1|->{t1_8t1}),(s7_s2|->{}),(s6_s3|->{t2_4t5}),(s3_s6|->{t1_4t5}),(s2_s7|->{}),
 (s1_s8|->{t2_8t1}),(s8_s2|->{t1_8t1}),(s7_s3|->{}),(s6_s4|->{t2_4t5}),(s4_s6|->{t1_4t5}),
 (s3_s7|->{}),(s2_s8|->{t2_8t1}),(s8_s3|->{}),(s7_s4|->{}),(s4_s7|->{}),(s3_s8|->{}),
 (s8_s4|->{}),(s7_s5|->{}),(s5_s7|->{}),(s4_s8|->{}),(s8_s5|->{}),(s7_s6|->{t2_6t7}),
 (s6_s7|->{t1_6t7}),(s5_s8|->{}),(s8_s6|->{t2_6t7}),(s6_s8|->{t1_6t7})}
Enabled events (from those of total observation):
{(s3_s1|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s1_s3|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s4_s1|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s3_s2|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s2_s3|->{t1_1t2,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s1_s4|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s5_s1|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s4_s2|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s2_s4|->{t1_1t2,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s1_s5|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s6_s1|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s5_s2|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s2_s5|->{t1_1t2,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s1_s6|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s7_s1|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s6_s2|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s2_s6|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s1_s7|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s8_s1|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s7_s2|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s6_s3|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s3_s6|->{t1_1t2,t1_2t3,t1_3t4,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s2_s7|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s1_s8|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8}),
 (s8_s2|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s7_s3|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s6_s4|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s4_s6|->{t1_1t2,t1_2t3,t1_3t4,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s3_s7|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s2_s8|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8}),
 (s8_s3|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s7_s4|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s4_s7|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s3_s8|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s8_s4|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s7_s5|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s5_s7|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s4_s8|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s8_s5|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s7_s6|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_7t8,t2_8t1}),
 (s6_s7|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s5_s8|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1}),
 (s8_s6|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_7t8,t2_8t1}),
 (s6_s8|->{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_7t8,t1_8t1,t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1})}
*/