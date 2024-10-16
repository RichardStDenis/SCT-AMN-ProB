// Richard St-Denis, Universite de Sherbrooke, 2023.
MODEL test_sbc_trains_Po_SMC_i

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
//Qpred = Qpred1
//Qpred = Qpred2
  Qpred = {s6_s1,s1_s6,s6_s2,s2_s6,s7_s2,s2_s7,s8_s2,s7_s3,s3_s7,s2_s8,
           s8_s3,s7_s4,s4_s7,s3_s8,s8_s4,s4_s8,s8_s5,s5_s8,s8_s6,s6_s8 }

VARIABLES
 code,
 supWCP,           // supremal controllable predicate stronger than Q
 sfbc_d_to,        // SFBC function for the set of disabled events when total observation
 sfbc_e_to,        // SFBC function for the set of enabled events when total observation
 supSMCP,          // supremal strong M-controllable predicate stronger than Q (iterative solution)
 sfbc_d,           // SFBC function for the set of disabled events when partial observation
 sfbc_e,           // SFBC function for the set of enabled events when partial observation
 sfbc_d_to2po,     // SFBC function for the set of disabled events from that of total observation
 sfbc_e_to2po,     // SFBC function for the set of enabled events from that of total observation
 tf_loop           // transition function of the closed-loop system

INITIALISATION
 code := 0;
 supWCP := CtrlField(G_FB,Qpred); 
 sfbc_d_to := f_d(G_FB,supWCP);
 sfbc_e_to := f_a(G_FB,supWCP);
 supSMCP := {};
 sfbc_d := {};
 sfbc_e :=  {};
 sfbc_d_to2po := {};
 sfbc_e_to2po := {};
 tf_loop := {}

INVARIANT
 code >= 0 & code <= 11 &
 supWCP <: Qpred &
 sfbc_e_to : SFBCf(G_FB'states,G_FB'ctrlEvents) &
 sfbc_d_to : SFBCf(G_FB'states,G_FB'ctrlEvents) &
 supSMCP <: Qpred &
 sfbc_d : SFBCf(G_FB'states,G_FB'ctrlEvents) &
 sfbc_e : SFBCf(G_FB'states,G_FB'events) &
 sfbc_d_to2po : SFBCf(G_FB'states,G_FB'ctrlEvents) &
 sfbc_e_to2po : SFBCf(G_FB'states,G_FB'ctrlEvents) &
 tf_loop <: G_FB'tf

OPERATIONS
 DisplayStatistics = PRE code = 0 THEN
  PRINT("Display statistics...");
  FPRINTF(output_file_name,"~w sections, ~w trains: ",[card(LocalStates),card(G_FB'events)/card(LocalStates)]);
  FPRINTF(output_file_name,"~w states and ",[card(G_FB'states)]);
  FPRINTF(output_file_name,"~w transitions~n~n",[card(G_FB'tf)]);
  FPRINTF(output_file_name,"Predicate (~w states):~n",[card(Qpred)]);
  code := 1 END;

ComputesupSMCP = PRE code = 1 THEN
  PRINT("Compute iteratively the supremal strong M-controllable predicate...");
  VAR i, Q_i, Q_succ_i IN
   i := 1;
   Q_i := {};   Q_succ_i := Qpred;
   WHILE Q_i /= Q_succ_i DO
    FPRINTF(output_file_name," iteration number ~w~n",[i]);
    i:= i+1;
    Q_i := Q_succ_i;
    Q_succ_i := iStepSupSMCP(G_FB,tunnel,Qpred,Q_i)
   INVARIANT card(Qpred) >= 0 VARIANT card(Q_succ_i) END; 
   supSMCP := Q_i;
   FPRINTF(output_file_name,"Iterative solution~nsupSMCP (~w reachable states):~n",[card(supSMCP)]);
   FPRINTF(output_file_name,"~w~n~n",[supSMCP]);
   IF Nonblocking(G_FB,supSMCP) THEN
    PRINT("The supremal predicate is nonblocking.") ELSE
    PRINT("The supremal predicate is blocking.") END;
   IF Coreachable(G_FB,supSMCP) THEN
    PRINT("The maximal predicate is coreachable.") ELSE
    PRINT("The maximal predicate is coreachable.") END;
   code := 2 END END;

 ComputeSFBCe = PRE code = 2 THEN
  PRINT("Compute SFBC (enabled events partial observation)...");
  sfbc_e := supSMCP<|f_hat(G_FB,tunnel,supWCP);
  FPRINTF(output_file_name,"Enabled events (partiel observation): ~w~n",[sfbc_e]);
  LET Gf BE Gf = InducedAutomaton(G_FB,sfbc_e,supSMCP) IN
   FPRINTF(output_file_name,"Automaton of the closed-loop system: ~w~n",[Gf]);
   IF Nonblocking(Gf,supSMCP) THEN
    PRINT("The closed-loop system is nonblocking.") ELSE
    PRINT("The closed-loop system is blocking.") END;
    FPRINTF(output_file_name,"Coreachable states: ~w~n",[CR(Gf,supSMCP)]) END;
  code := 3 END;

 ComputeSFBCd = PRE code = 3 THEN
  PRINT("Compute SFBC (disabled events partial observation)...");
  sfbc_d := supSMCP<|SFBC_hat(G_FB,tunnel,supWCP);
  FPRINTF(output_file_name,"Disabled events (partiel observation): ~w~n",[sfbc_d]);
  code := 4 END;

 CheckComplementarity = PRE code = 4 &
  !x_.(x_ : supSMCP => sfbc_d(x_) /\ sfbc_e(x_) = {}) THEN
  PRINT("The SFBC functions are complementary.");
  code := 5 END;

 ComputeSFBCdPoFromTo = PRE code = 5 THEN
  PRINT("Compute SFBC (disabled events from those of total observation)...");
  sfbc_d_to2po := supSMCP<|f_hat_d(G_FB,tunnel,sfbc_d_to);
  FPRINTF(output_file_name,"Disabled events (from those of total observation): ~w~n",[sfbc_d_to2po]);
  code := 6 END;

 CheckConsistency = PRE code = 6 & sfbc_d_to2po = sfbc_d THEN
  PRINT("The SFBC functions are consistent.");
  code := 7 END;

 ComputeSFBCePoFromTo = PRE code = 7 THEN
  PRINT("Compute SFBC (enabled events from those of total observation)...");
  sfbc_e_to2po := supSMCP<|f_hat_a(G_FB,tunnel,sfbc_e_to);
  FPRINTF(output_file_name,"Enabled events (from those of total observation): ~w~n",[sfbc_e_to2po]);
  code := 8 END;

 CheckConsistency_ = PRE code = 8 &
  !x_.(x_ : supSMCP => sfbc_e(x_) <: sfbc_e_to2po(x_)) THEN
   PRINT("The SFBC functions are consistent.");
   code := 9 END;

 CheckComplementarity_ = PRE code = 9 &
  !x_.(x_ : supSMCP => sfbc_d_to2po (x_) /\ sfbc_e_to2po(x_) = {}) THEN
   PRINT("The SFBC functions are complementary.");
   code := 10 END;

 CheckFo = PRE code = 10 & F_o(G_FB,supSMCP,tunnel,sfbc_d_to2po) &
  F_o(G_FB,supSMCP,tunnel,sfbc_e_to2po) THEN
   PRINT("The SFBC functions satisfy F_0.");
   code := 11 END END

/*
OUTPUT:
"Display statistics..."
cover(DisplayStatistics)
"Compute iteratively the supremal strong M-controllable predicate..."
"The supremal predicate is nonblocking."
"The maximal predicate is coreachable."
cover(ComputesupSMCP)
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

Predicate (20 states):
 iteration number 1
 iteration number 2
Iterative solution
supSMCP (0 reachable states):
{}

Enabled events (partiel observation): {}
Automaton of the closed-loop system:
rec(Xm:{},
    ctrlEvents:{t1_2t3,t1_4t5,t1_6t7,t1_8t1,t2_2t3,t2_4t5,t2_6t7,t2_8t1},
    events:{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,t2_1t2,t2_2t3,
               t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1},
    states:{},
    tf:{},
    x_0:s2_s4)
Coreachable states: {}
Disabled events (partiel observation): {}
Disabled events (from those of total observation): {}
Enabled events (from those of total observation): {}
*/