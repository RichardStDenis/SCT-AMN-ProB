// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL test_composite_component_LB_handling

SEES
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------

  LBhandling /*@file "./LBhandling.mch" */

CONSTANTS
 T_G_ts,           // Traces of the transition system
 L_G_ts, Lm_G_ts,  // Generated and marked languages of the transition system
 T_agent,          // Traces of the agent
 L_G_A, Lm_G_A,    // Generated and marked languages of the agent
 T_interface,      // Traces of the interface
 L_G_I, Lm_G_I,    // Generated and marked languages of the interface

 input_map         // a map from the set of traces of a Mealy machine to its set of input strings

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
 "../../Theories/Mealy_basis.def";
 "../../Theories/SCT_core.def";
 "../../Theories/SCT_hierarchical.def";
 "LibraryIO.def"

PROPERTIES
 G_ts : Generator_C(G_ts'states,G_ts'events) &
 G_A : Mealy_machine(G_A'states,G_A'inputs, G_A'outputs) &
 DeterministicMealy_machine(G_A) &
 G_I : Generator_C(G_I'states,G_I'events) &
 G_A'states = G_ts'states & G_A'inputs = G_ts'events &
 GetInTransition_Mealy(G_A,G_A'inputs,G_A'outputs,G_A'states) = G_ts'tf &
 G_A'x_0 = G_ts'x_0 & G_A'Xm = G_ts'Xm &
 G_A'ctrlEvents = G_ts'ctrlEvents &

 T_G_ts = GetTraces(G_ts,bound_G_ts) &
 L_G_ts = Traces2Strings(T_G_ts) &
 Lm_G_ts = Traces2Tasks(T_G_ts,G_ts'Xm) &
 T_agent = GetTraces_Mealy(G_A,bound_G_A) &
 L_G_A = Traces2Strings_i(T_agent,Input,Output) &
 Lm_G_A = Traces2Tasks_i(T_agent,G_A'Xm,Input,Output) &
 T_interface = GetTraces(G_I,bound_G_I) &
 L_G_I = Traces2Strings(T_interface) &
 Lm_G_I = Traces2Tasks(T_interface,G_I'Xm) &

 input_map : InputMap(G_A'states,G_A'inputs,G_A'outputs) &
 input_map = GetInputMap(T_agent,G_A'states,G_A'inputs,G_A'outputs)

VARIABLES
 code,
 theta   // A causal map

INVARIANT
 code >= 0 & code <= 5 &
 theta : CausalMap(G_A'inputs,G_I'events)

INITIALISATION
 code := 0;
 theta := GetCausalMap(T_agent,Input,Output,VocalOutput)

OPERATIONS
 DisplayStatistics = PRE code = 0 THEN
  PRINTF("Number of states of the transition system ",[card(G_ts'states)]);
  PRINTF("Number of transitions of the transition system: ",[card(G_ts'tf)]);
  PRINTF("Size of the prefix-closed language of the transition system: ",[card(L_G_ts)]);
  PRINTF("Size of the marked language of the transition system: ",[card(Lm_G_ts)]);
  PRINTF("Number of states of the agent: ",[card(G_A'states)]);
  PRINTF("Number of transitions of the agent: ",[card(G_A'tf)]);
  PRINTF("Size of the prefix-closed language of the agent: ",[card(L_G_A)]);
  PRINTF("Size of the marked language of the agent: ",[card(Lm_G_A)]);
  PRINTF("Number of states of the interface: ",[card(G_I'states)]);
  PRINTF("Number of transitions the interface: ",[card(G_I'tf)]);
  PRINTF("Size of the prefix-closed language of the interface: ",[card(L_G_I)]);
  PRINTF("Size of the marked language of the interface: ",[card(Lm_G_I)]);
  code := 1 END;

 Check_WeakConsistencyOfMarking = PRE code = 1 THEN
  IF ConsistencyOfMarking(L_G_A,Lm_G_A,Lm_G_I,theta) THEN
   PRINT("Marking is consistent.") ELSE
   IF WeakConsistencyOfMarking(G_A,bound_G_A,T_agent,Lm_G_A,Lm_G_I,theta,input_map) THEN
    PRINT("Marking is weakly consistent.") ELSE
     IF EventuallyReachMkState(G_A,T_agent,Lm_G_I,theta,input_map) THEN
        PRINT("Eventually reach a marking state.") ELSE
     PRINT("Marking is not (weakly) consistent.") END END END;
  code := 2 END;

 Check_ObserverProperty = PRE code = 2 &
  Observer(L_G_A,L_G_I,G_I'events,theta) THEN
   code := 3; PRINT("The causal map is an observer.") END;

 Check_ConditionOnSCT = PRE code = 3 &
  ControlCoincidence(L_G_A,G_A'inputs,G_A'ctrlEvents,G_I'ctrlEvents,theta) THEN
   code := 4; PRINT("Control coincidence property is satisfied.") END;

 Check_NonBlocking_Interface = PRE code = 4 &
  NonBlocking(L_G_I,Lm_G_I) THEN
   code := 5; PRINT("The interface is nonblocking.") END END

/*
OUTPUT:
% Runtime for SOLUTION for SETUP_CONSTANTS: 1500 ms (walltime: 1640 ms)
Finished Invariant Checking 10 ms walltime (10 ms runtime), since start: 6120 ms
Number of states of the transition system 44
Number of transitions of the transition system: 80
Size of the prefix-closed language of the transition system: 845
Size of the marked language of the transition system: 61
Number of states of the agent: 44
Number of transitions of the agent: 80
Size of the prefix-closed language of the agent: 845
Size of the marked language of the agent: 61
Number of states of the interface: 2
Number of transitions the interface: 2
Size of the prefix-closed language of the interface: 7
Size of the marked language of the interface: 4
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 6140 ms
"Eventually reach a marking state."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 6390 ms
"The causal map is an observer."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 23780 ms
"Control coincidence property is satisfied."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 23970 ms
"The interface is nonblocking."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 23980 ms
Deadlock reached after 7 steps (after Check_NonBlocking_Interface).

% Runtime: 19870 ms (total: 19870 ms, walltime: 21920 ms)
% Finished execute (errors=0, warnings=0)
% Finished processing file after 21990 ms walltime (19930 ms runtime), since start: 24040 ms

% Finished process_file (errors=0, warnings=0)

*/