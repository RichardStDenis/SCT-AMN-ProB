// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL test_composite_component_LB_nc1st

SEES
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------
  LBconveyor_ts /*@file "./LBconveyor_ts.mch" */
//LBcrane_br /*@file "./LBcrane_br.mch" */
//LBcrane_br_w /*@file "./LBcrane_br_w.mch" */
//LBcrane_lr /*@file "./LBcrane_lr.mch" */
//LBcrane_lr_all /*@file "./LBcrane_lr_all.mch" */
//LBcrane_lr2 /*@file "./LBcrane_lr2.mch" */
//LBcrane_lr2_f /*@file "./LBcrane_lr2_f.mch" */
//LBcrane_lr2_w /*@file "./LBcrane_lr2_w.mch" */
//LBcrane_lr2_f_w /*@file "./LBcrane_lr2_f_w.mch" */
//LBdrill /*@file "./LBdrill.mch" */
//LBhook_e /*@file "./LBhook_e.mch" */
//LBhook_ef /*@file "./LBhook_ef.mch" */
//LBmicrometer /*@file "./LBmicrometer.mch" */
//LBservomotor /*@file "./LBservomotor.mch" */
//LBtester  /*@file "./LBtester.mch" */

//LBdistribution_1 /*@file "./LBdistribution_1.mch" */
//LBtesting /*@file "./LBtesting.mch" */

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

 T_G_ts = GetTracesNc1st(G_ts,bound_G_ts) &
 L_G_ts = Traces2Strings(T_G_ts) &
 Lm_G_ts = Traces2Tasks(T_G_ts,G_ts'Xm) &
 T_agent = GetTracesNc1st_Mealy(G_A,bound_G_A) &
 L_G_A = Traces2Strings_i(T_agent,Input,Output) &
 Lm_G_A = Traces2Tasks_i(T_agent,G_A'Xm,Input,Output) &
 T_interface = GetTracesNc1st(G_I,bound_G_I) &
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
    PRINT("Marking is not (weakly) consistent.") END END;
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