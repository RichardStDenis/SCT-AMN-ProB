// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL test_elementary_component_LB_loop

SEES
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------
  jack111ce /*@file "./jack111ce.mch" */
//jack111cr /*@file "./jack111cr.mch" */
//jack111cecr /*@file "./jack111cecr.mch" */
//relay_ad_cc /*@file "./relay_ad_cc.mch" */
//relay_ad_co /*@file "./relay_ad_co.mch" */
//relay_ad_ccco /*@file "./relay_ad_ccco.mch" */
//timer_od_e /*@file "./timer_od_e.mch" */

CONSTANTS
 T_G_FB,           // Traces of the device exhaustive behavior
 L_G_FB, Lm_G_FB,  // Prefix-closed and marked languages of the device behavior
 T_G_ts,           // Traces of the transition systems
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
 G_FB : Generator_C(G_FB'states,G_FB'events) &
 G_ts : Generator_C(G_ts'states,G_ts'events) &
 G_A : Mealy_machine(G_A'states,G_A'inputs, G_A'outputs) &
 G_I : Generator_C(G_I'states,G_I'events) &
 G_FB'states - {illegal} = G_A'states & G_FB'events = G_A'inputs & 
 G_FB'x_0 = G_A'x_0 & G_FB'Xm = G_A'Xm &
 G_FB'ctrlEvents = G_A'ctrlEvents &
 G_A'states = G_ts'states & G_A'inputs = G_ts'events &
 GetInTransition_Mealy(G_A,G_A'inputs,G_A'outputs,G_A'states) = G_ts'tf &
 G_A'x_0 = G_ts'x_0 & G_A'Xm = G_ts'Xm &
 G_A'ctrlEvents = G_ts'ctrlEvents &

 T_G_FB = GetTracesNc1stNqcSevSst(G_FB,bound_G_ts,agentEventsOnLoops,{illegal}) &
 L_G_FB = Traces2Strings(T_G_FB) &
 Lm_G_FB = Traces2Tasks(T_G_FB,G_ts'Xm) &
 T_G_ts = GetTracesNc1stNqcSev(G_ts,bound_G_ts,agentEventsOnLoops) & L_G_ts = Traces2Strings(T_G_ts) &
 Lm_G_ts = Traces2Tasks(T_G_ts,G_ts'Xm) &
 T_agent =  GetTracesNcSio_Mealy(G_A,bound_G_A,agentIOonLoops) &
 L_G_A = Traces2Strings_i(T_agent,G_A'inputs,G_A'outputs) &
 Lm_G_A = Traces2Tasks_i(T_agent,G_A'Xm,G_A'inputs,G_A'outputs) &
 T_interface = GetTracesNc1stNcSev(G_I,bound_G_I,interfaceEventsOnLoops) &
 L_G_I = Traces2Strings(T_interface) &
 Lm_G_I = Traces2Tasks(T_interface,G_I'Xm) &

 input_map : InputMap(G_A'states,G_A'inputs,G_A'outputs) &
 input_map = GetInputMap(T_agent,G_A'states,G_A'inputs,G_A'outputs)

VARIABLES
 code,        // Sequencing code
 theta,       // A causal map
 phi_d,       // State realization of the supervisor for the set of disabled events at any state x
 phi_e        // State realization of the supervisor for the set of enabled events at any state x

INVARIANT
 code >= 0 & code <= 9 &
 theta : CausalMap(G_A'inputs,G_I'events) &
 phi_d : SRsupervisor_(G_FB'states,G_FB'events) &
 phi_e : SRsupervisor_(G_FB'states,G_FB'events)

INITIALISATION
 code := 0;
 theta := GetCausalMap(T_agent,Input,Output,VocalOutput);
 phi_d := {}; phi_e := {}

OPERATIONS

 DisplayStatistics = PRE code = 0 THEN
  PRINTF("Size of the prefix-closed language of the exhaustive behavior: ", [card(L_G_FB)]);
  PRINTF("Size of the marked language of the exhaustive behavior: ", [card(Lm_G_FB)]);
  PRINTF("Number of states of the agent: ", [card(G_A'states)]);
  PRINTF("Number of transitions of the agent: ", [card(G_A'tf)]);
  PRINTF("Size of the prefix-closed language of the agent: ", [card(L_G_A)]);
  PRINTF("Size of the marked language of the agent: ", [card(Lm_G_A)]);
  PRINTF("Number of states of the interface: ", [card(G_I'states)]);
  PRINTF("Number of transitions the interface: ", [card(G_I'tf)]);
  PRINTF("Size of the prefix-closed language of the interface: ", [card(L_G_I)]);
  PRINTF("Size of the marked language of the interface: ", [card(Lm_G_I)]);
  code := 1 END;

 Check_Controllability = PRE code = 1 &
  Controllability(L_G_ts,L_G_FB,G_FB'events-G_FB'ctrlEvents) THEN
   code := 2; PRINT("The device (agent) is controllable.") END;

 Check_Lm_closed = PRE code = 2 &
  Relative_closure(L_G_ts,Lm_G_ts,Lm_G_FB) THEN
   code := 3; PRINT("The device (agent) is Lm_closed.") END;

 Display_supervisor = PRE code = 3 THEN
  code := 4; PRINTF("The supervisor (relation): ", [supervisor_e_map_(T_G_FB,L_G_ts)]) END;

 Display_supervisors = PRE code = 4 THEN
    phi_d := SRsupervisorDISBL_(T_G_FB,L_G_ts,L_G_FB,G_ts'ctrlEvents);
    phi_e := SRsupervisorENBL_(T_G_ts,L_G_A,G_A'inputs);
    PRINTF("State realization of the supervisor (disabled events): ", [phi_d]);
    PRINTF("State realization of a supervisor (enabled events): ", [phi_e]);
  code := 5 END;

 Check_WeakConsistencyOfMarking = PRE code = 5 THEN
  IF ConsistencyOfMarking(L_G_A,Lm_G_A,Lm_G_I,theta) THEN
   PRINT("Marking is consistent.") ELSE
   IF WeakConsistencyOfMarking(G_A,bound_G_A,T_agent,Lm_G_A,Lm_G_I,theta,input_map) THEN 
    PRINT("Marking is weakly consistent.") END END;
  code := 6 END;

 Check_ObserverProperty = PRE code = 6 &
  Observer(L_G_A,L_G_I,G_I'events,theta) THEN
   code := 7; PRINT("The causal map is an observer.") END;

 Check_ConditionOnSCT = PRE code = 7 &
  ControlCoincidence(L_G_A,G_A'inputs,G_A'ctrlEvents,G_I'ctrlEvents,theta) THEN
   code := 8; PRINT("Control coincidence property is satisfied.") END;

 Check_NonBlocking_Interface = PRE code = 8 &
  NonBlocking(L_G_I,Lm_G_I) THEN
   code := 9; PRINT("The interface is nonblocking.") END END