// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL section6_ex7_2

SETS
 StatesG_A = {x0, x1, x2, x3, x4, x5, x6};
 StatesG_I = {im, nm};

 Input = {fY0, rY0, fX0, rX0, fX1, rX1};
 Output = {start, stop, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 G_A,         // Mealy machine modeling the device implementation (agent)
 bound_G_A,   // Bound on the trace length of the agent
 G_I,         // (quotient deterministic) automaton modeling a device abstraction (interface)
 bound_G_I,   // Bound on the trace length of the interface

 T_agent,          // Traces of the agent
 L_G_A, Lm_G_A,    // Generated and marked languages of the agent
 T_interface,      // Traces of the interface
 L_G_I, Lm_G_I,    // Generated and marked languages of the interface

 input_map         // a map from the set of traces of a Mealy machine to its set of input string

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
 "../Theories/Mealy_basis.def";
 "../Theories/SCT_hierarchical.def";
 "../Theories/SCT_core.def";
 "LibraryIO.def"

PROPERTIES
 VocalOutput = Output - {silentEvent}
 &
 G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,
           tf: {(x0,(fX0,silentEvent),x1),
                (x1,(rY0,start),x2),
                (x2,(rX0,silentEvent),x3),
                (x3,(rX1,silentEvent),x4),
                (x4,(fY0,silentEvent),x5), (x4,(fX1,silentEvent),x6),
                (x5,(fX1,stop),x0), (x6,(fY0,stop),x0) },
           x_0: x0,
           Xm: StatesG_A,
           ctrlEvents: {fY0,rY0} ) &
 bound_G_A = 6
 &
 G_I = rec(states: StatesG_I,
           events: VocalOutput,
           tf: {(nm,start,im),
                (im,stop,nm) },
           x_0: nm,
           Xm: {im,nm},
           ctrlEvents: {start,stop} ) &
 bound_G_I = 2
 &
 G_A : Mealy_machine(G_A'states,G_A'inputs, G_A'outputs) &
 G_I : Generator_C(G_I'states,G_I'events)
 &
 T_agent = GetTraces_Mealy(G_A,bound_G_A) &
 L_G_A = Traces2Strings_i(T_agent,Input,Output) &
 Lm_G_A = Traces2Tasks_i(T_agent,G_A'Xm,Input,Output) &
 T_interface = GetTraces(G_I,bound_G_I) &
 L_G_I = Traces2Strings(T_interface) &
 Lm_G_I = Traces2Tasks(T_interface,G_I'Xm) &

 input_map : InputMap(G_A'states,G_A'inputs,G_A'outputs) &
 input_map = GetInputMap(T_agent,G_A'states,G_A'inputs,G_A'outputs)

VARIABLES
 theta   // A causal map

INVARIANT
 theta : CausalMap(G_A'inputs,G_I'events)

INITIALISATION
 theta := GetCausalMap(T_agent,Input,Output,VocalOutput)

OPERATIONS
 Check_WeakConsistencyOfMarking = PRE
  WeakConsistencyOfMarking(G_A,bound_G_A,T_agent,Lm_G_A,Lm_G_I,theta,input_map) THEN
   PRINT("Marking is weakly consistent.") END;

 Check_ConsistencyOfMarking = PRE
  ConsistencyOfMarking(L_G_A,Lm_G_A,Lm_G_I,theta) THEN
   PRINT("Marking is consistent.") END;

 Check_ObserverProperty = PRE
  Observer(L_G_A,L_G_I,G_I'events,theta) THEN
   PRINT("The causal map is an observer.") END;

 Check_ConditionOnSCT = PRE
  ControlCoincidence(L_G_A,G_A'inputs,G_A'ctrlEvents,G_I'ctrlEvents,theta) THEN
   PRINT("Control coincidence property is satisfied.") END END

/*
OUTPUT:
"Marking is weakly consistent."
cover(Check_WeakConsistencyOfMarking)
"Marking is consistent."
cover(Check_ConsistencyOfMarking)
"The causal map is an observer."
cover(Check_ObserverProperty)
*/