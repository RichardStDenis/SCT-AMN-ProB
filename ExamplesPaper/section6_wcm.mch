// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL section6_wcm
// Simple example for weak consistency of marking (wcm).
SETS
 StatesG_A = {x0, x1, x2, x3, x4, x5, x6, x7, x8, x9};
 Input = {i1, i2, i3, i4, i5, i6, i7, i8, i9, i10};
 Output = {o1, o2, silentEvent}

CONSTANTS
 G_A,         // Mealy machine modeling the device implementation (agent)
 bound_G_A,   // Bound on the trace length of the agent

 T_agent,     // Traces of the agent
 Lm_G_A,      // task of the agent
 Lm_G_I,      // tasks of the interface

 VocalOutput,
 input_map    // a map from the set of traces of a Mealy machine to its set of input string

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
G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,
           tf: {(x0,(i1,silentEvent),x1),
                (x1,(i2,silentEvent),x2), (x2,(i3,silentEvent),x3),
                (x3,(i4,o1),x4),
                (x4,(i5,silentEvent),x5),
                //(x4,(i7,o2),x0),      // weak consistency of marking  with Lm_G_I = {[],[o1,o2],[o1,o2,o1,o2]}
                //(x4,(i7,o2),x1),      // not weak consistency of marking
                //(x4,(i7,o2),x2),      // not weak consistency of marking
                //(x4,(i7,o2),x3),      // not weak consistency of marking
                (x5,(i6,silentEvent),x6),
                (x6,(i7,o2),x7),
                //(x7,(i4,o1),x4),      // not weak consistency of marking
                //(x8,(i4,o1),x4),      // not weak consistency of marking
                //(x9,(i4,o1),x4),      // not weak consistency of marking
                //(x0,(i4,o1),x4),        // weak consistency of marking
                (x7,(i8,silentEvent),x8), (x8,(i9,silentEvent),x9),

                (x9,(i10,silentEvent),x0) },
           x_0: x0,
           Xm: {x0},
           ctrlEvents: {i3,i6} ) &

 bound_G_A = 10
 &
 G_A : Mealy_machine(G_A'states,G_A'inputs, G_A'outputs) &
 DeterministicMealy_machine(G_A)
 &
 T_agent = GetTraces_Mealy(G_A,bound_G_A) &
 Lm_G_A = Traces2Tasks_i(T_agent,G_A'Xm,Input,Output)
 &
 Lm_G_I = {[],[o1,o2]}   //,[o1,o2,o1,o2]}
 &
 VocalOutput = Output - {silentEvent}
 &
 input_map : InputMap(G_A'states,G_A'inputs,G_A'outputs) &
 input_map = GetInputMap(T_agent,G_A'states,G_A'inputs,G_A'outputs)

VARIABLES
 Theta   // A causal map

INVARIANT
 Theta : CausalMap(G_A'inputs,VocalOutput)

INITIALISATION
 Theta := GetCausalMap(T_agent,Input,Output,VocalOutput)

OPERATIONS
 DisplayResults =
  BEGIN
   PRINTF("Pre-states: ~w~n",[UNION(x_).(x_ : G_A'states | {(x_,GetPreStates_Mealy(G_A,x_,bound_G_A))})]);
   PRINTF("Post-states: ~w~n",[UNION(x_).(x_ : G_A'states | {(x_,GetPostStates_Mealy(G_A,x_,bound_G_A))})]) END;

 CheckWeakConsistencyOfMarking = PRE
  WeakConsistencyOfMarking(G_A,bound_G_A ,T_agent,Lm_G_A,Lm_G_I,Theta,input_map) THEN
   PRINT("Weak consistency of marking") END
END

/*
OUTPUT:
Pre-states:
{(x0|->{}),(x1|->{x1}),(x2|->{x1,x2}),(x3|->{x1,x2,x3}),(x4|->{x4}),(x5|->{x4,x5}),
 (x6|->{x4,x5,x6}),(x7|->{x7}),(x8|->{x7,x8}),(x9|->{x7,x8,x9})}
Post-states:
{(x0|->{}),(x1|->{x1,x2,x3}),(x2|->{x2,x3}),(x3|->{x3}),(x4|->{x4,x5,x6}),(x5|->{x5,x6}),
 (x6|->{x6}),(x7|->{x7,x8,x9}),(x8|->{x8,x9}),(x9|->{x9})}
cover(DisplayResults)
"Weak consistency of marking"
cover(CheckWeakConsistencyOfMarking)
*/
