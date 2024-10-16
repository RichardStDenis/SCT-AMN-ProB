// copyright  Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL section1_machine_supervisor

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 1000000;
 SET_PREF_SMT == TRUE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../Theories/SCT_core.def";
 "LibraryIO.def"

SETS
 States = {x0,x1, x2, x3, x4, x5, x6, x7, x8, x9};
 Events = {e0, e1, e2, e3, e4, e5, e6, e7, e8, e9}

CONSTANTS
 G_FB,        // Automaton for the uncontrolled behavior
 G_LB,        // Automaton for the legal behavior
 T_G_FB, L_G_FB, Lm_G_FB, bound_T_G_FB,
 T_G_LB, Lm_G_LB, bound_T_G_LB

PROPERTIES
 G_FB = rec(states: States,  // Automaton modeling the uncontrolled behavior
          events: Events,
          tf: {(x0, e1, x1), (x0, e2, x2), (x0, e6, x6), (x0, e4, x8),
               (x2, e3, x3), (x3, e4, x4), (x4, e5, x5),
               (x6, e7, x7), (x8, e9, x9) },
          x_0: x0,
          Xm: {x1, x4, x6, x9},
          ctrlEvents: {e2, e4, e7, e9} )
 &
 bound_T_G_FB = 8
 &
 G_LB = rec(states: States,  // Automaton modeling the legal behavior
          events: Events,
          tf: {(x0, e1, x1), (x0, e2, x2), (x0, e6, x6), (x0, e4, x8),
               (x2, e3, x3), (x3, e4, x4), (x8, e9, x9) },
          x_0: x0,
          Xm: {x1, x4, x6, x9} )
 &
 bound_T_G_LB = 8
 &
 T_G_FB = GetTraces(G_FB,bound_T_G_FB) &
 T_G_LB = GetTraces(G_LB,bound_T_G_LB) &
 L_G_FB = Traces2Strings(T_G_FB) & Lm_G_FB = Traces2Tasks(T_G_FB,G_FB'Xm) &
 Lm_G_LB = Traces2Tasks(T_G_LB,G_LB'Xm)

VARIABLES
 code,
 K_i, K_succ_i, phi

INITIALISATION
 code := 0;
 K_i := supR(Lm_G_LB,Lm_G_FB);
 K_succ_i := iStepSupC(K_i,pr(K_i),L_G_FB,G_FB'events-G_FB'ctrlEvents);
 phi := {}

INVARIANT
 code >= 0 & code <= 3 &
 K_i <: Lm_G_LB & K_succ_i <: K_i &
 phi : SRsupervisor(G_FB'states,G_LB'states,G_FB'events)

OPERATIONS
 GetSupRC = PRE code = 0 & Lm_G_LB <: Lm_G_FB THEN
  WHILE K_i /= K_succ_i DO
   K_i := K_succ_i;
   K_succ_i := iStepSupC(K_i,pr(K_i),L_G_FB,G_FB'events-G_FB'ctrlEvents)
  INVARIANT card(K_i) >= 0
  VARIANT card(K_i)
  END;
  PRINT("The language of tasks attained under supervision:");
  PRINT(K_succ_i);
  code := 1
 END;

 GetSupervisors = PRE code = 1 THEN
  PRINT("SFBC (disabled events):");
  ANY phi_ WHERE
   phi_ : SRsupervisor(G_FB'states,G_LB'states,G_FB'events) &
   phi_ = SRsupervisorDISBL(T_G_FB,T_G_LB,pr(K_i),L_G_FB,G_FB'ctrlEvents) THEN
   PRINT(phi_) END;
  PRINT("SFBC (enabled events:");
  ANY phi_ WHERE
   phi_ : SRsupervisor(G_FB'states,G_LB'states,G_FB'events) &
   phi_ = SRsupervisorENBL(T_G_LB,T_G_LB,pr(K_i),G_FB'events) THEN
    phi := phi_; PRINT(phi); code:= 2 END END;

 ComputeAutomaton = PRE code = 2 THEN
  PRINT("The automaton:");
  LET tf_ BE tf_ = tf_closed_loop(G_FB,G_LB,phi) IN
   PRINT(inducedCloseLoop(G_FB,G_LB,tf_)) END;
  code := 3 END END

/*
OUTPUT:
"The language of tasks attained under supervision:"
{{(1|->e1)},{(1|->e4),(2|->e9)},{(1|->e6)}}
cover(GetSupRC)
"SFBC (disabled events):"
{((x0|->x0)|->{e2}),((x6|->x6)|->{e7})}
"SFBC (enabled events:"
{((x0|->x0)|->{e1,e4,e6}),((x8|->x8)|->{e9})}
cover(GetSupervisors)
"The automaton:"
rec(Xm:{(x1|->x1),(x6|->x6),(x9|->x9)},
    ctrlEvents:{e4,e9},
    events:{e1,e4,e6,e9},
    states:{(x0|->x0),(x1|->x1),(x6|->x6),(x8|->x8),(x9|->x9)},
    tf:{(((x0|->x0)|->e1)|->(x1|->x1)),(((x0|->x0)|->e4)|->(x8|->x8)),
        (((x0|->x0)|->e6)|->(x6|->x6)),(((x8|->x8)|->e9)|->(x9|->x9))},
    x_0:(x0|->x0))
cover(ComputeAutomaton)
*/