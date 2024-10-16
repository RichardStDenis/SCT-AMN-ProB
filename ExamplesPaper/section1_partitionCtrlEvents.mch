// copyright  Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL section1_partitionCtrlEvents

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
 Events = {e1, e2, e3, e4, e5, e6, e7, e9}

CONSTANTS
 G_FB,        // Automaton for the uncontrolled behavior
 G_LB,        // Automaton for the legal behavior
 L_G_FB, bound_L_G_FB,
 L_G_LB, bound_L_G_LB

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
 bound_L_G_FB = 8
 &
 G_LB = rec(states: States,  // Automaton modeling the uncontrolled behavior
          events: Events,
          tf: {(x0, e1, x1), (x0, e2, x2), (x0, e6, x6), (x0, e4, x8),
               (x2, e3, x3), (x3, e4, x4), (x8, e9, x9) },
          x_0: x0,
          Xm: {x1, x4, x6, x9} )
 &
 bound_L_G_LB = 8
 &
 L_G_FB = GetStrings(G_FB,bound_L_G_FB) &
 L_G_LB = GetStrings(G_LB,bound_L_G_LB)

VARIABLES
 code

INITIALISATION
 code := 0

INVARIANT
 code >= 0 & code <= 1

OPERATIONS
 FindPartition = PRE code = 0 THEN
  ANY Ec_ WHERE Ec_ <: G_FB'events & card(Ec_) < 4 &
   Controllability(L_G_LB,L_G_FB,uEvents(G_FB,Ec_)) THEN
  PRINT(Ec_);
  PRINT(uEvents(G_FB,Ec_)) END;
  code := 1
 END END

/*
OUTPUT:
{e1,e5,e7}
{e2,e3,e4,e6,e9}
cover(FindPartition)
*/