// copyright  Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL section1_CNBfamily

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
 "../Theories/SBCDES.def";
 "LibraryIO.def"

SETS
 States = {x0,x1, x2, x3, x4, x5, x6, x7, x8, x9};
 Events = {e0, e1, e2, e3, e4, e5, e6, e7, e8, e9}

CONSTANTS
 G_,          // Automaton for the uncontrolled behavior
 Qpred        // Predicate representing the specification

PROPERTIES
 G_ = rec(states: States,    // Automaton modeling the uncontrolled behavior
          events: Events,
          tf: {(x0, e1, x1), (x0, e2, x2), (x0, e6, x6), (x0, e4, x8),
               (x1, e0, x0), (x2, e3, x3), (x3, e4, x4), (x4, e5, x5),
               (x6, e7, x7), (x8, e9, x9), (x9, e8, x8) },
          x_0: x0,
          Xm: {x1, x4, x6, x9},
          ctrlEvents: {e2, e4, e7, e8, e9} )
 &
 Qpred = States - {x5, x7}

VARIABLES
 code

INITIALISATION
 code := 0

INVARIANT
 code >= 0 & code <= 2

OPERATIONS
 DisplayC = PRE code = 0 THEN
  PRINT(C(G_,Qpred)); code := 1 END;

 DisplayCNB = PRE code = 1 THEN
  PRINT(CNB(G_,Qpred)); code := 2 END END

/*
OUTPUT:
{{},{x0,x1,x6},{x0,x1,x6,x8},{x0,x1,x2,x3,x6},{x0,x1,x2,x3,x6,x8},
 {x0,x1,x2,x3,x6,x8,x9},{x0,x1,x6,x8,x9}}
cover(DisplayC)
{{},{x0,x1,x6},{x0,x1,x6,x8,x9}}
cover(DisplayCNB)
*/