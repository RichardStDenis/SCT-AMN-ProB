// Richard St-Denis, Universite de Sherbrooke, 2023.
MODEL section5_3

SETS
 States = {x0, x1, x2, x3, x4, x5, x6, x7, x8};
 ObservationSpace = {y0, y1, y4, y5, y6, y7, y8};
 Events = {e1, e2, e3, e4, e5, e6}

CONSTANTS
 Gsys,   // Automaton for the uncontrolled behavior
 mask,   // Observation mask
 Qspec   // Predicate representing the specification to be fulfilled

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
 Gsys = rec(            // Automaton modeling the free behavior
            states: States,
            events: Events,
            tf: {(x0, e1, x1), (x0, e2, x2), (x0, e3, x3),
                 (x1, e4, x4), (x1, e5, x5), (x1, e6, x7),
                 (x2, e4, x8), (x2, e5, x6), (x2, e6, x7),
                 (x3, e4, x6), (x3, e5, x8), (x3, e6, x8) },
            x_0: x0,
            Xm: States,
            ctrlEvents: {e1, e2, e3, e4, e5} )
 &
 mask = {(x0, y0), (x1, y1), (x2, y1), (x3, y1), (x4, y4), (x5, y5), (x6, y6),
         (x7, y7), (x8, y8) }
 &
 Qspec = States - {x3, x6}
 &
 Gsys : Generator_C(Gsys'states,Gsys'events) &
 mask : obsMask(Gsys'states,ObservationSpace)

OPERATIONS
 ComputesupSMCP = VAR Q_i, Q_succ_i IN
  Q_i := {};   Q_succ_i := Qspec;
  WHILE Q_i /= Q_succ_i DO
   Q_i := Q_succ_i;  Q_succ_i := iStepSupSMCP(Gsys,mask,Qspec,Q_i)
  INVARIANT card(Qspec) >= 0 VARIANT card(Q_succ_i) END;
  PRINTF("~w~n",[Q_i]) END END

/*
OUTPUT:
{x0,x1,x2,x7}
cover(ComputesupSMCP)
*/