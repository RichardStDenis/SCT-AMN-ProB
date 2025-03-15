// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL section3_1
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------
SETS
 States = {x1, x2, x3, x4, x5, x6, x7};
 Events = {e1, e2, e3, e4, e5, e6, e7, e8}

CONSTANTS Sys,
          M_G, 
          bound_Tr

ABSTRACT_CONSTANTS GetLgModel

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 5000000;
 SET_PREF_SMT == FALSE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../Theories/SCT_core.def";
 "LibraryIO.def";

PROPERTIES
 Sys = rec(states: States,
         events: Events,
         tf: {(x1,e1,x2), (x2,e2,x3),
              (x1,e3,x4), (x4,e4,x5), (x5,e7,x7), (x7,e8,x6),
                                                  (x7,e1,x5),
              (x1,e5,x6), (x6,e6,x4) },
         x_0: x1,
         Xm: {x5, x6} ) & bound_Tr = 3 &
 Sys : FSA(Sys'states,Sys'events) &

 GetLgModel = lambdaGetLgModel(Sys,bound_Tr,GetLgModel) &
 M_G = GetLgModel(baseCaseTr(Sys),baseCaseTk(Sys),0)

VARIABLES
 code

INVARIANT
 code >= 0 & code <= 3

INITIALISATION
 code := 0

OPERATIONS
 generateObjects = PRE code = 0 THEN
  PRINTF("Set of strings: ~w~n", [prj1(M_G)]);
  PRINTF("Set of tasks: ~w~n", [prj2(M_G)]);
  PRINTF("Prefixes of tasks: ~w~n", [pr(prj2(M_G))]);
  code := 1 END
END

/*
OUTPUT:

Finished Invariant Checking 2 ms walltime (1 ms runtime), since start: 2035 ms
Set of strings: {{},
                 {(1|->e1)},{(1|->e1),(2|->e2)},
                 {(1|->e3)},{(1|->e3),(2|->e4)},
                 {(1|->e5)},{(1|->e5),(2|->e6)},
                 {(1|->e3),(2|->e4),(3|->e7)},
                 {(1|->e5),(2|->e6),(3|->e4)}}
Set of tasks: {{(1|->e3),(2|->e4)},{(1|->e5)},{(1|->e5),(2|->e6),(3|->e4)}}
Prefixes of tasks: {{},{(1|->e3)},{(1|->e3),(2|->e4)},{(1|->e5)},{(1|->e5),(2|->e6)},
                    {(1|->e5),(2|->e6),(3|->e4)}}
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2050 ms
*/