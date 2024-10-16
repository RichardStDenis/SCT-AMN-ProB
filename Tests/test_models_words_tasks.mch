// Richard St-Denis, Universite de Sherbrooke, 2023.
MODEL test_models_words_tasks
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------
SETS
  States = {s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10};
  Events = {e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10}

CONSTANTS G, M_G, S_M_G, T_G, bound_Tr
ABSTRACT_CONSTANTS GetLgModel, GetSingletonSetModel

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 5000000;
 SET_PREF_SMT == TRUE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../Theories/SCT_core.def";
 "LibraryIO.def"

PROPERTIES
 G = rec(states: States,
         events: Events,
         tf: {(s0,e1,s1), (s1,e2,s2), (s2,e10,s10),
              (s0,e3,s3), (s3,e4,s4),
                                      (s4,e5,s5), (s5,e10,s10),
                                      (s4,e6,s6), (s6,e10,s10),
              (s0,e7,s7), (s7,e8,s8), (s8,e9,s9) },
         x_0: s0,
         Xm: {s0,s4,s10} ) & bound_Tr = 10 &
 G : FSA(States,Events) &

 GetLgModel = lambdaGetLgModel(G,bound_Tr,GetLgModel) &
 M_G = GetLgModel(baseCaseTr(G),baseCaseTk(G),1) &

 GetSingletonSetModel = lambdaGetSetLgModel(G,bound_Tr,GetSingletonSetModel) &
 S_M_G = GetSingletonSetModel(baseCaseTr(G),baseCaseTk(G),1) &

 T_G = GetTraces(G,bound_Tr)

VARIABLES
 code
INITIALISATION
 code := 0
INVARIANT
 code >= 0 & code <= 3

OPERATIONS
 a_pair_of_languages = PRE code = 0 THEN
  PRINTF("Set of strings: ~w~n", [prj1(POW(String(Events)),POW(String(Events)))(M_G)]);
  PRINTF("Set of tasks: ~w~n", [prj2(POW(String(Events)),POW(String(Events)))(M_G)]);
  PRINTF("Prefixes of tasks: ~w~n", [pr(prj2(POW(String(Events)),POW(String(Events)))(M_G))]);
  code := 1 END;

 a_singleton_set_of_language_model = PRE code = 1 THEN
  ANY L, Lm WHERE L : dom(S_M_G) & Lm : ran(S_M_G) THEN
   PRINTF("Set of strings: ~w~n", [L]);
   PRINTF("Set of tasks: ~w~n", [Lm]);
   PRINTF("Prefixes of tasks: ~w~n", [pr(Lm)]);
   IF L = prj1(POW(String(Events)),POW(String(Events)))(M_G) &
      Lm = prj2(POW(String(Events)),POW(String(Events)))(M_G) THEN
    PRINT("Results are consistent.") END END;
  code := 2 END;

 a_set_of_tasks = PRE code = 2 THEN
  PRINTF("Set of strings: ~w~n", [Traces2Strings(T_G)]);
  PRINTF("Set of tasks: ~w~n", [Traces2Tasks(T_G,G'Xm)]);
  PRINTF("Prefixes of tasks: ~w~n", [pr(Traces2Tasks(T_G,G'Xm))]);
   IF Traces2Strings(T_G) = prj1(POW(String(Events)),POW(String(Events)))(M_G) &
      Traces2Tasks(T_G,G'Xm) = prj2(POW(String(Events)),POW(String(Events)))(M_G) THEN
    PRINT("Results are consistent.") END;
  code := 3 END
END

/*
OUTPUT:
Set of strings: {{},{(1|->e1)},{(1|->e1),(2|->e2)},{(1|->e3)},{(1|->e3),(2|->e4)},{(1|->e7)},{(1|->e7),(2|->e8)},
                 [e1,e2,e10],[e3,e4,e5],[e3,e4,e6],[e3,e4,e5,e10],[e3,e4,e6,e10],[e7,e8,e9]}
Set of tasks: {{},{(1|->e3),(2|->e4)},[e1,e2,e10],[e3,e4,e5,e10],[e3,e4,e6,e10]}
Prefixes of tasks: {{},{(1|->e1)},{(1|->e1),(2|->e2)},{(1|->e3)},{(1|->e3),(2|->e4)},[e1,e2,e10],[e3,e4,e5],
                    [e3,e4,e6],[e3,e4,e5,e10],[e3,e4,e6,e10]}
cover(a_pair_of_languages)
Set of strings: {{},{(1|->e1)},{(1|->e1),(2|->e2)},{(1|->e3)},{(1|->e3),(2|->e4)},{(1|->e7)},{(1|->e7),(2|->e8)},
                 [e1,e2,e10],[e3,e4,e5],[e3,e4,e6],[e3,e4,e5,e10],[e3,e4,e6,e10],[e7,e8,e9]}
Set of tasks: {{},{(1|->e3),(2|->e4)},[e1,e2,e10],[e3,e4,e5,e10],[e3,e4,e6,e10]}
Prefixes of tasks: {{},{(1|->e1)},{(1|->e1),(2|->e2)},{(1|->e3)},{(1|->e3),(2|->e4)},[e1,e2,e10],[e3,e4,e5],
                    [e3,e4,e6],[e3,e4,e5,e10],[e3,e4,e6,e10]}
"Results are consistent."
cover(a_singleton_set_of_language_model)
Set of strings: {{},{(1|->e1)},{(1|->e1),(2|->e2)},{(1|->e3)},{(1|->e3),(2|->e4)},{(1|->e7)},{(1|->e7),(2|->e8)},
                 [e1,e2,e10],[e3,e4,e5],[e3,e4,e6],[e3,e4,e5,e10],[e3,e4,e6,e10],[e7,e8,e9]}
Set of tasks: {{},{(1|->e3),(2|->e4)},[e1,e2,e10],[e3,e4,e5,e10],[e3,e4,e6,e10]}
Prefixes of tasks: {{},{(1|->e1)},{(1|->e1),(2|->e2)},{(1|->e3)},{(1|->e3),(2|->e4)},[e1,e2,e10],[e3,e4,e5],
                    [e3,e4,e6],[e3,e4,e5,e10],[e3,e4,e6,e10]}
"Results are consistent."
cover(a_set_of_tasks)

ALL OPERATIONS COVERED
*/