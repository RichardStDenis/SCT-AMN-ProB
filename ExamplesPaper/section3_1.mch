// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL section3_1
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------
SETS
 States = {x1, x2, x3, x4, x5, x6, x7};
 Events = {e1, e2, e3, e4, e5, e6, e7, e8}

CONSTANTS G,
          F_G, F_G_NC_1, F_G_NC_2,
          M_G, M_G_NC, S_M_G, 
          T_G, T_G_NC, T_G_NC_1, T_G_NC_2, T_G_NC_3, T_G_NC_4,
          T_S_G, T_S_G_NC, bound_Tr,
          S_G

ABSTRACT_CONSTANTS GetLgModel, GetLgModelNC, GetSingletonSetModel, 
                   GetSetTraces, GetSetTracesNC

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
 G = rec(states: States,
         events: Events,
         tf: {(x1,e1,x2), (x2,e2,x3),
              (x1,e3,x4), (x4,e4,x5), (x5,e7,x7), (x7,e8,x6),
                                                  (x7,e1,x5),
              (x1,e5,x6), (x6,e6,x4) },
         x_0: x1,
         Xm: {x5, x6} ) & bound_Tr = 6 &
 G : FSA(G'states,G'events) &

 S_G = GetStrings(G,bound_Tr) &

 T_G = GetTraces(G,bound_Tr) &
 T_G_NC = GetTracesNc1st(G,bound_Tr) &
 T_G_NC_1 = GetTracesNcSst(G,bound_Tr,{x4}) &
 T_G_NC_2 = GetTracesNcSev(G,bound_Tr,{e6}) &
 T_G_NC_3 = GetTracesNcSstSev(G,bound_Tr,{x4},{e6,e7}) &
 T_G_NC_4 = GetTracesNc1stNcSev(G,bound_Tr,{e7}) &

 GetSetTraces = lambdaGetTraces(G,bound_Tr,GetSetTraces) &
 T_S_G = GetSetTraces(baseCaseTr(G),1) &

 GetSetTracesNC = lambdaGetTracesNC(G,bound_Tr,GetSetTracesNC) &
 T_S_G_NC = GetSetTracesNC(baseCaseTr(G),1) &

 GetLgModel = lambdaGetLgModel(G,bound_Tr,GetLgModel) &
 M_G = GetLgModel(baseCaseTr(G),baseCaseTk(G),1) &

 GetLgModelNC = lambdaGetLgModelNC(G,bound_Tr,GetLgModelNC) &
 M_G_NC = GetLgModelNC(baseCaseTr(G),baseCaseTk(G),1) &

 GetSingletonSetModel = lambdaGetSetLgModel(G,bound_Tr,GetSingletonSetModel) &
 S_M_G = GetSingletonSetModel(baseCaseTr(G),baseCaseTk(G),1) &

 F_G = GetFragments(G,bound_Tr) &
 F_G_NC_1 = GetFragmentsNc1st(G,bound_Tr) &
 F_G_NC_2 = GetFragmentsNcSst(G,bound_Tr,{x4})

VARIABLES
 code

INVARIANT
 code >= 0 & code <= 3

INITIALISATION
 code := 0

OPERATIONS
 generateObjects = PRE code = 0 THEN
  PRINTF("Set of traces: ~w~n", [T_G]);
  PRINTF("Set of traces without cycles: ~w~n", [T_G_NC]);
  PRINTF("Set of traces: ~w~n", [card(T_S_G)]);
  PRINTF("Set of traces: ~w~n", [T_S_G]);
  PRINTF("Set of traces without cycles: ~w~n", [card(T_S_G_NC)]);
  PRINTF("Set of traces without cycles: ~w~n", [T_S_G_NC]);
  PRINTF("Set of strings: ~w~n", [prj1(POW(String(G'events)),POW(String(G'events)))(M_G)]);
  PRINTF("Set of tasks: ~w~n", [prj2(POW(String(G'events)),POW(String(G'events)))(M_G)]);
  PRINTF("Prefixes of tasks: ~w~n~n", [pr(prj2(POW(String(G'events)),POW(String(G'events)))(M_G))]);
  PRINTF("Set of strings without cycles: ~w~n", [prj1(POW(String(G'events)),POW(String(G'events)))(M_G_NC)]);
  PRINTF("Set of tasks without cycles: ~w~n", [prj2(POW(String(G'events)),POW(String(G'events)))(M_G_NC)]);
  code := 1 END;

 checkResults = PRE code = 1 &
  S_G = Traces2Strings(T_G) &
  T_G = T_S_G &
  T_G_NC_3 = T_S_G_NC &
  Traces2Strings(T_G) = prj1(POW(String(G'events)),POW(String(G'events)))(M_G) &
  Traces2Tasks(T_G,G'Xm) = prj2(POW(String(G'events)),POW(String(G'events)))(M_G) &
  Traces2Strings(T_G_NC_3) = prj1(POW(String(G'events)),POW(String(G'events)))(M_G_NC) &
  Traces2Tasks(T_G_NC_3,G'Xm) = prj2(POW(String(G'events)),POW(String(G'events)))(M_G_NC) &
  T_G = Fragments2Traces(F_G) &
  T_G_NC = Fragments2Traces(F_G_NC_1) &
  T_G_NC_1 = Fragments2Traces(F_G_NC_2)THEN
  PRINT("The results are consistent.");
  code := 2 END;

 generateSingletonSetLanguageModel = PRE code = 2 THEN
  ANY L, Lm WHERE L : dom(S_M_G) & Lm : ran(S_M_G) THEN
   PRINTF("Set of strings: ~w~n", [L]);
   PRINTF("Set of tasks: ~w~n", [Lm]);
   PRINTF("Prefixes of tasks: ~w~n", [pr(Lm)]);
   IF L = prj1(POW(String(G'events)),POW(String(G'events)))(M_G) &
      Lm = prj2(POW(String(G'events)),POW(String(G'events)))(M_G) THEN
    PRINT("Language models are consistent.") END END;
  code := 3 END
END

/*
OUTPUT:
% Runtime for SOLUTION for SETUP_CONSTANTS: 130 ms (walltime: 140 ms)

 =INIT=> code := 0
 [=OK= 0 ms]
Set of traces: {
rec(state:x1,string:{}),rec(state:x2,string:{(1|->e1)}),
rec(state:x3,string:{(1|->e1),(2|->e2)}),rec(state:x4,string:{(1|->e3)}),
rec(state:x4,string:{(1|->e5),(2|->e6)}),rec(state:x4,string:[e5,e6,e4,e7,e8,e6]),
rec(state:x4,string:[e3,e4,e7,e8,e6]),rec(state:x5,string:{(1|->e3),(2|->e4)}),
rec(state:x5,string:[e3,e4,e7,e1]),rec(state:x5,string:[e5,e6,e4]),
rec(state:x5,string:[e5,e6,e4,e7,e1]),rec(state:x5,string:[e3,e4,e7,e8,e6,e4]),
rec(state:x5,string:[e3,e4,e7,e1,e7,e1]),rec(state:x6,string:{(1|->e5)}),
rec(state:x6,string:[e3,e4,e7,e8]),rec(state:x6,string:[e5,e6,e4,e7,e8]),
rec(state:x6,string:[e3,e4,e7,e1,e7,e8]),rec(state:x7,string:[e3,e4,e7]),
rec(state:x7,string:[e5,e6,e4,e7]),rec(state:x7,string:[e5,e6,e4,e7,e1,e7]),
rec(state:x7,string:[e3,e4,e7,e1,e7])}
Set of traces without cycles: {
rec(state:x1,string:{}),rec(state:x2,string:{(1|->e1)}),rec(state:x3,string:{(1|->e1),
(2|->e2)}),rec(state:x4,string:{(1|->e3)}),rec(state:x4,string:{(1|->e5),(2|->e6)}),
rec(state:x4,string:[e5,e6,e4,e7,e8,e6]),rec(state:x4,string:[e3,e4,e7,e8,e6]),
rec(state:x5,string:{(1|->e3),(2|->e4)}),rec(state:x5,string:[e3,e4,e7,e1]),
rec(state:x5,string:[e5,e6,e4]),rec(state:x5,string:[e5,e6,e4,e7,e1]),
rec(state:x5,string:[e3,e4,e7,e8,e6,e4]),rec(state:x5,string:[e3,e4,e7,e1,e7,e1]),
rec(state:x6,string:{(1|->e5)}),rec(state:x6,string:[e3,e4,e7,e8]),
rec(state:x6,string:[e5,e6,e4,e7,e8]),rec(state:x6,string:[e3,e4,e7,e1,e7,e8]),
rec(state:x7,string:[e3,e4,e7]),rec(state:x7,string:[e5,e6,e4,e7]),
rec(state:x7,string:[e5,e6,e4,e7,e1,e7]),rec(state:x7,string:[e3,e4,e7,e1,e7])}
Set of traces: 21
Set of traces: {
rec(state:x1,string:{}),rec(state:x2,string:{(1|->e1)}),rec(state:x3,string:{(1|->e1),
(2|->e2)}),rec(state:x4,string:{(1|->e3)}),rec(state:x4,string:{(1|->e5),(2|->e6)}),
rec(state:x4,string:[e5,e6,e4,e7,e8,e6]),rec(state:x4,string:[e3,e4,e7,e8,e6]),
rec(state:x5,string:{(1|->e3),(2|->e4)}),rec(state:x5,string:[e3,e4,e7,e1]),
rec(state:x5,string:[e5,e6,e4]),rec(state:x5,string:[e5,e6,e4,e7,e1]),
rec(state:x5,string:[e3,e4,e7,e8,e6,e4]),rec(state:x5,string:[e3,e4,e7,e1,e7,e1]),
rec(state:x6,string:{(1|->e5)}),rec(state:x6,string:[e3,e4,e7,e8]),
rec(state:x6,string:[e5,e6,e4,e7,e8]),rec(state:x6,string:[e3,e4,e7,e1,e7,e8]),
rec(state:x7,string:[e3,e4,e7]),rec(state:x7,string:[e5,e6,e4,e7]),
rec(state:x7,string:[e5,e6,e4,e7,e1,e7]),rec(state:x7,string:[e3,e4,e7,e1,e7])}
Set of traces without cycles: 15
Set of traces without cycles: {
rec(state:x1,string:{}),rec(state:x2,string:{(1|->e1)}),rec(state:x3,string:{(1|->e1),
(2|->e2)}),rec(state:x4,string:{(1|->e3)}),rec(state:x4,string:{(1|->e5),(2|->e6)}),
rec(state:x4,string:[e3,e4,e7,e8,e6]),rec(state:x5,string:{(1|->e3),(2|->e4)}),
rec(state:x5,string:[e3,e4,e7,e1]),rec(state:x5,string:[e5,e6,e4]),
rec(state:x5,string:[e5,e6,e4,e7,e1]),rec(state:x6,string:{(1|->e5)}),
rec(state:x6,string:[e3,e4,e7,e8]),rec(state:x6,string:[e5,e6,e4,e7,e8]),
rec(state:x7,string:[e3,e4,e7]),rec(state:x7,string:[e5,e6,e4,e7])}
Set of strings: {{},{(1|->e1)},{(1|->e1),(2|->e2)},{(1|->e3)},{(1|->e3),
(2|->e4)},{(1|->e5)},{(1|->e5),(2|->e6)},[e3,e4,e7],[e3,e4,e7,e1],[e3,e4,e7,e8],[
e5,e6,e4],[e5,e6,e4,e7],[e5,e6,e4,e7,e1],[e5,e6,e4,e7,e8],[e5,e6,e4,e7,e1,e7],
[e5,e6,e4,e7,e8,e6],[e3,e4,e7,e1,e7],[e3,e4,e7,e8,e6],[e3,e4,e7,e8,e6,e4],
[e3,e4,e7,e1,e7,e1],[e3,e4,e7,e1,e7,e8]}
Set of tasks: {
{(1|->e3),(2|->e4)},{(1|->e5)},[e3,e4,e7,e1],[e3,e4,e7,e8],[e5,e6,e4],[e5,e6,e4,e7,e1],
 [e5,e6,e4,e7,e8],[e3,e4,e7,e8,e6,e4],[e3,e4,e7,e1,e7,e1],[e3,e4,e7,e1,e7,e8]}
Prefixes of tasks: {
{},{(1|->e3)},{(1|->e3),(2|->e4)},{(1|->e5)},{(1|->e5),(2|->e6)},[e3,e4,e7],
[e3,e4,e7,e1],[e3,e4,e7,e8],[e5,e6,e4],[e5,e6,e4,e7],[e5,e6,e4,e7,e1],
[e5,e6,e4,e7,e8],[e3,e4,e7,e1,e7],[e3,e4,e7,e8,e6],[e3,e4,e7,e8,e6,e4],
[e3,e4,e7,e1,e7,e1],[e3,e4,e7,e1,e7,e8]}

Set of strings without cycles: 
{{},{(1|->e1)},{(1|->e1),(2|->e2)},{(1|->e3)},{(1|->e3),(2|->e4)},{(1|->e5)},
 {(1|->e5),(2|->e6)},[e3,e4,e7],[e3,e4,e7,e1],[e3,e4,e7,e8],[e5,e6,e4],[e5,e6,e4,e7],
 [e5,e6,e4,e7,e1],[e5,e6,e4,e7,e8],[e3,e4,e7,e8,e6]}
Set of tasks without cycles: {
{(1|->e3),(2|->e4)},{(1|->e5)},[e3,e4,e7,e1],[e3,e4,e7,e8],[e5,e6,e4],[e5,e6,e4,e7,e1],
 [e5,e6,e4,e7,e8]}
cover(generateObjects)
"The results are consistent."
cover(checkResults)
Set of strings: {
{},{(1|->e1)},{(1|->e1),(2|->e2)},{(1|->e3)},{(1|->e3),(2|->e4)},{(1|->e5)},{(1|->e5),
(2|->e6)},[e3,e4,e7],[e3,e4,e7,e1],[e3,e4,e7,e8],[e5,e6,e4],[e5,e6,e4,e7]
[e5,e6,e4,e7,e1],[e5,e6,e4,e7,e8],[e5,e6,e4,e7,e1,e7],[e5,e6,e4,e7,e8,e6],
[e3,e4,e7,e1,e7],[e3,e4,e7,e8,e6],[e3,e4,e7,e8,e6,e4],[e3,e4,e7,e1,e7,e1],
[e3,e4,e7,e1,e7,e8]}
Set of tasks: {
{(1|->e3),(2|->e4)},{(1|->e5)},[e3,e4,e7,e1],[e3,e4,e7,e8],[e5,e6,e4],[e5,e6,e4,e7,e1],
[e5,e6,e4,e7,e8],[e3,e4,e7,e8,e6,e4],[e3,e4,e7,e1,e7,e1],[e3,e4,e7,e1,e7,e8]}
Prefixes of tasks: {
{},{(1|->e3)},{(1|->e3),(2|->e4)},{(1|->e5)},{(1|->e5),(2|->e6)},[e3,e4,e7],[e3,e4,e7,e1],
[e3,e4,e7,e8],[e5,e6,e4],[e5,e6,e4,e7],[e5,e6,e4,e7,e1],[e5,e6,e4,e7,e8],[e3,e4,e7,e1,e7],
[e3,e4,e7,e8,e6],[e3,e4,e7,e8,e6,e4],[e3,e4,e7,e1,e7,e1],[e3,e4,e7,e1,e7,e8]}
"Language models are consistent."
cover(generateSingletonSetLanguageModel)
*/