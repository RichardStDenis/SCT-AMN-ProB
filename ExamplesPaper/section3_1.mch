// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL section3_1
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------
SETS
 States = {x1, x2, x3, x4, x5, x6, x7};
 Events = {e1, e2, e3, e4, e5, e6, e7, e8}

CONSTANTS Sys,
          F_Sys, F_Sys_NC_1, F_Sys_NC_2,
          M_Sys, M_Sys_NC, S_M_Sys, 
          T_Sys, T_Sys_NC, T_Sys_NC_1, T_Sys_NC_2, T_Sys_NC_3, T_Sys_NC_4,
          T_S_Sys, T_S_Sys_NC,
          bound_Tr,
          S_Sys

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
 Sys = rec(states: States,
           events: Events,
           tf: {(x1,e1,x2), (x2,e2,x3), // (x3,e3,x1),
                (x1,e3,x4), (x4,e4,x5), (x5,e7,x7), (x7,e8,x6),
                                                    (x7,e1,x5),
                (x1,e5,x6), (x6,e6,x4) },
           x_0: x1,
           Xm: {x5, x6} ) & bound_Tr = 6 &
 Sys : FSA(Sys'states,Sys'events) &

 S_Sys = GetStrings(Sys,bound_Tr) &

 T_Sys = GetTraces(Sys,bound_Tr) &
 T_Sys_NC = GetTracesNc1st(Sys,bound_Tr) &
 T_Sys_NC_1 = GetTracesNcSst(Sys,bound_Tr,{x4}) &
 T_Sys_NC_2 = GetTracesNcSev(Sys,bound_Tr,{e6}) &
 T_Sys_NC_3 = GetTracesNcSstSev(Sys,bound_Tr,{x4},{e6,e7}) &
 T_Sys_NC_4 = GetTracesNc1stNcSev(Sys,bound_Tr,{e7}) &

 GetSetTraces = lambdaGetTraces(Sys,bound_Tr,GetSetTraces) &
 T_S_Sys = GetSetTraces(baseCaseTr(Sys),0) &

 GetSetTracesNC = lambdaGetTracesNC(Sys,bound_Tr,GetSetTracesNC) &
 T_S_Sys_NC = GetSetTracesNC(baseCaseTr(Sys),0) &

 GetLgModel = lambdaGetLgModel(Sys,bound_Tr,GetLgModel) &
 M_Sys = GetLgModel(baseCaseTr(Sys),baseCaseTk(Sys),0) &

 GetLgModelNC = lambdaGetLgModelNC(Sys,bound_Tr,GetLgModelNC) &
 M_Sys_NC = GetLgModelNC(baseCaseTr(Sys),baseCaseTk(Sys),0) &

 GetSingletonSetModel = lambdaGetSetLgModel(Sys,bound_Tr,GetSingletonSetModel) &
 S_M_Sys = GetSingletonSetModel(baseCaseTr(Sys),baseCaseTk(Sys),0) &

 F_Sys = GetFragments(Sys,bound_Tr) &
 F_Sys_NC_1 = GetFragmentsNc1st(Sys,bound_Tr) &
 F_Sys_NC_2 = GetFragmentsNcSst(Sys,bound_Tr,{x4})

VARIABLES
 code

INVARIANT
 code >= 0 & code <= 3

INITIALISATION
 code := 0

OPERATIONS
 generateObjects = PRE code = 0 THEN
  PRINTF("Set of traces: ~w~n", [T_Sys]);
  PRINTF("Set of traces without cycles: ~w~n", [T_Sys_NC]);
  PRINTF("Set of traces: ~w~n", [card(T_S_Sys)]);
  PRINTF("Set of traces: ~w~n", [T_S_Sys]);
  PRINTF("Set of traces without cycles: ~w~n", [card(T_S_Sys_NC)]);
  PRINTF("Set of traces without cycles: ~w~n", [T_S_Sys_NC]);
  PRINTF("Set of strings: ~w~n", [prj1(M_Sys)]);
  PRINTF("Set of tasks: ~w~n", [prj2(M_Sys)]);
  PRINTF("Prefixes of tasks: ~w~n~n", [pr(prj2(M_Sys))]);
  PRINTF("Set of strings without cycles: ~w~n", [prj1(M_Sys_NC)]);
  PRINTF("Set of tasks without cycles: ~w~n", [prj2(M_Sys_NC)]);
  PRINTF("Set of traces without cycles (x4): ~w~n", [T_Sys_NC_1]);
  PRINTF("Set of traces without cycles (e6): ~w~n", [T_Sys_NC_2]);
  PRINTF("Set of traces without cycles (e6,e7): ~w~n", [T_Sys_NC_3]);
  PRINTF("Set of traces without cycles (e7): ~w~n", [T_Sys_NC_4]);
  code := 1 END;

 checkResults = PRE code = 1 &
  S_Sys = Traces2Strings(T_Sys) &
  T_Sys = T_S_Sys &
  T_Sys_NC_3 = T_S_Sys_NC &
  Traces2Strings(T_Sys) = prj1(M_Sys) &
  Traces2Tasks(T_Sys,Sys'Xm) = prj2(M_Sys) &
  Traces2Strings(T_Sys_NC_3) = prj1(M_Sys_NC) &
  Traces2Tasks(T_Sys_NC_3,Sys'Xm) = prj2(M_Sys_NC) &
  T_Sys = Fragments2Traces(F_Sys) &
  T_Sys_NC = Fragments2Traces(F_Sys_NC_1) &
  T_Sys_NC_1 = Fragments2Traces(F_Sys_NC_2)THEN
  PRINT("The results are consistent.");
  code := 2 END;

 generateSingletonSetLanguageModel = PRE code = 2 THEN
  ANY L, Lm WHERE L : dom(S_M_Sys) & Lm : ran(S_M_Sys) THEN
   PRINTF("Set of strings: ~w~n", [L]);
   PRINTF("Set of tasks: ~w~n", [Lm]);
   PRINTF("Prefixes of tasks: ~w~n", [pr(Lm)]);     
    IF L = prj1(M_Sys) & Lm = prj2(M_Sys) THEN
    PRINT("Language models are consistent.") END END;
  code := 3 END 
END

/*
PARTIAL OUTPUT:
Set of traces without cycles (x4): 
{rec(state:x1,string:{}),
 rec(state:x2,string:{(1|->e1)}),
 rec(state:x3,string:{(1|->e1),(2|->e2)}),
 rec(state:x4,string:{(1|->e3)}),
 rec(state:x4,string:{(1|->e5),(2|->e6)}),
 rec(state:x4,string:{(1|->e5),(2|->e6),(3|->e4),(4|->e7),(5|->e8),(6|->e6)}),
 rec(state:x4,string:{(1|->e3),(2|->e4),(3|->e7),(4|->e8),(5|->e6)}),
 rec(state:x5,string:{(1|->e3),(2|->e4)}),
 rec(state:x5,string:{(1|->e3),(2|->e4),(3|->e7),(4|->e1)}),
 rec(state:x5,string:{(1|->e5),(2|->e6),(3|->e4)}),
 rec(state:x5,string:{(1|->e5),(2|->e6),(3|->e4),(4|->e7),(5|->e1)}),
 rec(state:x5,string:{(1|->e3),(2|->e4),(3|->e7),(4|->e1),(5|->e7),(6|->e1)}),
 rec(state:x6,string:{(1|->e5)}),
 rec(state:x6,string:{(1|->e3),(2|->e4),(3|->e7),(4|->e8)}),
 rec(state:x6,string:{(1|->e5),(2|->e6),(3|->e4),(4|->e7),(5|->e8)}),
 rec(state:x6,string:{(1|->e3),(2|->e4),(3|->e7),(4|->e1),(5|->e7),(6|->e8)}),
 rec(state:x7,string:{(1|->e3),(2|->e4),(3|->e7)}),
 rec(state:x7,string:{(1|->e5),(2|->e6),(3|->e4),(4|->e7)}),
 rec(state:x7,string:{(1|->e5),(2|->e6),(3|->e4),(4|->e7),(5|->e1),(6|->e7)}),
 rec(state:x7,string:{(1|->e3),(2|->e4),(3|->e7),(4|->e1),(5|->e7)})}

Set of traces without cycles (e6): 
{rec(state:x1,string:{}),
 rec(state:x2,string:{(1|->e1)}),
 rec(state:x3,string:{(1|->e1),(2|->e2)}),
 rec(state:x4,string:{(1|->e3)}),
 rec(state:x4,string:{(1|->e5),(2|->e6)}),
 rec(state:x4,string:{(1|->e3),(2|->e4),(3|->e7),(4|->e8),(5|->e6)}),
 rec(state:x5,string:{(1|->e3),(2|->e4)}),
 rec(state:x5,string:{(1|->e3),(2|->e4),(3|->e7),(4|->e1)}),
 rec(state:x5,string:{(1|->e5),(2|->e6),(3|->e4)}),
 rec(state:x5,string:{(1|->e5),(2|->e6),(3|->e4),(4|->e7),(5|->e1)}),
 rec(state:x5,string:{(1|->e3),(2|->e4),(3|->e7),(4|->e8),(5|->e6),(6|->e4)}),
 rec(state:x5,string:{(1|->e3),(2|->e4),(3|->e7),(4|->e1),(5|->e7),(6|->e1)}),
 rec(state:x6,string:{(1|->e5)}),
 rec(state:x6,string:{(1|->e3),(2|->e4),(3|->e7),(4|->e8)}),
 rec(state:x6,string:{(1|->e5),(2|->e6),(3|->e4),(4|->e7),(5|->e8)}),
 rec(state:x6,string:{(1|->e3),(2|->e4),(3|->e7),(4|->e1),(5|->e7),(6|->e8)}),
 rec(state:x7,string:{(1|->e3),(2|->e4),(3|->e7)}),
 rec(state:x7,string:{(1|->e5),(2|->e6),(3|->e4),(4|->e7)}),
 rec(state:x7,string:{(1|->e5),(2|->e6),(3|->e4),(4|->e7),(5|->e1),(6|->e7)}),
 rec(state:x7,string:{(1|->e3),(2|->e4),(3|->e7),(4|->e1),(5|->e7)})}

Set of traces without cycles (e6,e7): 
{rec(state:x1,string:{}),
 rec(state:x2,string:{(1|->e1)}),
 rec(state:x3,string:{(1|->e1),(2|->e2)}),
 rec(state:x4,string:{(1|->e3)}),
 rec(state:x4,string:{(1|->e3),(2|->e4),(3|->e7),(4|->e8),(5|->e6)}),
 rec(state:x5,string:{(1|->e3),(2|->e4)}),
 rec(state:x5,string:{(1|->e3),(2|->e4),(3|->e7),(4|->e1)}),
 rec(state:x5,string:{(1|->e5),(2|->e6),(3|->e4)}),
 rec(state:x5,string:{(1|->e5),(2|->e6),(3|->e4),(4|->e7),(5|->e1)}),
 rec(state:x6,string:{(1|->e5)}),
 rec(state:x6,string:{(1|->e3),(2|->e4),(3|->e7),(4|->e8)}),
 rec(state:x6,string:{(1|->e5),(2|->e6),(3|->e4),(4|->e7),(5|->e8)}),
 rec(state:x7,string:{(1|->e3),(2|->e4),(3|->e7)}),
 rec(state:x7,string:{(1|->e5),(2|->e6),(3|->e4),(4|->e7)})}

Set of traces without cycles (e7): 
{rec(state:x1,string:{}),
 rec(state:x2,string:{(1|->e1)}),
 rec(state:x3,string:{(1|->e1),(2|->e2)}),
 rec(state:x4,string:{(1|->e3)}),
 rec(state:x4,string:{(1|->e5),(2|->e6)}),
 rec(state:x4,string:{(1|->e5),(2|->e6),(3|->e4),(4|->e7),(5|->e8),(6|->e6)}),
 rec(state:x4,string:{(1|->e3),(2|->e4),(3|->e7),(4|->e8),(5|->e6)}),
 rec(state:x5,string:{(1|->e3),(2|->e4)}),
 rec(state:x5,string:{(1|->e3),(2|->e4),(3|->e7),(4|->e1)}),
 rec(state:x5,string:{(1|->e5),(2|->e6),(3|->e4)}),
 rec(state:x5,string:{(1|->e5),(2|->e6),(3|->e4),(4|->e7),(5|->e1)}),
 rec(state:x5,string:{(1|->e3),(2|->e4),(3|->e7),(4|->e8),(5|->e6),(6|->e4)}),
 rec(state:x6,string:{(1|->e5)}),
 rec(state:x6,string:{(1|->e3),(2|->e4),(3|->e7),(4|->e8)}),
 rec(state:x6,string:{(1|->e5),(2|->e6),(3|->e4),(4|->e7),(5|->e8)}),
 rec(state:x7,string:{(1|->e3),(2|->e4),(3|->e7)}),

"The results are consistent."

"Language models are consistent."
*/