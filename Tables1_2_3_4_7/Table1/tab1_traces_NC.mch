// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL tab1_traces_NC

 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------
USES
//trains_6_2  /*@file "../../Trains/trains_6_2.mch" */
//trains_6_3  /*@file "../../Trains/trains_6_3.mch" */
  trains_12_2 /*@file "../../Trains/trains_12_2.mch" */

CONSTANTS T_G_NC

ABSTRACT_CONSTANTS GetSetTracesNC

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
 "../../Theories/SCT_core.def";
 "LibraryIO.def";

PROPERTIES
 G_FB : FSA(G_FB'states,G_FB'events) &
 GetSetTracesNC = lambdaGetTracesNC(G_FB,bound,GetSetTracesNC) &
 T_G_NC = GetSetTracesNC(baseCaseTr(G_FB),0)

VARIABLES
 code

INITIALISATION
 code := 0

INVARIANT
 code >= 0 & code <= 1

OPERATIONS
 DisplayStatistics = PRE code = 0 THEN
  PRINTF("Bound on the length of traces: ~w~n", [bound]);
  PRINTF("Number of traces (without cycle): ~w~n", [card(T_G_NC)]);
//PRINTF("Traces: ~w~n", [T_G_NC]);
  code := 1 END
END