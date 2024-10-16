// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL tab1_model_NC

SEES
 // ----------------------------------------------------------------------------
 // Machine for input data
 // ----------------------------------------------------------------------------
//maze_5rooms /*@file "../../Maze/maze_5rooms.mch" */
//maze_8rooms /*@file "../../Maze/maze_8rooms.mch" */
  trains_6_2  /*@file "../../Trains/trains_6_2.mch" */
//trains_8_2  /*@file "../../Trains/trains_8_2.mch" */
//trains_10_2 /*@file "../../Trains/trains_10_2.mch" */

CONSTANTS M_G_NC

ABSTRACT_CONSTANTS GetLgModelNC

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 10000000;
 SET_PREF_SMT == TRUE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../../Theories/SCT_core.def";
 "LibraryIO.def";

PROPERTIES
 G : FSA(G'states,G'events) &
 GetLgModelNC = lambdaGetLgModelNC(G,bound,GetLgModelNC) &
 M_G_NC = GetLgModelNC(baseCaseTr(G),baseCaseTk(G),1)

VARIABLES
 code    // Sequencing code

INVARIANT
 code >= 0 & code <= 1

INITIALISATION
 code := 0

OPERATIONS
 a_pair_of_languages = PRE code = 0 THEN
  PRINTF("Set of Strings: ~w~n", [card(prj1(POW(String(G'events)),POW(String(G'events)))(M_G_NC))]);
  PRINTF("Set of tasks: ~w~n", [card(prj2(POW(String(G'events)),POW(String(G'events)))(M_G_NC))]);
  PRINTF("Prefixes of tasks: ~w~n~n", [card(pr(prj2(POW(String(G'events)),POW(String(G'events)))(M_G_NC)))]);
  code := 1 END
END