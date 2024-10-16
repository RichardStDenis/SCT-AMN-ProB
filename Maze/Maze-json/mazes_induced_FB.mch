// Richard St-Denis, Universite de Sherbrooke, 2023.
MODEL mazes_induced_LB

USES
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------

  SBcat_mouse_5rooms /*@file "./SBcat_mouse_5rooms.mch" */
//SBcat_mouse_8rooms /*@file "./SBcat_mouse_8rooms.mch" */

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 50000000;
 SET_PREF_SMT == FALSE;

 Induced_G_LB(G,Q) ==
  rec(states: Q,
      events: G'events,
      tf: {t_ | #(x_,e_,x__).(x_ : Q & x__ : Q & e_ : G'events &
                              t_ = (x_,e_,x__) & t_ : G'tf )},
      x_0: G'x_0,
      Xm: G'Xm /\ Q );

 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../../Theories/SBCDES.def";
 "LibraryIO.def"

CONSTANTS
 output_file_name           // output file name

PROPERTIES
 output_file_name = "~/Documents/SCTbenchmark/output_maze.txt"

VARIABLES
 code,
 FSA_LB

INITIALISATION
 code := 0;
 FSA_LB := rec(states:{}, events:{}, tf:{}, x_0:G_FB'x_0, Xm:{})

INVARIANT
 code >= 0 & code <= 2 &
 FSA_LB : FSA(G_FB'states,G_FB'events)

OPERATIONS
 DisplayStatistics = PRE code = 0 THEN
  PRINTF("Number of states of the exhaustive model: ", [card(G_FB'states)]);
  PRINTF("Number of transitions of the exhaustive model: ", [card(G_FB'tf)]);
  code := 1 END;

 InduceFSA_LB = PRE code = 1 THEN
  FSA_LB := Induced_G_LB(G_FB, Qpred);
  PRINTF("Number of states of the restricted model: ", [card(FSA_LB'states)]);
  PRINTF("Number of transitions of the restricted model: ", [card(FSA_LB'tf)]);
  FPRINTF(output_file_name, "~w~n", [FSA_LB]);
  code := 2 END

END