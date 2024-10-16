// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL tab3_trains_Po_MC_app

SEES
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------

//tunnel_6_2 /*@file "../../Trains/tunnel_6_2.mch" */
  tunnels_6_2 /*@file "../../Trains/tunnels_6_2.mch" */
//tunnel_8_2 /*@file "../../Trains/tunnel_8_2.mch" */
//tunnels_8_2 /*@file "../../Trains/tunnels_8_2.mch" */
// IMPORTANT: The initial state and marked state in the file LBtrains_FB_8_2
//            must be s2_s6.
//            The predicate Qpred1 or Qpred2 must be selected in the file SBtrains_8_2.

CONSTANTS
 output_file_name,           // output file name
 Qpred                       // set of good states

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
 "../../Theories/SBCDES.def";
 "LibraryIO.def";

PROPERTIES
 output_file_name = "~/Documents/SCTBenchmark/output_trains.txt" &

 G_FB : Generator_C(States,Events) &
 tunnel : obsMask(G_FB'states,ObservationSpace) &
  Qpred = Qpred1
//Qpred = Qpred2

VARIABLES
 code,
 supWCP,           // supremal controllable predicate stronger than Q
 appMCP,           // M-controllable predicate stronger than Q
 sfbc_d,           // SFBC function for the set of disabled events when partial observation
 sfbc_e            // SFBC function for the set of enabled events when partial observation

INITIALISATION
 code := 0;
 supWCP := CtrlField(G_FB,Qpred); 
 appMCP := {};
 sfbc_d := {};
 sfbc_e :=  {}

INVARIANT
 code >= 0 & code <= 10 &
 supWCP <: Qpred &
 appMCP <: Qpred &
 sfbc_d : SFBCf(G_FB'states,G_FB'ctrlEvents) &
 sfbc_e : SFBCf(G_FB'states,G_FB'events)

OPERATIONS
 DisplayStatistics = PRE code = 0 THEN
  PRINT("Display statistics...");
  FPRINTF(output_file_name,"~w sections, ~w trains: ",[card(LocalStates),card(G_FB'events)/card(LocalStates)]);
  FPRINTF(output_file_name,"~w states and ",[card(G_FB'states)]);
  FPRINTF(output_file_name,"~w transitions~n~n",[card(G_FB'tf)]);
  FPRINTF(output_file_name,"Predicate (~w states):~n",[card(Qpred)]);
  code := 1 END;

ComputeApproxQbyMCpred = PRE code = 1 THEN
  PRINT("Compute a M-controllable predicate...");
  appMCP := ApproxQbyMCpred(G_FB,tunnel,Qpred);
  FPRINTF(output_file_name,"Solution~nappMCP (~w reachable states):~n",[card(appMCP)]);
  FPRINTF(output_file_name,"~w~n~n",[appMCP]);
  code := 2 END;

 ComputeSFBCe = PRE code = 2 THEN
  PRINT("Compute SFBC (enabled events partial observation)...");
  sfbc_e := appMCP<|f_breve(G_FB,tunnel,supWCP);
  FPRINTF(output_file_name,"Enabled events (partiel observation): ~w~n",[sfbc_e]);
  code := 3 END;

 ComputeSFBCd = PRE code = 3 THEN
  PRINT("Compute SFBC (disabled events partial observation)...");
  sfbc_d := appMCP<|SFBC_breve(G_FB,tunnel,supWCP);
  FPRINTF(output_file_name,"Disabled events (partiel observation): ~w~n",[sfbc_d]);
  code := 4 END;

 CheckComplementarity = PRE code = 4 &
  !x_.(x_ : appMCP => sfbc_d(x_) /\ sfbc_e(x_) = {}) THEN
  PRINT("The SFBC functions are complementary.");
  code := 5 END END