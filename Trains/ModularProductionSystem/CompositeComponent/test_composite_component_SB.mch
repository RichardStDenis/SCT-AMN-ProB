// Richard St-Denis, Universite de Sherbrooke, 2023.
MODEL test_composite_component_SB

SEES
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------

  SBconveyor_ts /*@file "./SBconveyor_ts.mch" */
//SBcrane_br /*@file "./SBcrane_br.mch" */
//SBcrane_br_w /*@file "./SBcrane_br_w.mch" */
//SBcrane_lr /*@file "./SBcrane_lr.mch" */
//SBcrane_lr2 /*@file "./SBcrane_lr2.mch" */
//SBcrane_lr2_w /*@file "./SBcrane_lr2_w.mch" */
//SBcrane_lr2_f /*@file "./SBcrane_lr2_f.mch" */
//SBcrane_lr2_f_w /*@file "./SBcrane_lr2_f_w.mch" */
//SBdispatcher /*@file "./SBdispatcher.mch" */
//SBdrill /*@file "./SBdrill.mch" */
//SBhook_e /*@file "./SBhook_e.mch" */
//SBhook_ef /*@file "./SBhook_ef.mch" */
//SBmicrometer /*@file "./SBmicrometer.mch" */
//SBservomotor /*@file "./SBservomotor.mch" */
//SBsorter /*@file "./SBsorter.mch" */
//SBtester /*@file "./SBtester.mch" */

//SBdistribution /*@file "./SBdistribution.mch" */
//SBhandling /*@file "./SBhandling.mch" */
//SBprocessing /*@file "./SBprocessing.mch" */
//SBtesting /*@file "./SBtesting.mch" */

//SBmps_dt /*@file "./SBmps_dt.mch" */
//SBmps_tp /*@file "./SBmps_tp.mch" */
//SBmps_ph /*@file "./SBmps_ph.mch" */
//SBmps /*@file "./SBmps.mch" */

CONSTANTS
 output_file_name            // output file name

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
 "LibraryIO.def"

PROPERTIES
 output_file_name = "~/Documents/SCTBenchmark/output_MPS.txt" &
 G_FB : Generator_C(States,Events)

VARIABLES
 code,
 supCCP,      // supremal controllable and coreachable predicate stronger than Q
 sfbc_d,      // SFBC function for the set of disabled events at any state x
 sfbc_e       // SFBC function for the set of enabled events at any state x

INITIALISATION
 code := 0;
 supCCP := {}; sfbc_d := {}; sfbc_e := {}

INVARIANT
 code >= 0 & code <= 3 &
 supCCP <: Qpred &
 sfbc_d : SFBCf(G_FB'states,G_FB'ctrlEvents) &
 sfbc_e : SFBCf(G_FB'states,G_FB'events)

OPERATIONS
 DisplayStatistics = PRE code = 0 THEN
  FPRINTF(output_file_name,"The transition structure of the agent~w~n",[""]);
  FPRINTF(output_file_name," (i.e.,the solution of the control problem)~w~n",[""]);
  FPRINTF(output_file_name,"Number of states of the exhaustive model: ",[card(G_FB'states)]);
  FPRINTF(output_file_name,"Number of transitions of the exhaustive model: ",[card(G_FB'tf)]);
  FPRINTF(output_file_name,"Cardinality of the predicate: ",[card(Qpred)]);
  code := 1 END;

 supCP = PRE code = 1 THEN
  PRINT("Compute supCCP...");
  VAR prev_, next_ IN
   prev_ := {};   next_ := Qpred;
   WHILE prev_ /= next_ DO
    prev_ := next_;   next_ := iStepSupCWCP(G_FB,Qpred,prev_)
   INVARIANT card(Qpred) >= 0 VARIANT card(next_) END;
  supCCP := R(G_FB,next_);
  PRINT("Compute state feedback functions...");
  sfbc_d := SFBC(G_FB,supCCP);
  sfbc_e := SFBCEnabled(G_FB,supCCP);
  PRINT("Save results...");
  FPRINTF(output_file_name,"Supremal coreachable and controllable predicates (~w):",[card(supCCP)]);
  FPRINTF(output_file_name,"~w~n",[supCCP]);
  FPRINTF(output_file_name,"Disabled events: ~w~n",[sfbc_d]);
  FPRINTF(output_file_name,"Enabled events: ~w~n",[sfbc_e]);
  FPRINTF(output_file_name,"Closed-loop system: ~w~n",[InducedAutomaton(G_FB,sfbc_e,supCCP)]);
  code := 2 END END;

 CheckControlNonconflicting_ = PRE code = 2 &
  !x_.(x_ : supCCP => sfbc_d (x_) /\ sfbc_e(x_) = {}) THEN
   PRINT("The control is nonconflicting.");
   code := 3 END END