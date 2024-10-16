// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL test_composite_component_LB_processing

SEES
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------
 LBprocessing /*@file "./LBprocessing.mch" */

CONSTANTS
 L_G_ts, Lm_G_ts,  // Languages of the transition system
 T_interface,      // Traces of the interface
 L_G_I, Lm_G_I     // Generated and marked languages of the interface

ABSTRACT_CONSTANTS //CheckMembership, GetReachableState, 
 ApplyTheta

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
 "../../Theories/Mealy_basis.def";
 "../../Theories/SCT_core.def";
 "../../Theories/SCT_hierarchical.def";
 "LibraryIO.def";

// theta for the particular case of the processing subsystem.
 lambdaApplyTheta(I,O,fun_) == /*@symbolic*/
  %(s_,t_).(s_ : String(I) & t_ : String(O) |
   IF s_ = [] THEN t_ ELSE
      IF first(s_) =  G_open THEN fun_(tail(s_),t_<-open) ELSE
         IF first(s_) = G_eoc THEN fun_(tail(s_),t_<-eoc) ELSE
            fun_(tail(s_),t_) END END END);

// No (weak0 consistency of marking, but eventually reach a marking state.
 EventuallyReachMkState_(Lm,Mm) ==
  !s_.(s_ : Mm => 
   #s__.( s__ : Lm & ApplyTheta(s__,[]) = s_) );

// Observer property.
 Observer_(L,M,T) == !(s_,tau_).(s_ : L & tau_ : T &
  ApplyTheta(s_,[])<-tau_ : M => #s__.(s__ : L & size(s__) > size(s_) &
   s__ /|\ size(s_) = s_ & ApplyTheta(s__,[]) = ApplyTheta(s_,[])<-tau_ ) );

// The set of vocal strings with respect to an output event tau.
 X_tau_(L,E,tau) == {s_ | s_ : L-{[]} &
  #e_.(e_ : E & last(s_) = e_ &
   ApplyTheta(s_,[]) = ApplyTheta(front(s_),[])<-tau ) };

// Control coincidence condition.
 ControlCoincidence_(L,E,Ec,Tc) ==
  !tau_.(tau_ : Tc & X_tau_(L,E,tau_) /= {} =>
   !s__.(s__ : X_tau_(L,E,tau_) => last(s__) : Ec) );

PROPERTIES
 //CheckMembership = lambdaCheckMembership(G_ts,CheckMembership) &
 //GetReachableState = lambdaGetReachableState(G_ts,GetReachableState) &
 ApplyTheta = lambdaApplyTheta(Input,Output,ApplyTheta) &

 Lm_G_ts =
            {[],
// 2 strings with one workpiece:
             [G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,R_rotate,R_eor,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa] }
             \/
// 12 strings with two workpieces:
            {[G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,R_rotate,R_eor,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa,
              G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,R_rotate,R_eor,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa],

             [G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,R_rotate,R_eor,T_test,T_success,T_eot,
              G_open,G_eoc,R_rotate,R_eor,C_transfer,C_eoa,D_drill,D_eod,R_rotate,R_eor,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa],

             [G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,
              G_open,G_eoc,R_rotate,R_eor,T_test,T_success,T_eot,D_drill,D_eod,R_rotate,R_eor,C_transfer,C_eoa,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa] }
              \/
// 16 strings with three workpieces:
            {[G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,R_rotate,R_eor,T_test,T_success,T_eot,
              G_open,G_eoc,R_rotate,R_eor,C_transfer,C_eoa,D_drill,D_eod,
              G_open,G_eoc,R_rotate,R_eor,T_test,T_success,T_eot,D_drill,D_eod,R_rotate,R_eor,C_transfer,C_eoa,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa],

             [G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,
              G_open,G_eoc,R_rotate,R_eor,T_test,T_success,T_eot,D_drill,D_eod,
              G_open,G_eoc,R_rotate,R_eor,C_transfer,C_eoa,T_test,T_success,T_eot,D_drill,D_eod,R_rotate,R_eor,C_transfer,C_eoa,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa] }
             \/
// 16 strings with four workpieces:
            {[G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,
              G_open,G_eoc,R_rotate,R_eor,T_test,T_success,T_eot,D_drill,D_eod,
              G_open,G_eoc,R_rotate,R_eor,C_transfer,C_eoa,T_test,T_success,T_eot,D_drill,D_eod,
              G_open,G_eoc,R_rotate,R_eor,C_transfer,C_eoa,T_test,T_success,T_eot,D_drill,D_eod,R_rotate,R_eor,
                                                           C_transfer,C_eoa,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa],

             [G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,R_rotate,R_eor,T_test,T_success,T_eot,
              G_open,G_eoc,R_rotate,R_eor,C_transfer,C_eoa,D_drill,D_eod,
              G_open,G_eoc,R_rotate,R_eor,T_test,T_success,T_eot,D_drill,D_eod,R_rotate,R_eor,C_transfer,C_eoa,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa,
              G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,R_rotate,R_eor,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa],

             [G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,
              G_open,G_eoc,R_rotate,R_eor,T_test,T_success,T_eot,D_drill,D_eod,
              G_open,G_eoc,R_rotate,R_eor,C_transfer,C_eoa,T_test,T_success,T_eot,D_drill,D_eod,R_rotate,R_eor,C_transfer,C_eoa,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa,
              G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,R_rotate,R_eor,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa],

             [G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,R_rotate,R_eor,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa,
              G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,R_rotate,R_eor,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa,
              G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,R_rotate,R_eor,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa,
              G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,R_rotate,R_eor,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa],

             [G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,R_rotate,R_eor,T_test,T_success,T_eot,
              G_open,G_eoc,R_rotate,R_eor,C_transfer,C_eoa,D_drill,D_eod,R_rotate,R_eor,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa,
              G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,R_rotate,R_eor,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa,
              G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,R_rotate,R_eor,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa],

             [G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,
              G_open,G_eoc,R_rotate,R_eor,T_test,T_success,T_eot,D_drill,D_eod,R_rotate,R_eor,C_transfer,C_eoa,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa,
              G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,R_rotate,R_eor,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa,
              G_open,G_eoc,R_rotate,R_eor,D_drill,D_eod,R_rotate,R_eor,T_test,T_success,T_eot,R_rotate,R_eor,C_transfer,C_eoa] } &

// !(s__).(s__ : Lm_G_ts => CheckMembership(G_ts'x_0,s__) = TRUE) &
 L_G_ts = pr(Lm_G_ts) &

 G_I : Generator_C(G_I'states,G_I'events) &
 T_interface =  GetTraces(G_I,bound_G_I) &
 L_G_I = Traces2Strings(T_interface) &
 Lm_G_I = Traces2Tasks(T_interface,G_I'Xm)
VARIABLES
 code

INVARIANT
 code >= 0 & code <= 5

INITIALISATION
 code := 0

OPERATIONS
 DisplayStatistics = PRE code = 0 THEN
  PRINTF("Number of states of the transition system ",[card(G_ts'states)]);
  PRINTF("Number of transitions of the transition system: ",[card(G_ts'tf)]);
  PRINTF("Number of states of the interface: ",[card(G_I'states)]);
  PRINTF("Number of transitions the interface: ",[card(G_I'tf)]);
  PRINTF("Number of strings: ~w~n", [card(L_G_ts)]);
  PRINTF("Number of tasks: ~w~n", [card(Lm_G_ts)]);
//  PRINTF("theta:~w~n",[UNION(s_).(s_ : Lm_G_ts | {ApplyTheta(s_,[])} )]);
//  PRINTF("Reachable state:~w~n",[ran(UNION(s_).(s_ : Lm_G_ts | {GetReachableState(G_ts'x_0,s_,s_)} ))]);
  code := 1 END;

 Check_EventuallyReachMkState = PRE code = 1 THEN
  IF EventuallyReachMkState_(Lm_G_ts,Lm_G_I) THEN
     PRINT("Eventually reach a marking state.") END;
  code := 2 END;

 Check_ObserverProperty = PRE code = 2 &
  Observer_(L_G_ts,L_G_I,G_I'events) THEN
   code := 3; PRINT("The causal map is an observer.") END;

 Check_ConditionOnSCT = PRE code = 3 &
  ControlCoincidence_(L_G_ts,G_ts'events,G_ts'ctrlEvents,G_I'ctrlEvents) THEN
   code := 4; PRINT("Control coincidence property is satisfied.") END;

 Check_NonBlocking_Interface = PRE code = 4 &
  NonBlocking(L_G_I,Lm_G_I) THEN
   code := 5; PRINT("The interface is nonblocking.") END END

/*
OUTPUT:
% Runtime for SOLUTION for SETUP_CONSTANTS: 180 ms (walltime: 190 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2630 ms
Number of states of the transition system 526
Number of transitions of the transition system: 1328
Number of states of the interface: 2
Number of transitions the interface: 2
Number of strings: 252
Number of tasks: 13
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2640 ms
"Eventually reach a marking state."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2670 ms
"The causal map is an observer."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 449720 ms
"Control coincidence property is satisfied."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 450650 ms
"The interface is nonblocking."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 450650 ms
Deadlock reached after 7 steps (after Check_NonBlocking_Interface).

% Runtime: 432370 ms (total: 432370 ms, walltime: 448240 ms)
% Finished execute (errors=0, warnings=0)
% Finished processing file after 448320 ms walltime (432450 ms runtime), since start: 450670 ms

% Finished process_file (errors=0, warnings=0)
*/
