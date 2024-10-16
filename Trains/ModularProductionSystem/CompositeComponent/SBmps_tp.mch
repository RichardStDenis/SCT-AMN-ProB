// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBmps_tp

SETS
 States = {Tidle_b20_Pready, Tpause_b20_Pready, Tidle_b20_Punready,
           Trt_b20_Pready, Tpause_b20_Punready, Twt_b20_Pready,
           Trt_b20_Punready, Twt_b20_Punready, Tit_b20_Pready,
           Tit_b20_Punready, Trt_b21_Pready, Trt_b21_Punready,
           Tidle_b21_Pready, Tidle_b21_Punready, Tpause_b21_Pready,
           Tpause_b21_Punready, Twt_b21_Pready, Twt_b21_Punready,
           Tit_b21_Pready, Tit_b21_Punready };
 Events = {T_eot, T_eotwp, T_fail, T_pass, T_test, T_twp, P_eoc, P_open};
 LocalStates = {Tidle, Tit, Tpause, Trt, Twt, b20, b21, Pready, Punready}

CONSTANTS
 G_FB,
 testing, buffer2, processing,
 Qpred, BadStates

PROPERTIES
 G_FB = rec(       // Automaton modeling the free behavior
            states: States,
            events: Events,
            tf: {(Tidle_b20_Pready,T_test,Tpause_b20_Pready), (Tidle_b20_Pready,P_open,Tidle_b20_Punready), (Tpause_b20_Pready,T_fail,Trt_b20_Pready),
                 (Tpause_b20_Pready,P_open,Tpause_b20_Punready), (Tpause_b20_Pready,T_pass,Twt_b20_Pready), (Tidle_b20_Punready,T_test,Tpause_b20_Punready),
                 (Tidle_b20_Punready,P_eoc,Tidle_b20_Pready), (Trt_b20_Pready,T_eot,Tidle_b20_Pready), (Trt_b20_Pready,P_open,Trt_b20_Punready),
                 (Tpause_b20_Punready,T_fail,Trt_b20_Punready), (Tpause_b20_Punready,P_eoc,Tpause_b20_Pready), (Tpause_b20_Punready,T_pass,Twt_b20_Punready),
                 (Twt_b20_Pready,T_twp,Tit_b20_Pready), (Twt_b20_Pready,P_open,Twt_b20_Punready), (Trt_b20_Punready,T_eot,Tidle_b20_Punready),
                 (Trt_b20_Punready,P_eoc,Trt_b20_Pready), (Twt_b20_Punready,T_twp,Tit_b20_Punready), (Twt_b20_Punready,P_eoc,Twt_b20_Pready),
                 (Tit_b20_Pready,T_eotwp,Trt_b21_Pready), (Tit_b20_Pready,P_open,Tit_b20_Punready), (Tit_b20_Punready,T_eotwp,Trt_b21_Punready),
                 (Tit_b20_Punready,P_eoc,Tit_b20_Pready), (Trt_b21_Pready,T_eot,Tidle_b21_Pready), (Trt_b21_Pready,P_open,Trt_b21_Punready),
                 (Trt_b21_Punready,T_eot,Tidle_b21_Punready), (Trt_b21_Punready,P_eoc,Trt_b20_Pready), (Tidle_b21_Pready,T_test,Tpause_b21_Pready),
                 (Tidle_b21_Pready,P_open,Tidle_b21_Punready), (Tidle_b21_Punready,T_test,Tpause_b21_Punready), (Tidle_b21_Punready,P_eoc,Tidle_b20_Pready),
                 (Tpause_b21_Pready,T_fail,Trt_b21_Pready), (Tpause_b21_Pready,P_open,Tpause_b21_Punready), (Tpause_b21_Pready,T_pass,Twt_b21_Pready),
                 (Tpause_b21_Punready,T_fail,Trt_b21_Punready), (Tpause_b21_Punready,P_eoc,Tpause_b20_Pready), (Tpause_b21_Punready,T_pass,Twt_b21_Punready),
                 (Twt_b21_Pready,T_twp,Tit_b21_Pready), (Twt_b21_Pready,P_open,Twt_b21_Punready), (Twt_b21_Punready,T_twp,Tit_b21_Punready),
                 (Twt_b21_Punready,P_eoc,Twt_b20_Pready), (Tit_b21_Pready,P_open,Tit_b21_Punready), (Tit_b21_Pready,T_eotwp,Trt_b21_Pready),
                 (Tit_b21_Punready,P_eoc,Tit_b20_Pready), (Tit_b21_Punready,T_eotwp,Trt_b21_Punready) },
            x_0: Tidle_b20_Pready,
            Xm: {Tidle_b20_Pready},
            ctrlEvents: {T_test, T_twp, P_open} )
 &
 testing = {
      (Tidle_b20_Pready,Tidle),(Tpause_b20_Pready,Tpause),(Tidle_b20_Punready,Tidle),
      (Trt_b20_Pready,Trt),(Tpause_b20_Punready,Tpause),(Twt_b20_Pready,Twt),
      (Trt_b20_Punready,Trt),(Twt_b20_Punready,Twt),(Tit_b20_Pready,Tit),
      (Tit_b20_Punready,Tit),(Trt_b21_Pready,Trt),(Trt_b21_Punready,Trt),
      (Tidle_b21_Pready,Tidle),(Tidle_b21_Punready,Tidle),(Tpause_b21_Pready,Tpause),
      (Tpause_b21_Punready,Tpause),(Twt_b21_Pready,Twt),(Twt_b21_Punready,Twt),
      (Tit_b21_Pready,Tit),(Tit_b21_Punready,Tit) }
 &
 buffer2 = {
      (Tidle_b20_Pready,b20),(Tpause_b20_Pready,b20),(Tidle_b20_Punready,b20),
      (Trt_b20_Pready,b20),(Tpause_b20_Punready,b20),(Twt_b20_Pready,b20),
      (Trt_b20_Punready,b20),(Twt_b20_Punready,b20),(Tit_b20_Pready,b20),
      (Tit_b20_Punready,b20),(Trt_b21_Pready,b21),(Trt_b21_Punready,b21),
      (Tidle_b21_Pready,b21),(Tidle_b21_Punready,b21),(Tpause_b21_Pready,b21),
      (Tpause_b21_Punready,b21),(Twt_b21_Pready,b21),(Twt_b21_Punready,b21),
      (Tit_b21_Pready,b21),(Tit_b21_Punready,b21) }
 &
 processing = {
      (Tidle_b20_Pready,Pready),(Tpause_b20_Pready,Pready),(Tidle_b20_Punready,Punready),
      (Trt_b20_Pready,Pready),(Tpause_b20_Punready,Punready),(Twt_b20_Pready,Pready),
      (Trt_b20_Punready,Punready),(Twt_b20_Punready,Punready),(Tit_b20_Pready,Pready),
      (Tit_b20_Punready,Punready),(Trt_b21_Pready,Pready),(Trt_b21_Punready,Punready),
      (Tidle_b21_Pready,Pready),(Tidle_b21_Punready,Punready),(Tpause_b21_Pready,Pready),
      (Tpause_b21_Punready,Punready),(Twt_b21_Pready,Pready),(Twt_b21_Punready,Punready),
      (Tit_b21_Pready,Pready),(Tit_b21_Punready,Punready) }
 &
 /*
  Standard constraints:
     3. The second buffer cannot be declared free when the processing station is unready,
        that is, it cannot accept a workpiece from the testing station.
     4. When the second buffer is full, the testing station cannot deliver a
        workpiece to the processing station.
*/
 BadStates =
// The lift is in extension and the measurement sequence of the micrometer is completed:
  { x | x : States & buffer2(x) = b20 & processing(x) = Punready} \/
// The lift is in retraction and the micrometer is idle:
  { x | x : States & buffer2(x) = b21 & testing(x) = Tit}
 &
 Qpred = States - BadStates
END

/*
OUTPUT:
The transition structure of the agent
 (i.e.,the solution of the control problem)
Number of states of the exhaustive model: 20
Number of transitions of the exhaustive model: 44
Cardinality of the predicate: 13
Supremal coreachable and controllable predicates (13):
{Tidle_b20_Pready,Tpause_b20_Pready,Trt_b20_Pready,Twt_b20_Pready,
 Tit_b20_Pready,Trt_b21_Pready,Trt_b21_Punready,Tidle_b21_Pready,
 Tidle_b21_Punready,Tpause_b21_Pready,Tpause_b21_Punready,Twt_b21_Pready,
 Twt_b21_Punready}
Disabled events:
{(Tidle_b20_Pready|->{P_open}),(Tpause_b20_Pready|->{P_open}),
 (Trt_b20_Pready|->{P_open}),(Twt_b20_Pready|->{P_open}),
 (Tit_b20_Pready|->{P_open}),(Trt_b21_Pready|->{}),
 (Trt_b21_Punready|->{}),(Tidle_b21_Pready|->{}),
 (Tidle_b21_Punready|->{}),(Tpause_b21_Pready|->{}),
 (Tpause_b21_Punready|->{}),(Twt_b21_Pready|->{T_twp}),
 (Twt_b21_Punready|->{T_twp})}
Enabled events:
{(Tidle_b20_Pready|->{T_test}),(Tpause_b20_Pready|->{T_fail,T_pass}),
 (Trt_b20_Pready|->{T_eot}),(Twt_b20_Pready|->{T_twp}),
 (Tit_b20_Pready|->{T_eotwp}),(Trt_b21_Pready|->{T_eot,P_open}),
 (Trt_b21_Punready|->{T_eot,P_eoc}),(Tidle_b21_Pready|->{T_test,P_open}),
 (Tidle_b21_Punready|->{T_test,P_eoc}),(Tpause_b21_Pready|->{T_fail,T_pass,P_open}),
 (Tpause_b21_Punready|->{T_fail,T_pass,P_eoc}),(Twt_b21_Pready|->{P_open}),
 (Twt_b21_Punready|->{P_eoc})}
Closed-loop system:
rec(Xm:{Tidle_b20_Pready},
    ctrlEvents:{T_test,T_twp,P_open},
    events:{T_eot,T_eotwp,T_fail,T_pass,T_test,T_twp,P_eoc,P_open},
    states:{Tidle_b20_Pready,Tpause_b20_Pready,Trt_b20_Pready,Twt_b20_Pready,
            Tit_b20_Pready,Trt_b21_Pready,Trt_b21_Punready,Tidle_b21_Pready,
            Tidle_b21_Punready,Tpause_b21_Pready,Tpause_b21_Punready,Twt_b21_Pready,
            Twt_b21_Punready},
    tf:{((Tidle_b20_Pready|->T_test)|->Tpause_b20_Pready),
          ((Tpause_b20_Pready|->T_fail)|->Trt_b20_Pready),
            ((Trt_b20_Pready|->T_eot)|->Tidle_b20_Pready),
          ((Tpause_b20_Pready|->T_pass)|->Twt_b20_Pready),
            ((Twt_b20_Pready|->T_twp)|->Tit_b20_Pready),
              ((Tit_b20_Pready|->T_eotwp)|->Trt_b21_Pready),
                ((Trt_b21_Pready|->T_eot)|->Tidle_b21_Pready),
                  ((Tidle_b21_Pready|->P_open)|->Tidle_b21_Punready),
                    ((Tidle_b21_Punready|->P_eoc)|->Tidle_b20_Pready),

                    ((Tidle_b21_Punready|->T_test)|->Tpause_b21_Punready),
                      ((Tpause_b21_Punready|->P_eoc)|->Tpause_b20_Pready),
                      ((Tpause_b21_Punready|->T_fail)|->Trt_b21_Punready),
                      ((Tpause_b21_Punready|->T_pass)|->Twt_b21_Punready),
                        ((Twt_b21_Punready|->P_eoc)|->Twt_b20_Pready)},

                ((Trt_b21_Pready|->P_open)|->Trt_b21_Punready),
                  ((Trt_b21_Punready|->P_eoc)|->Trt_b20_Pready),
                  ((Trt_b21_Punready|->T_eot)|->Tidle_b21_Punready), 

                  ((Tidle_b21_Pready|->T_test)|->Tpause_b21_Pready),
                    ((Tpause_b21_Pready|->P_open)|->Tpause_b21_Punready),
                    ((Tpause_b21_Pready|->T_fail)|->Trt_b21_Pready),
                    ((Tpause_b21_Pready|->T_pass)|->Twt_b21_Pready),
                     ((Twt_b21_Pready|->P_open)|->Twt_b21_Punready),
    x_0:Tidle_b20_Pready)
*/

/*
  ControlLaw(P_open) = Pready & b21
  ControlLaw(T_test) = Tidle
  ControlLaw(T_twp) = Twt & b20 & Pready
*/

/*
% Runtime for SOLUTION for SETUP_CONSTANTS: 90 ms (walltime: 90 ms)
% Finished searching for valid initial states 110 ms walltime (100 ms runtime), since start: 1740 ms
% Starting random_animate (max_steps=5)
perform_random_step(1,1)
random(2,1)
perform_random_step(1,2)
random(2,1)
perform_random_step(1,3)
random(2,1)
"Compute supCCP..."
"Compute state feedback functions..."
"Save results..."
perform_random_step(1,4)
random(2,1)
"The control is nonconflicting."

ALL OPERATIONS COVERED

perform_random_step(1,5)
random(2,1)
finished_random_animate(5,[runtime/110,total_runtime/110,walltime/120])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 280 ms walltime (270 ms runtime), since start: 1860 ms

% Finished process_file (errors=0, warnings=0)
*/