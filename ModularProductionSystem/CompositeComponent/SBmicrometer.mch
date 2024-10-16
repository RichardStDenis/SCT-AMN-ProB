// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBmicrometer

SETS
 States = {bom_Pr_Trtc, bom_Pr_Ti, bom_Pie_Trtc,
           bom_Pr_Tip, bom_Pie_Ti, bom_Pe_Trtc,
           mip_Pr_Trtc, bom_Pie_Tip, bom_Pe_Ti,
           bom_Pir_Trtc, mip_Pie_Trtc, mip_Pr_Ti,
           bom_Pe_Tip, bom_Pir_Ti, mip_Pe_Trtc,
           mip_Pie_Ti, mip_Pr_Tip, bom_Pir_Tip,
           mip_Pir_Trtc, mip_Pe_Ti, mip_Pie_Tip,
           eom_Pr_Trtc, mip_Pir_Ti, mip_Pe_Tip,
           eom_Pr_Ti, eom_Pie_Trtc, mip_Pir_Tip,
           eom_Pr_Tip, eom_Pie_Ti, eom_Pe_Trtc,
           eom_Pie_Tip, eom_Pe_Ti, eom_Pir_Trtc,
           eom_Pe_Tip, eom_Pir_Ti, eom_Pir_Tip };
 Events = {rst, P_eoa, T_exp, P_eod, P_gd, P_gu, T_clr, T_set};
 LocalStates = {bom, mip, eom, Pe, Pie, Pir, Pr, Ti, Tip, Trtc}

CONSTANTS
 G_FB,
 seqM, probe, timer,
 Qpred, BadStates

PROPERTIES
 G_FB =rec(   // Automaton modeling the free behavior
      states: States,
      events: Events,
      tf: {(bom_Pr_Trtc,T_clr,bom_Pr_Ti), (bom_Pr_Trtc,P_gd,bom_Pie_Trtc), (bom_Pr_Ti,T_set,bom_Pr_Tip),
           (bom_Pr_Ti,P_gd,bom_Pie_Ti), (bom_Pie_Trtc,T_clr,bom_Pie_Ti), (bom_Pie_Trtc,P_eod,bom_Pe_Trtc),
           (bom_Pr_Tip,T_exp,mip_Pr_Trtc), (bom_Pr_Tip,P_gd,bom_Pie_Tip), (bom_Pie_Ti,T_set,bom_Pie_Tip),
           (bom_Pie_Ti,P_eod,bom_Pe_Ti), (bom_Pe_Trtc,T_clr,bom_Pe_Ti), (bom_Pe_Trtc,P_gu,bom_Pir_Trtc),
           (mip_Pr_Trtc,P_gd,mip_Pie_Trtc), (mip_Pr_Trtc,T_clr,mip_Pr_Ti), (bom_Pie_Tip,T_exp,mip_Pie_Trtc),
           (bom_Pie_Tip,P_eod,bom_Pe_Tip), (bom_Pe_Ti,T_set,bom_Pe_Tip), (bom_Pe_Ti,P_gu,bom_Pir_Ti),
           (bom_Pir_Trtc,T_clr,bom_Pir_Ti), (bom_Pir_Trtc,P_eoa,bom_Pr_Trtc), (mip_Pie_Trtc,P_eod,mip_Pe_Trtc),
           (mip_Pie_Trtc,T_clr,mip_Pie_Ti), (mip_Pr_Ti,P_gd,mip_Pie_Ti), (mip_Pr_Ti,T_set,mip_Pr_Tip),
           (bom_Pe_Tip,T_exp,mip_Pe_Trtc), (bom_Pe_Tip,P_gu,bom_Pir_Tip), (bom_Pir_Ti,T_set,bom_Pir_Tip),
           (bom_Pir_Ti,P_eoa,bom_Pr_Ti), (mip_Pe_Trtc,P_gu,mip_Pir_Trtc), (mip_Pe_Trtc,T_clr,mip_Pe_Ti),
           (mip_Pie_Ti,P_eod,mip_Pe_Ti), (mip_Pie_Ti,T_set,mip_Pie_Tip), (mip_Pr_Tip,P_gd,mip_Pie_Tip),
           (mip_Pr_Tip,T_exp,mip_Pr_Trtc), (bom_Pir_Tip,T_exp,mip_Pir_Trtc), (bom_Pir_Tip,P_eoa,bom_Pr_Tip),
           (mip_Pir_Trtc,P_eoa,eom_Pr_Trtc), (mip_Pir_Trtc,T_clr,mip_Pir_Ti), (mip_Pe_Ti,P_gu,mip_Pir_Ti),
           (mip_Pe_Ti,T_set,mip_Pe_Tip), (mip_Pie_Tip,P_eod,mip_Pe_Tip), (mip_Pie_Tip,T_exp,mip_Pie_Trtc),
           (eom_Pr_Trtc,rst,bom_Pr_Trtc), (eom_Pr_Trtc,T_clr,eom_Pr_Ti), (eom_Pr_Trtc,P_gd,eom_Pie_Trtc),
           (mip_Pir_Ti,P_eoa,eom_Pr_Ti), (mip_Pir_Ti,T_set,mip_Pir_Tip), (mip_Pe_Tip,P_gu,mip_Pir_Tip),
           (mip_Pe_Tip,T_exp,mip_Pe_Trtc), (eom_Pr_Ti,rst,bom_Pr_Ti), (eom_Pr_Ti,T_set,eom_Pr_Tip),
           (eom_Pr_Ti,P_gd,eom_Pie_Ti), (eom_Pie_Trtc,rst,bom_Pie_Trtc), (eom_Pie_Trtc,T_clr,eom_Pie_Ti),
           (eom_Pie_Trtc,P_eod,eom_Pe_Trtc), (mip_Pir_Tip,P_eoa,eom_Pr_Tip), (mip_Pir_Tip,T_exp,mip_Pir_Trtc),
           (eom_Pr_Tip,rst,bom_Pr_Tip), (eom_Pr_Tip,P_gd,eom_Pie_Tip), (eom_Pr_Tip,T_exp,eom_Pr_Trtc),
           (eom_Pie_Ti,rst,bom_Pie_Ti), (eom_Pie_Ti,T_set,eom_Pie_Tip), (eom_Pie_Ti,P_eod,eom_Pe_Ti),
           (eom_Pe_Trtc,rst,bom_Pe_Trtc), (eom_Pe_Trtc,T_clr,eom_Pe_Ti), (eom_Pe_Trtc,P_gu,eom_Pir_Trtc),
           (eom_Pie_Tip,rst,bom_Pie_Tip), (eom_Pie_Tip,P_eod,eom_Pe_Tip), (eom_Pie_Tip,T_exp,eom_Pie_Trtc),
           (eom_Pe_Ti,rst,bom_Pe_Ti), (eom_Pe_Ti,T_set,eom_Pe_Tip), (eom_Pe_Ti,P_gu,eom_Pir_Ti),
           (eom_Pir_Trtc,rst,bom_Pir_Trtc), (eom_Pir_Trtc,T_clr,eom_Pir_Ti), (eom_Pir_Trtc,P_eoa,eom_Pr_Trtc),
           (eom_Pe_Tip,rst,bom_Pe_Tip), (eom_Pe_Tip,P_gu,eom_Pir_Tip), (eom_Pe_Tip,T_exp,eom_Pe_Trtc),
           (eom_Pir_Ti,rst,bom_Pir_Ti), (eom_Pir_Ti,T_set,eom_Pir_Tip), (eom_Pir_Ti,P_eoa,eom_Pr_Ti),
           (eom_Pir_Tip,rst,bom_Pir_Tip), (eom_Pir_Tip,P_eoa,eom_Pr_Tip), (eom_Pir_Tip,T_exp,eom_Pir_Trtc) },
      x_0: bom_Pr_Trtc,
      Xm: {bom_Pr_Trtc},
      ctrlEvents: {rst, P_gd, P_gu, T_clr, T_set} )
 &
 seqM = {
      (bom_Pr_Trtc,bom),(bom_Pr_Ti,bom),(bom_Pie_Trtc,bom),
      (bom_Pr_Tip,bom),(bom_Pie_Ti,bom),(bom_Pe_Trtc,bom),
      (mip_Pr_Trtc,mip),(bom_Pie_Tip,bom),(bom_Pe_Ti,bom),
      (bom_Pir_Trtc,bom),(mip_Pie_Trtc,mip),(mip_Pr_Ti,mip),
      (bom_Pe_Tip,bom),(bom_Pir_Ti,bom),(mip_Pe_Trtc,mip),
      (mip_Pie_Ti,mip),(mip_Pr_Tip,mip),(bom_Pir_Tip,bom),
      (mip_Pir_Trtc,mip),(mip_Pe_Ti,mip),(mip_Pie_Tip,mip),
      (eom_Pr_Trtc,eom),(mip_Pir_Ti,mip),(mip_Pe_Tip,mip),
      (eom_Pr_Ti,eom),(eom_Pie_Trtc,eom),(mip_Pir_Tip,mip),
      (eom_Pr_Tip,eom),(eom_Pie_Ti,eom),(eom_Pe_Trtc,eom),
      (eom_Pie_Tip,eom),(eom_Pe_Ti,eom),(eom_Pir_Trtc,eom),
      (eom_Pe_Tip,eom),(eom_Pir_Ti,eom),(eom_Pir_Tip,eom) }
 &
 probe = {
      (bom_Pr_Trtc,Pr),(bom_Pr_Ti,Pr),(bom_Pie_Trtc,Pie),
      (bom_Pr_Tip,Pr),(bom_Pie_Ti,Pie),(bom_Pe_Trtc,Pe),
      (mip_Pr_Trtc,Pr),(bom_Pie_Tip,Pie),(bom_Pe_Ti,Pe),
      (bom_Pir_Trtc,Pir),(mip_Pie_Trtc,Pie),(mip_Pr_Ti,Pr),
      (bom_Pe_Tip,Pe),(bom_Pir_Ti,Pir),(mip_Pe_Trtc,Pe),
      (mip_Pie_Ti,Pie),(mip_Pr_Tip,Pr),(bom_Pir_Tip,Pir),
      (mip_Pir_Trtc,Pir),(mip_Pe_Ti,Pe),(mip_Pie_Tip,Pie),
      (eom_Pr_Trtc,Pr),(mip_Pir_Ti,Pir),(mip_Pe_Tip,Pe),
      (eom_Pr_Ti,Pr),(eom_Pie_Trtc,Pie),(mip_Pir_Tip,Pir),
      (eom_Pr_Tip,Pr),(eom_Pie_Ti,Pie),(eom_Pe_Trtc,Pe),
      (eom_Pie_Tip,Pie),(eom_Pe_Ti,Pe),(eom_Pir_Trtc,Pir),
      (eom_Pe_Tip,Pe),(eom_Pir_Ti,Pir),(eom_Pir_Tip,Pir) }
 &
 timer = {
      (bom_Pr_Trtc,Trtc),(bom_Pr_Ti,Ti),(bom_Pie_Trtc,Trtc),
      (bom_Pr_Tip,Tip),(bom_Pie_Ti,Ti),(bom_Pe_Trtc,Trtc),
      (mip_Pr_Trtc,Trtc),(bom_Pie_Tip,Tip),(bom_Pe_Ti,Ti),
      (bom_Pir_Trtc,Trtc),(mip_Pie_Trtc,Trtc),(mip_Pr_Ti,Ti),
      (bom_Pe_Tip,Tip),(bom_Pir_Ti,Ti),(mip_Pe_Trtc,Trtc),
      (mip_Pie_Ti,Ti),(mip_Pr_Tip,Tip),(bom_Pir_Tip,Tip),
      (mip_Pir_Trtc,Trtc),(mip_Pe_Ti,Ti),(mip_Pie_Tip,Tip),
      (eom_Pr_Trtc,Trtc),(mip_Pir_Ti,Ti),(mip_Pe_Tip,Tip),
      (eom_Pr_Ti,Ti),(eom_Pie_Trtc,Trtc),(mip_Pir_Tip,Tip),
      (eom_Pr_Tip,Tip),(eom_Pie_Ti,Ti),(eom_Pe_Trtc,Trtc),
      (eom_Pie_Tip,Tip),(eom_Pe_Ti,Ti),(eom_Pir_Trtc,Trtc),
      (eom_Pe_Tip,Tip),(eom_Pir_Ti,Ti),(eom_Pir_Tip,Tip) }
 &

/*
  Implicit constraint:
    1. The probe and timer cannot proceed simultaneously.
  Standard constraint:
    2. In the beginning of the measurement sequence, the probe cannot attempt to
       retract (be in up motion).
    3. At the end of the measurement sequence, the probe cannot attempt to extend
       (be in down motion).
    4. The timer cannot be in progress except in the beginning of the measurement
       sequence.
  Explicit constraint:
    5. The micrometer can only be reset at the end of the measurement sequence when
       the probe is completely retracted.
       The event "rst" can only occur if the value of the state variable is <eom, _, _>.
*/
 BadStates =
// The probe is not completely extended and the timer is idle or in progress:
  {x | x : States & probe(x) : {Pie,Pir,Pr} & timer(x) : {Ti,Tip}} \/
// In the beginning of the measurement sequence, the probe is in retraction:
  {x | x : States & seqM(x) = bom & probe(x) = Pir} \/
// At the end of the measurement sequence, the probe is in extension:
  {x | x : States & seqM(x) = eom & probe(x) = Pie} \/
// Not in the beginning of the measurement sequence and the timer is in progress:
  {x | x : States & seqM(x) : {mip,eom} & timer(x) = Tip}
 &
 Qpred = States - BadStates
END

/*
OUTPUT:
The transition structure of the agent
 (i.e.,the solution of the control problem)
Number of states of the exhaustive model: 36
Number of transitions of the exhaustive model: 84
Cardinality of the predicate: 14
Supremal coreachable and controllable predicates (8):
{bom_Pr_Trtc,bom_Pie_Trtc,bom_Pe_Trtc,bom_Pe_Ti,
 bom_Pe_Tip,mip_Pe_Trtc,mip_Pir_Trtc,eom_Pr_Trtc}
Disabled events: 
{(bom_Pr_Trtc|->{T_clr}),(bom_Pie_Trtc|->{T_clr}),(bom_Pe_Trtc|->{P_gu}),
 (bom_Pe_Ti|->{P_gu}),(bom_Pe_Tip|->{P_gu}),(mip_Pe_Trtc|->{T_clr}),
 (mip_Pir_Trtc|->{T_clr}),(eom_Pr_Trtc|->{P_gd,T_clr})}
Enabled events:
{(bom_Pr_Trtc|->{P_gd}),(bom_Pie_Trtc|->{P_eod}),(bom_Pe_Trtc|->{T_clr}),
 (bom_Pe_Ti|->{T_set}),(bom_Pe_Tip|->{T_exp}),(mip_Pe_Trtc|->{P_gu}),
 (mip_Pir_Trtc|->{P_eoa}),(eom_Pr_Trtc|->{rst})}
Closed-loop system:
 rec(Xm:{bom_Pr_Trtc},
     ctrlEvents:{rst,P_gd,P_gu,T_clr,T_set},
     events:{rst,P_eoa,T_exp,P_eod,P_gd,P_gu,T_clr,T_set},
     states:{bom_Pr_Trtc,bom_Pie_Trtc,bom_Pe_Trtc,bom_Pe_Ti,
             bom_Pe_Tip,mip_Pe_Trtc,mip_Pir_Trtc,eom_Pr_Trtc},
     tf:{((bom_Pr_Trtc|->P_gd)|->bom_Pie_Trtc),
         ((bom_Pie_Trtc|->P_eod)|->bom_Pe_Trtc),
         ((bom_Pe_Trtc|->T_clr)|->bom_Pe_Ti),
         ((bom_Pe_Ti|->T_set)|->bom_Pe_Tip),
         ((bom_Pe_Tip|->T_exp)|->mip_Pe_Trtc),
         ((mip_Pe_Trtc|->P_gu)|->mip_Pir_Trtc),
         ((mip_Pir_Trtc|->P_eoa)|->eom_Pr_Trtc),
         ((eom_Pr_Trtc|->rst)|->bom_Pr_Trtc)},
     x_0:bom_Pr_Trtc)
*/

/*
  ControlLaw(rst) = eom
  ControlLaw(P_gd)= bom & Pr
  ControlLaw(P_gu) = mip & Pe
  ControlLaw(T_clr) = bom & Pe & Trtc
  ControlLaw(T_set) = Ti
*/

/*
% Runtime for SOLUTION for SETUP_CONSTANTS: 100 ms (walltime: 100 ms)
% Finished searching for valid initial states 120 ms walltime (120 ms runtime), since start: 1770 ms
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
finished_random_animate(5,[runtime/120,total_runtime/120,walltime/130])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 300 ms walltime (290 ms runtime), since start: 1900 ms

% Finished process_file (errors=0, warnings=0)
*/