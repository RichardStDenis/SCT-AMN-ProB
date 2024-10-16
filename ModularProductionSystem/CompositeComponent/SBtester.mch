// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBtester

SETS
 States = {Pr_Ti, Pie_Ti, Pe_Ti,
           Pie_Tip, Pir_Ti, Pe_Tip,
           Pie_Trtc, Pir_Tip, Pe_Trtc,
           Pir_Trtc, Pr_Tip, Pr_Trtc };
 Events = {P_cd, P_eod, P_eoa, P_gd, P_gu, T_clr, T_rst, T_set, T_exp};
 LocalStates = {Pe, Pie, Pir, Pr, Ti, Tip, Trtc}

CONSTANTS
 G_FB,
 probe, timer,
 Qpred, BadStates

PROPERTIES
 G_FB =rec(   // Automaton modeling the free behavior
      states: States,
      events: Events,
      tf: {(Pr_Ti,P_gd,Pie_Ti), (Pie_Ti,P_eod,Pe_Ti), (Pie_Ti,T_set,Pie_Tip),
           (Pe_Ti,P_gu,Pir_Ti), (Pie_Tip,P_eod,Pe_Tip), (Pie_Tip,T_exp,Pie_Trtc),
           (Pir_Ti,P_eoa,Pr_Ti), (Pe_Tip,P_gu,Pir_Tip), (Pe_Tip,T_exp,Pe_Trtc),
           (Pe_Tip,T_rst,Pe_Ti), (Pie_Trtc,P_eod,Pe_Trtc), (Pie_Trtc,P_cd,Pir_Trtc),
           (Pir_Tip,P_eoa,Pr_Tip), (Pir_Tip,T_exp,Pir_Trtc), (Pe_Trtc,P_gu,Pir_Trtc),
           (Pe_Trtc,T_clr,Pe_Ti), (Pir_Trtc,P_eoa,Pr_Trtc), (Pir_Trtc,T_clr,Pir_Ti),
           (Pr_Tip,T_exp,Pr_Trtc), (Pr_Trtc,T_clr,Pr_Ti) },
      x_0: Pr_Ti,
      Xm: {Pr_Ti},
      ctrlEvents: {P_cd, P_gd, P_gu, T_clr, T_rst, T_set} )
 &
 probe = {
      (Pr_Ti,Pr),(Pie_Ti,Pie),(Pe_Ti,Pe),
      (Pie_Tip,Pie),(Pir_Ti,Pir),(Pe_Tip,Pe),
      (Pie_Trtc,Pie),(Pir_Tip,Pir),(Pe_Trtc,Pe),
      (Pir_Trtc,Pir),(Pr_Tip,Pr),(Pr_Trtc,Pr) }
 &
 timer = {
      (Pr_Ti,Ti),(Pie_Ti,Ti),(Pe_Ti,Ti),
      (Pie_Tip,Tip),(Pir_Ti,Ti),(Pe_Tip,Tip),
      (Pie_Trtc,Trtc),(Pir_Tip,Tip),(Pe_Trtc,Trtc),
      (Pir_Trtc,Trtc),(Pr_Tip,Tip),(Pr_Trtc,Trtc) }
 &

/*
  Explicit constraints:
    1. The timer can only be enabled when the probe is in extension.
       The event T_set can only occur if the value of the state variable is <Pie, _>.
    2. The timer can be reset when the the probe is completely extended.
       The event T_rst can only occur if the value of the state variable is <Pe ,_>.
    3. The timer can only be cleared once a diagnostic has been issued.
       The event T_clr can only occur if the value of the state variable is <Pe|Pir|Pr, _>.
    4. Starting a test can be done when the timer idle.
       The event P_gd can only occur if the value of the state variable is <_, Ti>.
    5. A failure diagnostic can be issued once the timer interval is expired.
       The event P_cd can only occur if the value of the state variable is <_, Trtc>.
  Standard constraint:
    6. The tester cannot attempt to enable the timer when the probe is completely
       retracted or in retraction.
*/
 BadStates =
// The probe is in retraction or completely retracted and the timer is in progress:
    {x | x : States &  probe(x) : {Pir,Pr} & timer(x) = Tip}
 &
 Qpred = States - BadStates
END

/*
OUTPUT:
The transition structure of the agent
 (i.e.,the solution of the control problem)
Number of states of the exhaustive model: 12
Number of transitions of the exhaustive model: 20
Cardinality of the predicate: 10
Supremal coreachable and controllable predicates (10):
{Pr_Ti,Pie_Ti,Pe_Ti,Pie_Tip,Pir_Ti,Pe_Tip,Pie_Trtc,Pe_Trtc,Pir_Trtc,Pr_Trtc}
Disabled events:
{(Pr_Ti|->{}),(Pie_Ti|->{}),(Pe_Ti|->{}),(Pie_Tip|->{}),(Pir_Ti|->{}),
 (Pe_Tip|->{P_gu}),(Pie_Trtc|->{}),(Pe_Trtc|->{}),(Pir_Trtc|->{}),(Pr_Trtc|->{})}
Enabled events:
{(Pr_Ti|->{P_gd}),(Pie_Ti|->{P_eod,T_set}),(Pe_Ti|->{P_gu}),
 (Pie_Tip|->{P_eod,T_exp}),(Pir_Ti|->{P_eoa}),(Pe_Tip|->{T_rst,T_exp}),
 (Pie_Trtc|->{P_cd,P_eod}),(Pe_Trtc|->{P_gu,T_clr}),(Pir_Trtc|->{P_eoa,T_clr}),
 (Pr_Trtc|->{T_clr})}
Closed-loop system:
rec(Xm:{Pr_Ti},
    ctrlEvents:{P_cd,P_gd,P_gu,T_clr,T_rst,T_set},
    events:{P_cd,P_eod,P_eoa,P_gd,P_gu,T_clr,T_rst,T_set,T_exp},
    states:{Pr_Ti,Pie_Ti,Pe_Ti,Pie_Tip,Pir_Ti,Pe_Tip,Pie_Trtc,Pe_Trtc,Pir_Trtc,Pr_Trtc},
    tf:{((Pr_Ti|->P_gd)|->Pie_Ti),((Pie_Ti|->P_eod)|->Pe_Ti),
        ((Pie_Ti|->T_set)|->Pie_Tip),((Pe_Ti|->P_gu)|->Pir_Ti),
        ((Pie_Tip|->P_eod)|->Pe_Tip),((Pie_Tip|->T_exp)|->Pie_Trtc),
        ((Pir_Ti|->P_eoa)|->Pr_Ti),((Pe_Tip|->T_rst)|->Pe_Ti),
        ((Pe_Tip|->T_exp)|->Pe_Trtc),((Pie_Trtc|->P_cd)|->Pir_Trtc),
        ((Pie_Trtc|->P_eod)|->Pe_Trtc),((Pe_Trtc|->P_gu)|->Pir_Trtc),
        ((Pe_Trtc|->T_clr)|->Pe_Ti),((Pir_Trtc|->P_eoa)|->Pr_Trtc),
        ((Pir_Trtc|->T_clr)|->Pir_Ti),((Pr_Trtc|->T_clr)|->Pr_Ti)},
     x_0:Pr_Ti)
*/

/*
  ControlLaw(P_cd) = Pie & Trtc
  ControlLaw(P_gd) = Pr & Ti
  ControlLaw(P_gu) = Pe & (Ti | Trtc)
  ControlLaw(T_clr) = (Pe | Pir | Pr) & Trtc
  ControlLaw(T_rst) = Pe & Tip
  ControlLaw(T_set) = Pie & Ti
*/

/*
% Runtime for SOLUTION for SETUP_CONSTANTS: 100 ms (walltime: 100 ms)
% Finished searching for valid initial states 120 ms walltime (110 ms runtime), since start: 1740 ms
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
finished_random_animate(5,[runtime/100,total_runtime/100,walltime/110])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 270 ms walltime (260 ms runtime), since start: 1850 ms

% Finished process_file (errors=0, warnings=0)
*/