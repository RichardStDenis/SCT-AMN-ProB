// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBconveyor_ts

SETS
 States = {p1_Mnm_Trtc, p1_Mim_Trtc, p1_Mnm_Ti,
           p2_Mnm_Trtc, p1_Mim_Ti, p1_Mnm_Tip,
           p2_Mnm_Ti, p2_Mim_Trtc, p1_Mim_Tip,
           p2_Mnm_Tip, p2_Mim_Ti, p2_Mim_Tip };
 Events = {M_eos, T_exp, M_step, T_clr, T_set};
 LocalStates = {p1, p2, Mim, Mnm, Ti, Tip, Trtc}

CONSTANTS
 G_FB,
 seqC, motor, timer,
 Qpred, BadStates

PROPERTIES
 G_FB =rec(   // Automaton modeling the free behavior
      states: States,
      events: Events,
      tf: {(p1_Mnm_Trtc,M_step,p1_Mim_Trtc), (p1_Mnm_Trtc,T_clr,p1_Mnm_Ti), (p1_Mim_Trtc,M_eos,p2_Mnm_Trtc),
           (p1_Mim_Trtc,T_clr,p1_Mim_Ti), (p1_Mnm_Ti,M_step,p1_Mim_Ti), (p1_Mnm_Ti,T_set,p1_Mnm_Tip),
           (p2_Mnm_Trtc,T_clr,p2_Mnm_Ti), (p2_Mnm_Trtc,M_step,p2_Mim_Trtc), (p1_Mim_Ti,M_eos,p2_Mnm_Ti),
           (p1_Mim_Ti,T_set,p1_Mim_Tip), (p1_Mnm_Tip,M_step,p1_Mim_Tip), (p1_Mnm_Tip,T_exp,p1_Mnm_Trtc),
           (p2_Mnm_Ti,T_set,p2_Mnm_Tip), (p2_Mnm_Ti,M_step,p2_Mim_Ti), (p2_Mim_Trtc,T_clr,p2_Mim_Ti),
           (p2_Mim_Trtc,M_eos,p2_Mnm_Trtc), (p1_Mim_Tip,M_eos,p2_Mnm_Tip), (p1_Mim_Tip,T_exp,p1_Mim_Trtc),
           (p2_Mnm_Tip,T_exp,p1_Mnm_Trtc), (p2_Mnm_Tip,M_step,p2_Mim_Tip), (p2_Mim_Ti,T_set,p2_Mim_Tip),
           (p2_Mim_Ti,M_eos,p2_Mnm_Ti), (p2_Mim_Tip,T_exp,p1_Mim_Trtc), (p2_Mim_Tip,M_eos,p2_Mnm_Tip) },
      x_0: p1_Mnm_Trtc,
      Xm: {p1_Mnm_Trtc},
      ctrlEvents: {M_step, T_clr, T_set} )
 &
 seqC = {
      (p1_Mnm_Trtc,p1),(p1_Mim_Trtc,p1),(p1_Mnm_Ti,p1),
      (p2_Mnm_Trtc,p2),(p1_Mim_Ti,p1),(p1_Mnm_Tip,p1),
      (p2_Mnm_Ti,p2),(p2_Mim_Trtc,p2),(p1_Mim_Tip,p1),
      (p2_Mnm_Tip,p2),(p2_Mim_Ti,p2),(p2_Mim_Tip,p2) }
 &
 motor = {
      (p1_Mnm_Trtc,Mnm),(p1_Mim_Trtc,Mim),(p1_Mnm_Ti,Mnm),
      (p2_Mnm_Trtc,Mnm),(p1_Mim_Ti,Mim),(p1_Mnm_Tip,Mnm),
      (p2_Mnm_Ti,Mnm),(p2_Mim_Trtc,Mim),(p1_Mim_Tip,Mim),
      (p2_Mnm_Tip,Mnm),(p2_Mim_Ti,Mim),(p2_Mim_Tip,Mim) }
 &
 timer = {
      (p1_Mnm_Trtc,Trtc),(p1_Mim_Trtc,Trtc),(p1_Mnm_Ti,Ti),
      (p2_Mnm_Trtc,Trtc),(p1_Mim_Ti,Ti),(p1_Mnm_Tip,Tip),
      (p2_Mnm_Ti,Ti),(p2_Mim_Trtc,Trtc),(p1_Mim_Tip,Tip),
      (p2_Mnm_Tip,Tip),(p2_Mim_Ti,Ti),(p2_Mim_Tip,Tip) }
 &

/*
  Standard constraints:
    1. The timer must not be in progress in the first phase of the operation sequence.
    2. The motor must not run in the second phase of the operation sequence.
*/
 BadStates =
// The conveyor is in its first phase of the operation sequence and the timer
// is in progress:
    {x | x : States & seqC(x) = p1 & timer(x) = Tip } \/
// The conveyor is in its second phase of the operation sequence and the motor
// is in motion:
    {x | x : States & seqC(x) = p2 & motor(x) = Mim }
 &
 Qpred = States - BadStates
END

/*
OUTPUT:
The transition structure of the agent
 (i.e.,the solution of the control problem)
Number of states of the exhaustive model: 12
Number of transitions of the exhaustive model: 24
Cardinality of the predicate: 7
Supremal coreachable and controllable predicates (7):
{p1_Mnm_Trtc,p1_Mim_Trtc,p1_Mnm_Ti,p2_Mnm_Trtc,p1_Mim_Ti,p2_Mnm_Ti,p2_Mnm_Tip}
Disabled events:
{(p1_Mnm_Trtc|->{}),(p1_Mim_Trtc|->{}),(p1_Mnm_Ti|->{T_set}),
(p2_Mnm_Trtc|->{M_step}),(p1_Mim_Ti|->{T_set}),(p2_Mnm_Ti|->{M_step}),
(p2_Mnm_Tip|->{M_step})}
Enabled events:
{(p1_Mnm_Trtc|->{M_step,T_clr}),(p1_Mim_Trtc|->{M_eos,T_clr}),
(p1_Mnm_Ti|->{M_step}),(p2_Mnm_Trtc|->{T_clr}),(p1_Mim_Ti|->{M_eos}),
(p2_Mnm_Ti|->{T_set}),(p2_Mnm_Tip|->{T_exp})}
Closed-loop system: 
rec(Xm:{p1_Mnm_Trtc},
    ctrlEvents:{M_step,T_clr,T_set},
    events:{M_eos,T_exp,M_step,T_clr,T_set},
    states:{p1_Mnm_Trtc,p1_Mim_Trtc,p1_Mnm_Ti,p2_Mnm_Trtc,p1_Mim_Ti,p2_Mnm_Ti,p2_Mnm_Tip},
    tf:{((p1_Mnm_Trtc|->M_step)|->p1_Mim_Trtc),
        ((p1_Mnm_Trtc|->T_clr)|->p1_Mnm_Ti),
        ((p1_Mim_Trtc|->M_eos)|->p2_Mnm_Trtc),
        ((p1_Mim_Trtc|->T_clr)|->p1_Mim_Ti),
        ((p1_Mnm_Ti|->M_step)|->p1_Mim_Ti),
        ((p2_Mnm_Trtc|->T_clr)|->p2_Mnm_Ti),
        ((p1_Mim_Ti|->M_eos)|->p2_Mnm_Ti),
        ((p2_Mnm_Ti|->T_set)|->p2_Mnm_Tip),
        ((p2_Mnm_Tip|->T_exp)|->p1_Mnm_Trtc)},
     x_0:p1_Mnm_Trtc)
*/

/*
  ControlLaw(Mstep) = p1 & Mnm
  ControlLaw(Tclr) = Trtc
  ControlLaw(Tset) = p2 & Ti
*/

/*
% Runtime for SOLUTION for SETUP_CONSTANTS: 90 ms (walltime: 90 ms)
% Finished searching for valid initial states 110 ms walltime (110 ms runtime), since start: 1190 ms
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
finished_random_animate(5,[runtime/100,total_runtime/100,walltime/120])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 270 ms walltime (260 ms runtime), since start: 1310 ms

% Finished process_file (errors=0, warnings=0)
*/