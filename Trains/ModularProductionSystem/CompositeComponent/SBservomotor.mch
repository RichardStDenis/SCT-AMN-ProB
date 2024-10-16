// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBservomotor

SETS
 States = {rfm123_Ti, im123_Ti, rfm123_Tip,
           im123_Tip, rfm0_Trtc, im0_Trtc,
           rfm0_Ti, im0_Ti, rfm0_Tip,
           im0_Tip, rfm123_Trtc, im123_Trtc };
 Events = {mt0, mt1, mt2, mt3, T_exp, T_clr, T_set};
 LocalStates = {im0, im123, rfm0, rfm123, Ti, Tip, Trtc}

CONSTANTS
 G_FB,
 seqSM, timer,
 Qpred, BadStates

PROPERTIES
 G_FB =rec(   // Automaton modeling the free behavior
      states: States,
      events: Events,
      tf: {(rfm123_Ti,mt1,im123_Ti), (rfm123_Ti,T_set,rfm123_Tip), (rfm123_Ti,mt2,im123_Ti),
           (rfm123_Ti,mt3,im123_Ti), (im123_Ti,T_set,im123_Tip), (rfm123_Tip,mt1,im123_Tip),
           (rfm123_Tip,mt2,im123_Tip), (rfm123_Tip,mt3,im123_Tip), (im123_Tip,T_exp,rfm0_Trtc),
           (rfm0_Trtc,mt0,im0_Trtc), (rfm0_Trtc,T_clr,rfm0_Ti), (rfm0_Ti,mt0,im0_Ti),
           (rfm0_Ti,T_set,rfm0_Tip), (im0_Ti,T_set,im0_Tip), (rfm0_Tip,mt0,im0_Tip),
           (im0_Tip,T_exp,rfm123_Trtc), (rfm123_Trtc,mt1,im123_Trtc), (rfm123_Trtc,mt2,im123_Trtc),
           (rfm123_Trtc,mt3,im123_Trtc), (rfm123_Trtc,T_clr,rfm123_Ti) },
      x_0: rfm123_Ti,
      Xm: {rfm123_Ti},
      ctrlEvents: {mt0, mt1, mt2, mt3, T_clr, T_set} )
 &
 seqSM = {
      (rfm123_Ti,rfm123),(im123_Ti,im123),(rfm123_Tip,rfm123),
      (im123_Tip,im123),(rfm0_Trtc,rfm0),(im0_Trtc,im0),
      (rfm0_Ti,rfm0),(im0_Ti,im0),(rfm0_Tip,rfm0),
      (im0_Tip,im0),(rfm123_Trtc,rfm123),(im123_Trtc,im123) }
 &
 timer = {
      (rfm123_Ti,Ti),(im123_Ti,Ti),(rfm123_Tip,Tip),
      (im123_Tip,Tip),(rfm0_Trtc,Trtc),(im0_Trtc,Trtc),
      (rfm0_Ti,Ti),(im0_Ti,Ti),(rfm0_Tip,Tip),
      (im0_Tip,Tip),(rfm123_Trtc,Trtc),(im123_Trtc,Trtc) }
 &
/*
  Explicit constraint:
    1. The timer must be cleared before a positioning command is issued.
       The event T_clr can only occur if the value of the state variable
       is <rfm0|rfm123|, _>.
  Standard constraint:
    2. The timer can only be in progress after a positioning command has been issued.
*/
 BadStates =
// The servo motor is ready to move in a position and the timer is in progress:
    {x | x : States & seqSM(x) : {rfm0,rfm123} & timer(x) = Tip}
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
Supremal coreachable and controllable predicates (8):
{rfm123_Ti,im123_Ti,im123_Tip,rfm0_Trtc,rfm0_Ti,im0_Ti,im0_Tip,rfm123_Trtc}
Disabled events:
{(rfm123_Ti|->{T_set}),(im123_Ti|->{}),(im123_Tip|->{}),(rfm0_Trtc|->{mt0}),
 (rfm0_Ti|->{T_set}),(im0_Ti|->{}),(im0_Tip|->{}),(rfm123_Trtc|->{mt1,mt2,mt3})}
Enabled events:
{(rfm123_Ti|->{mt1,mt2,mt3}),(im123_Ti|->{T_set}),(im123_Tip|->{T_exp}),
 (rfm0_Trtc|->{T_clr}),(rfm0_Ti|->{mt0}),(im0_Ti|->{T_set}),(im0_Tip|->{T_exp}),
 (rfm123_Trtc|->{T_clr})}
Closed-loop system:
rec(Xm:{rfm123_Ti},
    ctrlEvents:{mt0,mt1,mt2,mt3,T_clr,T_set},
    events:{mt0,mt1,mt2,mt3,T_exp,T_clr,T_set},
    states:{rfm123_Ti,im123_Ti,im123_Tip,rfm0_Trtc,rfm0_Ti,im0_Ti,im0_Tip,rfm123_Trtc},
    tf:{((rfm123_Ti|->mt1)|->im123_Ti),((rfm123_Ti|->mt2)|->im123_Ti),
        ((rfm123_Ti|->mt3)|->im123_Ti),((im123_Ti|->T_set)|->im123_Tip),
        ((im123_Tip|->T_exp)|->rfm0_Trtc),((rfm0_Trtc|->T_clr)|->rfm0_Ti),
        ((rfm0_Ti|->mt0)|->im0_Ti),((im0_Ti|->T_set)|->im0_Tip),
        ((im0_Tip|->T_exp)|->rfm123_Trtc),((rfm123_Trtc|->T_clr)|->rfm123_Ti)},
    x_0:rfm123_Ti)
*/

/*
  ControlLaw(mt0) = rfm0 & Ti
  ControlLaw(mt1) = rfm123 & Ti
  ControlLaw(mt2) = rfm123 & Ti
  ControlLaw(mt3) = rfm123 & Ti
  ControlLaw(T_clr) = Trtc
  ControlLaw(T_set) = (im0|im123) & Ti
*/

/*
% Runtime for SOLUTION for SETUP_CONSTANTS: 90 ms (walltime: 90 ms)
% Finished searching for valid initial states 120 ms walltime (120 ms runtime), since start: 1750 ms
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
% Finished processing file after 280 ms walltime (270 ms runtime), since start: 1870 ms

% Finished process_file (errors=0, warnings=0)
*/