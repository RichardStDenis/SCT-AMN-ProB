// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBcrane_br_w

SETS
 States = {w_Cidle, p1_Cbusy, p2_Cbusy,
           p3_Cbusy };
 Events = {C_eot, C_pck1, C_pck2, C_pck3};
 LocalStates = {p1, p2, p3, w, Cbusy, Cidle}

CONSTANTS
 G_FB,
 seqC, crane,
 Qpred, BadStates

PROPERTIES
 G_FB =rec(   // Automaton modeling the free behavior
      states: States,
      events: Events,
      tf: {(w_Cidle,C_pck1,p1_Cbusy), (w_Cidle,C_pck2,p2_Cbusy), (w_Cidle,C_pck3,p3_Cbusy),
           (p1_Cbusy,C_eot,w_Cidle), (p2_Cbusy,C_eot,w_Cidle), (p3_Cbusy,C_eot,w_Cidle) },
      x_0: w_Cidle,
      Xm: {w_Cidle},
      ctrlEvents: {C_pck1, C_pck2, C_pck3} )
 &
 seqC = {
      (w_Cidle,w),(p1_Cbusy,p1),(p2_Cbusy,p2),
      (p3_Cbusy,p3) }
 &
 crane = {
      (w_Cidle,Cidle),(p1_Cbusy,Cbusy),(p2_Cbusy,Cbusy),
      (p3_Cbusy,Cbusy) }
 &

/*
  No constraints
*/
 BadStates = {}
 &
 Qpred = States - BadStates
END

/*
OUTPUT:
The transition structure of the agent
 (i.e.,the solution of the control problem)
Number of states of the exhaustive model: 4
Number of transitions of the exhaustive model: 6
Cardinality of the predicate: 4
Supremal coreachable and controllable predicates (4):
{w_Cidle,p1_Cbusy,p2_Cbusy,p3_Cbusy}
Disabled events:
{(w_Cidle|->{}),(p1_Cbusy|->{}),(p2_Cbusy|->{}),(p3_Cbusy|->{})}
Enabled events:
{(w_Cidle|->{C_pck1,C_pck2,C_pck3}),(p1_Cbusy|->{C_eot}),
 (p2_Cbusy|->{C_eot}),(p3_Cbusy|->{C_eot})}
Closed-loop system:
 rec(Xm:{w_Cidle},
     ctrlEvents:{C_pck1,C_pck2,C_pck3},
     events:{C_eot,C_pck1,C_pck2,C_pck3},
     states:{w_Cidle,p1_Cbusy,p2_Cbusy,p3_Cbusy},
     tf:{((w_Cidle|->C_pck1)|->p1_Cbusy),
         ((w_Cidle|->C_pck2)|->p2_Cbusy),
         ((w_Cidle|->C_pck3)|->p3_Cbusy),
         ((p1_Cbusy|->C_eot)|->w_Cidle),
         ((p2_Cbusy|->C_eot)|->w_Cidle),
         ((p3_Cbusy|->C_eot)|->w_Cidle)},
     x_0:w_Cidle)
*/

/*
  ControlLaw(C_pck1) = w_Cidle
  ControlLaw(C_pck2) = w_Cidle
  ControlLaw(C_pck2) = w_Cidle
*/

/*
% Runtime for SOLUTION for SETUP_CONSTANTS: 40 ms (walltime: 50 ms)
% Finished searching for valid initial states 70 ms walltime (70 ms runtime), since start: 1670 ms
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
finished_random_animate(5,[runtime/140,total_runtime/140,walltime/150])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 270 ms walltime (260 ms runtime), since start: 1830 ms

% Finished process_file (errors=0, warnings=0)
*/