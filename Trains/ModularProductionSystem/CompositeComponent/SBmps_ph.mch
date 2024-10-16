// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBmps_ph

SETS
 States = {Pready_b30_Hidle, Punready_b30_Hidle, Pready_b30_Hbusy,
           Pready_b31_Hidle, Punready_b30_Hbusy, Punready_b31_Hidle,
           Pready_b31_Hbusy, Punready_b31_Hbusy };
 Events = {P_open, P_eoc, H_stop, H_start};
 LocalStates = {Pready, Punready, b30, b31, Hbusy, Hidle}

CONSTANTS
 G_FB,
 processing, buffer3, handling,
 Qpred, BadStates

PROPERTIES
 G_FB = rec(       // Automaton modeling the free behavior
            states: States,
            events: Events,
            tf: {(Pready_b30_Hidle,P_open,Punready_b30_Hidle), (Pready_b30_Hidle,H_start,Pready_b30_Hbusy), (Punready_b30_Hidle,P_eoc,Pready_b31_Hidle),
                 (Punready_b30_Hidle,H_start,Punready_b30_Hbusy), (Pready_b30_Hbusy,P_open,Punready_b30_Hbusy), (Pready_b30_Hbusy,H_stop,Pready_b30_Hidle),
                 (Pready_b31_Hidle,P_open,Punready_b31_Hidle), (Pready_b31_Hidle,H_start,Pready_b31_Hbusy), (Punready_b30_Hbusy,P_eoc,Pready_b31_Hbusy),
                 (Punready_b30_Hbusy,H_stop,Punready_b30_Hidle), (Punready_b31_Hidle,H_start,Punready_b31_Hbusy), (Punready_b31_Hidle,P_eoc,Pready_b31_Hidle),
                 (Pready_b31_Hbusy,P_open,Punready_b31_Hbusy), (Pready_b31_Hbusy,H_stop,Pready_b30_Hidle), (Punready_b31_Hbusy,H_stop,Punready_b30_Hidle),
                 (Punready_b31_Hbusy,P_eoc,Pready_b31_Hbusy) },
            x_0: Pready_b30_Hidle,
            Xm: {Pready_b30_Hidle},
            ctrlEvents: {P_open, H_start} )
 &
 processing = {
      (Pready_b30_Hidle,Pready),(Punready_b30_Hidle,Punready),(Pready_b30_Hbusy,Pready),
      (Pready_b31_Hidle,Pready),(Punready_b30_Hbusy,Punready),(Punready_b31_Hidle,Punready),
      (Pready_b31_Hbusy,Pready),(Punready_b31_Hbusy,Punready) }
 &
 buffer3 = {
      (Pready_b30_Hidle,b30),(Punready_b30_Hidle,b30),(Pready_b30_Hbusy,b30),
      (Pready_b31_Hidle,b31),(Punready_b30_Hbusy,b30),(Punready_b31_Hidle,b31),
      (Pready_b31_Hbusy,b31),(Punready_b31_Hbusy,b31) }
 &
 handling = {
      (Pready_b30_Hidle,Hidle),(Punready_b30_Hidle,Hidle),(Pready_b30_Hbusy,Hbusy),
      (Pready_b31_Hidle,Hidle),(Punready_b30_Hbusy,Hbusy),(Punready_b31_Hidle,Hidle),
      (Pready_b31_Hbusy,Hbusy),(Punready_b31_Hbusy,Hbusy) }
 &
 /*
  Standard constraints:
     5. The third buffer cannot be declared free when the handling station is busy,
        that is, it cannot accept a workpiece from the processing station.
     6. When the third buffer is full, the processing station cannot deliver a
        workpiece to the handling station.
*/
  BadStates =
// The third buffer is free and the handling station is busy.
  { x | x : States & buffer3(x) = b30 & handling(x) = Hbusy} \/
// The third is full and the processing station is unready.
  { x | x : States & buffer3(x) = b31 & processing(x) = Punready}
 &
 Qpred = States - BadStates
END

/*
OUTPUT:
The transition structure of the agent
 (i.e.,the solution of the control problem)
Number of states of the exhaustive model: 8
Number of transitions of the exhaustive model: 16
Cardinality of the predicate: 4
Supremal coreachable and controllable predicates (4):
{Pready_b30_Hidle,Punready_b30_Hidle,Pready_b31_Hidle,Pready_b31_Hbusy}
Disabled events:
{(Pready_b30_Hidle|->{H_start}),(Punready_b30_Hidle|->{H_start}),
 (Pready_b31_Hidle|->{P_open}),(Pready_b31_Hbusy|->{P_open})}
Enabled events:
{(Pready_b30_Hidle|->{P_open}),(Punready_b30_Hidle|->{P_eoc}),
 (Pready_b31_Hidle|->{H_start}),(Pready_b31_Hbusy|->{H_stop})}
Closed-loop system:
rec(Xm:{Pready_b30_Hidle},
    ctrlEvents:{P_open,H_start},
    events:{P_open,P_eoc,H_stop,H_start},
    states:{Pready_b30_Hidle,Punready_b30_Hidle,Pready_b31_Hidle,Pready_b31_Hbusy},
    tf:{((Pready_b30_Hidle|->P_open)|->Punready_b30_Hidle),
        ((Punready_b30_Hidle|->P_eoc)|->Pready_b31_Hidle),
        ((Pready_b31_Hidle|->H_start)|->Pready_b31_Hbusy),
        ((Pready_b31_Hbusy|->H_stop)|->Pready_b30_Hidle)},
    x_0:Pready_b30_Hidle)
*/

/*
  ControlLaw(P_open) = Pready & b30 &_Hidle
  ControlLaw(H_start) = Pready & b31 & Hidle
*/

/*
% Runtime for SOLUTION for SETUP_CONSTANTS: 90 ms (walltime: 90 ms)
% Finished searching for valid initial states 110 ms walltime (110 ms runtime), since start: 1720 ms
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
finished_random_animate(5,[runtime/110,total_runtime/110,walltime/110])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 270 ms walltime (260 ms runtime), since start: 1840 ms

% Finished process_file (errors=0, warnings=0)
*/