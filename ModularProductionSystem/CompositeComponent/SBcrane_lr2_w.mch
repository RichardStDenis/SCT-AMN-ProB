// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBcrane_lr2_w

SETS
 States = {idle_CatLnWp, rjt_CatLnWp, idle_CatLaEp,
           trf_CatLnWp, rjt_CatLaEp, idle_CatRaWp,
           trf_CatLaEp, rjt_CatRaWp, idle_CatRaRp,
           idle_CatRaTp, trf_CatRaWp, rjt_CatRaRp,
           rjt_CatRaTp, trf_CatRaRp, trf_CatRaTp };
 Events = {reject, transfer, C_eor, C_eot, C_eog, C_grab, C_reject, C_transfer};
 LocalStates = {rjt, trf, idle, CatLaEp, CatLnWp, CatRaRp, CatRaTp, CatRaWp}

CONSTANTS
 G_FB,
 seqC, crane,
 Qpred, BadStates

PROPERTIES
 G_FB =rec(   // Automaton modeling the free behavior
      states: States,
      events: Events,
      tf: {(idle_CatLnWp,reject,rjt_CatLnWp), (idle_CatLnWp,C_grab,idle_CatLaEp), (idle_CatLnWp,transfer,trf_CatLnWp),
           (rjt_CatLnWp,C_grab,rjt_CatLaEp), (idle_CatLaEp,reject,rjt_CatLaEp), (idle_CatLaEp,C_eog,idle_CatRaWp),
           (idle_CatLaEp,transfer,trf_CatLaEp), (trf_CatLnWp,C_grab,trf_CatLaEp), (rjt_CatLaEp,C_eog,rjt_CatRaWp),
           (idle_CatRaWp,reject,rjt_CatRaWp), (idle_CatRaWp,C_reject,idle_CatRaRp), (idle_CatRaWp,C_transfer,idle_CatRaTp),
           (idle_CatRaWp,transfer,trf_CatRaWp), (trf_CatLaEp,C_eog,trf_CatRaWp), (rjt_CatRaWp,C_reject,rjt_CatRaRp),
           (rjt_CatRaWp,C_transfer,rjt_CatRaTp), (idle_CatRaRp,reject,rjt_CatRaRp), (idle_CatRaRp,transfer,trf_CatRaRp),
           (idle_CatRaRp,C_eor,idle_CatLnWp), (idle_CatRaTp,reject,rjt_CatRaTp), (idle_CatRaTp,transfer,trf_CatRaTp),
           (idle_CatRaTp,C_eot,idle_CatLnWp), (trf_CatRaWp,C_reject,trf_CatRaRp), (trf_CatRaWp,C_transfer,trf_CatRaTp),
           (rjt_CatRaRp,C_eor,idle_CatLnWp), (rjt_CatRaTp,C_eot,rjt_CatLnWp), (trf_CatRaRp,C_eor,trf_CatLnWp),
           (trf_CatRaTp,C_eot,idle_CatLnWp) },
      x_0: idle_CatLnWp,
      Xm: {idle_CatLnWp},
      ctrlEvents: {reject, transfer, C_grab, C_reject, C_transfer} )
 &
 seqC = {
      (idle_CatLnWp,idle),(rjt_CatLnWp,rjt),(idle_CatLaEp,idle),
      (trf_CatLnWp,trf),(rjt_CatLaEp,rjt),(idle_CatRaWp,idle),
      (trf_CatLaEp,trf),(rjt_CatRaWp,rjt),(idle_CatRaRp,idle),
      (idle_CatRaTp,idle),(trf_CatRaWp,trf),(rjt_CatRaRp,rjt),
      (rjt_CatRaTp,rjt),(trf_CatRaRp,trf),(trf_CatRaTp,trf) }
 &
 crane = {
      (idle_CatLnWp,CatLnWp),(rjt_CatLnWp,CatLnWp),(idle_CatLaEp,CatLaEp),
      (trf_CatLnWp,CatLnWp),(rjt_CatLaEp,CatLaEp),(idle_CatRaWp,CatRaWp),
      (trf_CatLaEp,CatLaEp),(rjt_CatRaWp,CatRaWp),(idle_CatRaRp,CatRaRp),
      (idle_CatRaTp,CatRaTp),(trf_CatRaWp,CatRaWp),(rjt_CatRaRp,CatRaRp),
      (rjt_CatRaTp,CatRaTp),(trf_CatRaRp,CatRaRp),(trf_CatRaTp,CatRaTp) }
 
 &
/*
  Explicit constraints:
    1. The reject command can only be issued when the crane is idle.
       The event "reject" can only occur if the value of the state variable is <idle, _>.
    2  The transfer command can only be issued when the crane is idle.
       The event "transfer" can only occur if the value of the state variable is <idle, _>.
  Implicit constraints:
    3. The crane cannot attempt to grab a piece before a command has been issued.
  Standard constraints:
    4. Reject or transfer cannot occur without a command.
    5. Transfer cannot occur when the crane attempts to reject a piece.
    6. Rejection cannot occur when the crane attempts to transfer a piece.
*/
 BadStates =
// The crane is idle and the crane is at left above the entry position:
    {x | x : States & seqC(x) = idle & crane(x) = CatLaEp} \/
// The crane is idle and the crane is at right above an output position:
    {x | x : States & seqC(x) = idle & crane(x) : {CatRaRp,CatRaTp}} \/
// The crane is rejecting a piece and is at right above the transfer position:
    {x | x : States & seqC(x) = rjt & crane(x) = CatRaTp} \/
// The crane is transferring a piece and is at right above the rejection position:
    {x | x : States & seqC(x) = trf & crane(x) = CatRaRp}
 &
 Qpred = States - BadStates
END

/*
OUTPUT:
The transition structure of the agent
 (i.e.,the solution of the control problem)
Number of states of the exhaustive model: 15
Number of transitions of the exhaustive model: 28
Cardinality of the predicate: 10
Supremal coreachable and controllable predicates (9):
 {idle_CatLnWp,rjt_CatLnWp,trf_CatLnWp,
  rjt_CatLaEp,trf_CatLaEp,rjt_CatRaWp,
  trf_CatRaWp,rjt_CatRaRp,trf_CatRaTp}
Disabled events:
 {(idle_CatLnWp|->{C_grab}),(rjt_CatLnWp|->{}),(trf_CatLnWp|->{}),(rjt_CatLaEp|->{}),
  (trf_CatLaEp|->{}),(rjt_CatRaWp|->{C_transfer}),(trf_CatRaWp|->{C_reject}),
  (rjt_CatRaRp|->{}),(trf_CatRaTp|->{})}
Enabled events:
 {(idle_CatLnWp|->{reject,transfer}),(rjt_CatLnWp|->{C_grab}),(trf_CatLnWp|->{C_grab}),
  (rjt_CatLaEp|->{C_eog}),(trf_CatLaEp|->{C_eog}),(rjt_CatRaWp|->{C_reject}),
  (trf_CatRaWp|->{C_transfer}),(rjt_CatRaRp|->{C_eor}),(trf_CatRaTp|->{C_eot})}
Closed-loop system:
rec(Xm:{idle_CatLnWp},
    ctrlEvents:{reject,transfer,C_grab,C_reject,C_transfer},
    events:{reject,transfer,C_eor,C_eot,C_eog,C_grab,C_reject,C_transfer},
    states:{idle_CatLnWp,rjt_CatLnWp,trf_CatLnWp,
            rjt_CatLaEp,trf_CatLaEp,rjt_CatRaWp,
            trf_CatRaWp,rjt_CatRaRp,trf_CatRaTp},
    tf:{((idle_CatLnWp|->reject)|->rjt_CatLnWp),
        ((idle_CatLnWp|->transfer)|->trf_CatLnWp),
        ((rjt_CatLnWp|->C_grab)|->rjt_CatLaEp),
        ((trf_CatLnWp|->C_grab)|->trf_CatLaEp),
        ((rjt_CatLaEp|->C_eog)|->rjt_CatRaWp),
        ((trf_CatLaEp|->C_eog)|->trf_CatRaWp),
        ((rjt_CatRaWp|->C_reject)|->rjt_CatRaRp),
        ((trf_CatRaWp|->C_transfer)|->trf_CatRaTp),
        ((rjt_CatRaRp|->C_eor)|->idle_CatLnWp),
        ((trf_CatRaTp|->C_eot)|->idle_CatLnWp)},
     x_0:idle_CatLnWp)
*/

/*
  ControlLaw(reject) = idle
  ControlLaw(transfert) = idle
  ControlLaw(C_grab) = (rjt | trf) & CatLnWp
  ControlLaw(C_reject) = rjt & CatRaWp
  ControlLaw(C_transfer) = trf & CatRaWp
*/

/*
% Runtime for SOLUTION for SETUP_CONSTANTS: 100 ms (walltime: 100 ms)
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
finished_random_animate(5,[runtime/100,total_runtime/100,walltime/110])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 270 ms walltime (260 ms runtime), since start: 1860 ms

% Finished process_file (errors=0, warnings=0)
*/