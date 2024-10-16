// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBcrane_lr2_f_w

SETS
 States = {idle_CatLnWp, rjt_CatLnWp, idle_CatLaEpWo,
           trf_CatLnWp, rjt_CatLaEpWo, idle_CatLaEp,
           idle_CatLa, trf_CatLaEpWo, rjt_CatLaEp,
           rjt_CatLa, idle_CatRaWp, trf_CatLaEp,
           trf_CatLa, rjt_CatRaWp, idle_CatRaRp,
           idle_CatRaTp, trf_CatRaWp, rjt_CatRaRp,
           rjt_CatRaTp, trf_CatRaRp, trf_CatRaTp };
 Events = {reject, transfer, C_eoa, C_eor, C_eot, C_abort, C_confirm, C_eog, C_grab, C_reject, C_transfer};
 LocalStates = {rjt, trf, idle, CatLa, CatLaEp, CatLnWp, CatLaEpWo, CatRaRp, CatRaTp, CatRaWp}

CONSTANTS
 G_FB,
 seqC, crane,
 Qpred, BadStates

PROPERTIES
 G_FB =rec(   // Automaton modeling the free behavior
      states: States,
      events: Events,
      tf: {(idle_CatLnWp,reject,rjt_CatLnWp), (idle_CatLnWp,C_grab,idle_CatLaEpWo), (idle_CatLnWp,transfer,trf_CatLnWp),
           (rjt_CatLnWp,C_grab,rjt_CatLaEpWo), (idle_CatLaEpWo,reject,rjt_CatLaEpWo), (idle_CatLaEpWo,C_confirm,idle_CatLaEp),
           (idle_CatLaEpWo,C_abort,idle_CatLa), (idle_CatLaEpWo,transfer,trf_CatLaEpWo), (trf_CatLnWp,C_grab,trf_CatLaEpWo),
           (rjt_CatLaEpWo,C_confirm,rjt_CatLaEp), (rjt_CatLaEpWo,C_abort,rjt_CatLa), (idle_CatLaEp,reject,rjt_CatLaEp),
           (idle_CatLaEp,C_eog,idle_CatRaWp), (idle_CatLaEp,transfer,trf_CatLaEp), (idle_CatLa,reject,rjt_CatLa),
           (idle_CatLa,transfer,trf_CatLa), (idle_CatLa,C_eoa,idle_CatLnWp), (trf_CatLaEpWo,C_confirm,trf_CatLaEp),
           (trf_CatLaEpWo,C_abort,trf_CatLa), (rjt_CatLaEp,C_eog,rjt_CatRaWp), (rjt_CatLa,C_eoa,idle_CatLnWp),
           (idle_CatRaWp,reject,rjt_CatRaWp), (idle_CatRaWp,C_reject,idle_CatRaRp), (idle_CatRaWp,C_transfer,idle_CatRaTp),
           (idle_CatRaWp,transfer,trf_CatRaWp), (trf_CatLaEp,C_eog,trf_CatRaWp), (trf_CatLa,C_eoa,idle_CatLnWp),
           (rjt_CatRaWp,C_reject,rjt_CatRaRp), (rjt_CatRaWp,C_transfer,rjt_CatRaTp), (idle_CatRaRp,reject,rjt_CatRaRp),
           (idle_CatRaRp,transfer,trf_CatRaRp), (idle_CatRaRp,C_eor,idle_CatLnWp), (idle_CatRaTp,reject,rjt_CatRaTp),
           (idle_CatRaTp,transfer,trf_CatRaTp), (idle_CatRaTp,C_eot,idle_CatLnWp), (trf_CatRaWp,C_reject,trf_CatRaRp),
           (trf_CatRaWp,C_transfer,trf_CatRaTp), (rjt_CatRaRp,C_eor,idle_CatLnWp), (rjt_CatRaTp,C_eot,rjt_CatLnWp),
           (trf_CatRaRp,C_eor,trf_CatLnWp), (trf_CatRaTp,C_eot,idle_CatLnWp) },
      x_0: idle_CatLnWp,
      Xm: {idle_CatLnWp},
      ctrlEvents: {reject, transfer, C_confirm, C_grab, C_reject, C_transfer} )
 &
 seqC = {
      (idle_CatLnWp,idle),(rjt_CatLnWp,rjt),(idle_CatLaEpWo,idle),
      (trf_CatLnWp,trf),(rjt_CatLaEpWo,rjt),(idle_CatLaEp,idle),
      (idle_CatLa,idle),(trf_CatLaEpWo,trf),(rjt_CatLaEp,rjt),
      (rjt_CatLa,rjt),(idle_CatRaWp,idle),(trf_CatLaEp,trf),
      (trf_CatLa,trf),(rjt_CatRaWp,rjt),(idle_CatRaRp,idle),
      (idle_CatRaTp,idle),(trf_CatRaWp,trf),(rjt_CatRaRp,rjt),
      (rjt_CatRaTp,rjt),(trf_CatRaRp,trf),(trf_CatRaTp,trf) }
 &
 crane = {
      (idle_CatLnWp,CatLnWp),(rjt_CatLnWp,CatLnWp),(idle_CatLaEpWo,CatLaEpWo),
      (trf_CatLnWp,CatLnWp),(rjt_CatLaEpWo,CatLaEpWo),(idle_CatLaEp,CatLaEp),
      (idle_CatLa,CatLa),(trf_CatLaEpWo,CatLaEpWo),(rjt_CatLaEp,CatLaEp),
      (rjt_CatLa,CatLa),(idle_CatRaWp,CatRaWp),(trf_CatLaEp,CatLaEp),
      (trf_CatLa,CatLa),(rjt_CatRaWp,CatRaWp),(idle_CatRaRp,CatRaRp),
      (idle_CatRaTp,CatRaTp),(trf_CatRaWp,CatRaWp),(rjt_CatRaRp,CatRaRp),
      (rjt_CatRaTp,CatRaTp),(trf_CatRaRp,CatRaRp),(trf_CatRaTp,CatRaTp) }
 &

/*
  Explicit constraints:
    1. The reject command can only be issued when the crane is idle.
       The event "reject" can only occur if the value of the state variable is <idle, _>.
    2  The transfer command can only be issued when the crane is idle.
       The event "transfer" can only occur if the value of the state variable is <idle, _>.
  Standard constraints:
    3. Abortion, reject, or transfer cannot occur without a command.
    4. Transfer cannot occur when the crane attempts to reject a piece.
    5. Rejection cannot occur when the crane attempts to transfer a piece.
*/
 BadStates =
// The crane is idle and the crane is at left in abortion or
//  at right above an output position:
    {x | x : States & seqC(x) = idle & crane(x) : {CatLa,CatRaRp,CatRaTp}} \/
// The crane is rejecting a piece and at right above the transfer position:
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
Number of states of the exhaustive model: 21
Number of transitions of the exhaustive model: 41
Cardinality of the predicate: 16
Supremal coreachable and controllable predicates (13):
 {idle_CatLnWp,rjt_CatLnWp,trf_CatLnWp,rjt_CatLaEpWo,
  trf_CatLaEpWo,rjt_CatLaEp,rjt_CatLa,trf_CatLaEp,
  trf_CatLa,rjt_CatRaWp,trf_CatRaWp,rjt_CatRaRp,
  trf_CatRaTp}
Disabled events:
 {(idle_CatLnWp|->{C_grab}),(rjt_CatLnWp|->{}),(trf_CatLnWp|->{}),
  (rjt_CatLaEpWo|->{}),(trf_CatLaEpWo|->{}),(rjt_CatLaEp|->{}),
  (rjt_CatLa|->{}),(trf_CatLaEp|->{}),(trf_CatLa|->{}),(rjt_CatRaWp|->{C_transfer}),
  (trf_CatRaWp|->{C_reject}),(rjt_CatRaRp|->{}),(trf_CatRaTp|->{})}
Enabled events:
 {(idle_CatLnWp|->{reject,transfer}),(rjt_CatLnWp|->{C_grab}),(trf_CatLnWp|->{C_grab}),
  (rjt_CatLaEpWo|->{C_abort,C_confirm}),(trf_CatLaEpWo|->{C_abort,C_confirm}),
  (rjt_CatLaEp|->{C_eog}),(rjt_CatLa|->{C_eoa}),(trf_CatLaEp|->{C_eog}),
  (trf_CatLa|->{C_eoa}),(rjt_CatRaWp|->{C_reject}),(trf_CatRaWp|->{C_transfer}),
  (rjt_CatRaRp|->{C_eor}),(trf_CatRaTp|->{C_eot})}
Closed-loop system:
 rec(Xm:{idle_CatLnWp},
 ctrlEvents:{reject,transfer,C_confirm,C_grab,C_reject,C_transfer},
 events:{reject,transfer,
         C_eoa,C_eor,C_eot,C_abort,C_confirm,C_eog,C_grab,C_reject,C_transfer},
 states:{idle_CatLnWp,rjt_CatLnWp,trf_CatLnWp,rjt_CatLaEpWo,
         trf_CatLaEpWo,rjt_CatLaEp,rjt_CatLa,trf_CatLaEp,
         trf_CatLa,rjt_CatRaWp,trf_CatRaWp,rjt_CatRaRp,
         trf_CatRaTp},
 tf:{((idle_CatLnWp|->reject)|->rjt_CatLnWp),
     ((idle_CatLnWp|->transfer)|->trf_CatLnWp),
     ((rjt_CatLnWp|->C_grab)|->rjt_CatLaEpWo),
     ((trf_CatLnWp|->C_grab)|->trf_CatLaEpWo),
     ((rjt_CatLaEpWo|->C_abort)|->rjt_CatLa),
     ((rjt_CatLaEpWo|->C_confirm)|->rjt_CatLaEp),
     ((trf_CatLaEpWo|->C_abort)|->trf_CatLa),
     ((trf_CatLaEpWo|->C_confirm)|->trf_CatLaEp),
     ((rjt_CatLaEp|->C_eog)|->rjt_CatRaWp),
     ((rjt_CatLa|->C_eoa)|->idle_CatLnWp),
     ((trf_CatLaEp|->C_eog)|->trf_CatRaWp),
     ((trf_CatLa|->C_eoa)|->idle_CatLnWp),
     ((rjt_CatRaWp|->C_reject)|->rjt_CatRaRp),
     ((trf_CatRaWp|->C_transfer)|->trf_CatRaTp),
     ((rjt_CatRaRp|->C_eor)|->idle_CatLnWp),
     ((trf_CatRaTp|->C_eot)|->idle_CatLnWp)},
     x_0:idle_CatLnWp)
*/

/*
  ControlLaw(reject) = idle
  ControlLaw(transfert) = idle
  ControlLaw(C_confirm) = (rjt | trf) & CatLaEpWo
  ControlLaw(C_grab) = (rjt | trf) & CatLnWp
  ControlLaw(C_reject) = rjt & CatRaWp
  ControlLaw(C_transfer) = trf & CatRaWp
*/

/*
% Runtime for SOLUTION for SETUP_CONSTANTS: 100 ms (walltime: 100 ms)
% Finished searching for valid initial states 110 ms walltime (110 ms runtime), since start: 1740 ms
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
% Finished processing file after 290 ms walltime (270 ms runtime), since start: 1870 ms

% Finished process_file (errors=0, warnings=0)
*/