// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBhook_e

SETS
 States = {grbStg_Wr_Hnv, grbStg_Wie_Hnv, grbStg_Wr_Hp,
           grbStg_We_Hnv, grbStg_Wie_Hp, grbStg_Wr_Hv,
           grbStg_Wir_Hnv, grbStg_We_Hp, grbStg_Wie_Hv,
           grbStg_Wr_Hl, drpStg_Wr_Hnv, grbStg_Wir_Hp,
           grbStg_We_Hv, grbStg_Wie_Hl, drpStg_Wie_Hnv,
           drpStg_Wr_Hp, grbStg_Wir_Hv, grbStg_We_Hl,
           drpStg_We_Hnv, drpStg_Wie_Hp, drpStg_Wr_Hv,
           grbStg_Wir_Hl, drpStg_Wir_Hnv, drpStg_We_Hp,
           drpStg_Wie_Hv, drpStg_Wr_Hl, drpStg_Wir_Hp,
           drpStg_We_Hv, drpStg_Wie_Hl, drpStg_Wir_Hv,
           drpStg_We_Hl, drpStg_Wir_Hl };
 Events = {W_eoa, W_eod, W_gd, W_gu, H_drop, H_eog, H_eod, H_grab};
 LocalStates = {grbStg, drpStg, We, Wie, Wir, Wr, Hl, Hnv, Hp, Hv}

CONSTANTS
 G_FB,
 seqH, winch, hook,
 Qpred, BadStates

PROPERTIES
 G_FB =rec(   // Automaton modeling the free behavior
      states: States,
      events: Events,
      tf: {(grbStg_Wr_Hnv,W_gd,grbStg_Wie_Hnv), (grbStg_Wr_Hnv,H_grab,grbStg_Wr_Hp), (grbStg_Wie_Hnv,W_eod,grbStg_We_Hnv),
           (grbStg_Wie_Hnv,H_grab,grbStg_Wie_Hp), (grbStg_Wr_Hp,W_gd,grbStg_Wie_Hp), (grbStg_Wr_Hp,H_eog,grbStg_Wr_Hv),
           (grbStg_We_Hnv,W_gu,grbStg_Wir_Hnv), (grbStg_We_Hnv,H_grab,grbStg_We_Hp), (grbStg_Wie_Hp,W_eod,grbStg_We_Hp),
           (grbStg_Wie_Hp,H_eog,grbStg_Wie_Hv), (grbStg_Wr_Hv,W_gd,grbStg_Wie_Hv), (grbStg_Wr_Hv,H_drop,grbStg_Wr_Hl),
           (grbStg_Wir_Hnv,W_eoa,drpStg_Wr_Hnv), (grbStg_Wir_Hnv,H_grab,grbStg_Wir_Hp), (grbStg_We_Hp,W_gu,grbStg_Wir_Hp),
           (grbStg_We_Hp,H_eog,grbStg_We_Hv), (grbStg_Wie_Hv,W_eod,grbStg_We_Hv), (grbStg_Wie_Hv,H_drop,grbStg_Wie_Hl),
           (grbStg_Wr_Hl,W_gd,grbStg_Wie_Hl), (grbStg_Wr_Hl,H_eod,grbStg_Wr_Hnv), (drpStg_Wr_Hnv,W_gd,drpStg_Wie_Hnv),
           (drpStg_Wr_Hnv,H_grab,drpStg_Wr_Hp), (grbStg_Wir_Hp,W_eoa,drpStg_Wr_Hp), (grbStg_Wir_Hp,H_eog,grbStg_Wir_Hv),
           (grbStg_We_Hv,W_gu,grbStg_Wir_Hv), (grbStg_We_Hv,H_drop,grbStg_We_Hl), (grbStg_Wie_Hl,W_eod,grbStg_We_Hl),
           (grbStg_Wie_Hl,H_eod,grbStg_Wie_Hnv), (drpStg_Wie_Hnv,W_eod,drpStg_We_Hnv), (drpStg_Wie_Hnv,H_grab,drpStg_Wie_Hp),
           (drpStg_Wr_Hp,W_gd,drpStg_Wie_Hp), (drpStg_Wr_Hp,H_eog,drpStg_Wr_Hv), (grbStg_Wir_Hv,W_eoa,drpStg_Wr_Hv),
           (grbStg_Wir_Hv,H_drop,grbStg_Wir_Hl), (grbStg_We_Hl,W_gu,grbStg_Wir_Hl), (grbStg_We_Hl,H_eod,grbStg_We_Hnv),
           (drpStg_We_Hnv,W_gu,drpStg_Wir_Hnv), (drpStg_We_Hnv,H_grab,drpStg_We_Hp), (drpStg_Wie_Hp,W_eod,drpStg_We_Hp),
           (drpStg_Wie_Hp,H_eog,drpStg_Wie_Hv), (drpStg_Wr_Hv,W_gd,drpStg_Wie_Hv), (drpStg_Wr_Hv,H_drop,drpStg_Wr_Hl),
           (grbStg_Wir_Hl,W_eoa,drpStg_Wr_Hl), (grbStg_Wir_Hl,H_eod,grbStg_Wir_Hnv), (drpStg_Wir_Hnv,W_eoa,grbStg_Wr_Hnv),
           (drpStg_Wir_Hnv,H_grab,drpStg_Wir_Hp), (drpStg_We_Hp,W_gu,drpStg_Wir_Hp), (drpStg_We_Hp,H_eog,drpStg_We_Hv),
           (drpStg_Wie_Hv,W_eod,drpStg_We_Hv), (drpStg_Wie_Hv,H_drop,drpStg_Wie_Hl), (drpStg_Wr_Hl,W_gd,drpStg_Wie_Hl),
           (drpStg_Wr_Hl,H_eod,drpStg_Wr_Hnv), (drpStg_Wir_Hp,W_eoa,grbStg_Wr_Hp), (drpStg_Wir_Hp,H_eog,drpStg_Wir_Hv),
           (drpStg_We_Hv,W_gu,drpStg_Wir_Hv), (drpStg_We_Hv,H_drop,drpStg_We_Hl), (drpStg_Wie_Hl,W_eod,drpStg_We_Hl),
           (drpStg_Wie_Hl,H_eod,drpStg_Wie_Hnv), (drpStg_Wir_Hv,W_eoa,grbStg_Wr_Hv), (drpStg_Wir_Hv,H_drop,drpStg_Wir_Hl),
           (drpStg_We_Hl,W_gu,drpStg_Wir_Hl), (drpStg_We_Hl,H_eod,drpStg_We_Hnv), (drpStg_Wir_Hl,W_eoa,grbStg_Wr_Hl),
           (drpStg_Wir_Hl,H_eod,drpStg_Wir_Hnv) },
      x_0: grbStg_Wr_Hnv,
      Xm: {grbStg_Wr_Hnv},
      ctrlEvents: {W_gd, W_gu, H_drop, H_grab} )
 &
 seqH = {
      (grbStg_Wr_Hnv,grbStg),(grbStg_Wie_Hnv,grbStg),(grbStg_Wr_Hp,grbStg),
      (grbStg_We_Hnv,grbStg),(grbStg_Wie_Hp,grbStg),(grbStg_Wr_Hv,grbStg),
      (grbStg_Wir_Hnv,grbStg),(grbStg_We_Hp,grbStg),(grbStg_Wie_Hv,grbStg),
      (grbStg_Wr_Hl,grbStg),(drpStg_Wr_Hnv,drpStg),(grbStg_Wir_Hp,grbStg),
      (grbStg_We_Hv,grbStg),(grbStg_Wie_Hl,grbStg),(drpStg_Wie_Hnv,drpStg),
      (drpStg_Wr_Hp,drpStg),(grbStg_Wir_Hv,grbStg),(grbStg_We_Hl,grbStg),
      (drpStg_We_Hnv,drpStg),(drpStg_Wie_Hp,drpStg),(drpStg_Wr_Hv,drpStg),
      (grbStg_Wir_Hl,grbStg),(drpStg_Wir_Hnv,drpStg),(drpStg_We_Hp,drpStg),
      (drpStg_Wie_Hv,drpStg),(drpStg_Wr_Hl,drpStg),(drpStg_Wir_Hp,drpStg),
      (drpStg_We_Hv,drpStg),(drpStg_Wie_Hl,drpStg),(drpStg_Wir_Hv,drpStg),
      (drpStg_We_Hl,drpStg),(drpStg_Wir_Hl,drpStg) }
 &
 winch = {
      (grbStg_Wr_Hnv,Wr),(grbStg_Wie_Hnv,Wie),(grbStg_Wr_Hp,Wr),
      (grbStg_We_Hnv,We),(grbStg_Wie_Hp,Wie),(grbStg_Wr_Hv,Wr),
      (grbStg_Wir_Hnv,Wir),(grbStg_We_Hp,We),(grbStg_Wie_Hv,Wie),
      (grbStg_Wr_Hl,Wr),(drpStg_Wr_Hnv,Wr),(grbStg_Wir_Hp,Wir),
      (grbStg_We_Hv,We),(grbStg_Wie_Hl,Wie),(drpStg_Wie_Hnv,Wie),
      (drpStg_Wr_Hp,Wr),(grbStg_Wir_Hv,Wir),(grbStg_We_Hl,We),
      (drpStg_We_Hnv,We),(drpStg_Wie_Hp,Wie),(drpStg_Wr_Hv,Wr),
      (grbStg_Wir_Hl,Wir),(drpStg_Wir_Hnv,Wir),(drpStg_We_Hp,We),
      (drpStg_Wie_Hv,Wie),(drpStg_Wr_Hl,Wr),(drpStg_Wir_Hp,Wir),
      (drpStg_We_Hv,We),(drpStg_Wie_Hl,Wie),(drpStg_Wir_Hv,Wir),
      (drpStg_We_Hl,We),(drpStg_Wir_Hl,Wir) }
 &
 hook = {
      (grbStg_Wr_Hnv,Hnv),(grbStg_Wie_Hnv,Hnv),(grbStg_Wr_Hp,Hp),
      (grbStg_We_Hnv,Hnv),(grbStg_Wie_Hp,Hp),(grbStg_Wr_Hv,Hv),
      (grbStg_Wir_Hnv,Hnv),(grbStg_We_Hp,Hp),(grbStg_Wie_Hv,Hv),
      (grbStg_Wr_Hl,Hl),(drpStg_Wr_Hnv,Hnv),(grbStg_Wir_Hp,Hp),
      (grbStg_We_Hv,Hv),(grbStg_Wie_Hl,Hl),(drpStg_Wie_Hnv,Hnv),
      (drpStg_Wr_Hp,Hp),(grbStg_Wir_Hv,Hv),(grbStg_We_Hl,Hl),
      (drpStg_We_Hnv,Hnv),(drpStg_Wie_Hp,Hp),(drpStg_Wr_Hv,Hv),
      (grbStg_Wir_Hl,Hl),(drpStg_Wir_Hnv,Hnv),(drpStg_We_Hp,Hp),
      (drpStg_Wie_Hv,Hv),(drpStg_Wr_Hl,Hl),(drpStg_Wir_Hp,Hp),
      (drpStg_We_Hv,Hv),(drpStg_Wie_Hl,Hl),(drpStg_Wir_Hv,Hv),
      (drpStg_We_Hl,Hl),(drpStg_Wir_Hl,Hl) }
 &

/*
   1. The winch and hook commands must be strictly alternated.
   2. When the winch is completely retracted, the hook must be either closed (vacuum)
      or open (no vacuum).
   3. In the grab stage (pick up a piece), the hook cannot attempt to drop
      a piece and the winch cannot go up without a piece.
   4. In the drop stage (put down a piece), the hook cannot attempt to grap
      a piece and the winch cannot go up with a piece.
*/
 BadStates =
// The winch is in extension or in retraction and the hook is loosing or pumping air
// (attempting to release or grab a piece):
    {x | x : States & winch(x) : {Wie,Wir} & hook(x) : {Hl,Hp} } \/
// The winch is completely retracted and the hook is loosing or pumping air (attempting
// to drop or grab a piece):
    {x | x : States & winch(x) = Wr & hook(x) : {Hl,Hp} } \/
// In the grab stage, the winch is completely extended and the hook is loosing air or
// the winch is in retraction and there is no vacuum:
    {x | x : States & seqH(x) = grbStg & ((winch(x) = We & hook(x) = Hl) or
                                          (winch(x) = Wir & hook(x) = Hnv)) } \/
// In the drop stage, the winch is completely extended and the hook is pumping air or
// the winch is in retraction and there is vacuum:
    {x | x : States & seqH(x) = drpStg & ((winch(x) = We & hook(x) = Hp) or
                                          (winch(x) = Wir & hook(x) = Hv)) }
 &
 Qpred = States - BadStates
END

/*
OUTPUT: 
The transition structure of the agent
 (i.e.,the solution of the control problem)
Number of states of the exhaustive model: 32
Number of transitions of the exhaustive model: 64
Cardinality of the predicate: 16
Supremal coreachable and controllable predicates (12):
{grbStg_Wr_Hnv,grbStg_Wie_Hnv,grbStg_We_Hnv,grbStg_We_Hp,grbStg_We_Hv,grbStg_Wir_Hv,
 drpStg_We_Hnv,drpStg_Wr_Hv,drpStg_Wir_Hnv,drpStg_Wie_Hv,drpStg_We_Hv,drpStg_We_Hl}
Disabled events:
{(grbStg_Wr_Hnv|->{H_grab}),(grbStg_Wie_Hnv|->{H_grab}),(grbStg_We_Hnv|->{W_gu}),
 (grbStg_We_Hp|->{W_gu}),(grbStg_We_Hv|->{H_drop}),(grbStg_Wir_Hv|->{H_drop}),
 (drpStg_We_Hnv|->{H_grab}),(drpStg_Wr_Hv|->{H_drop}),(drpStg_Wir_Hnv|->{H_grab}),
 (drpStg_Wie_Hv|->{H_drop}),(drpStg_We_Hv|->{W_gu}),(drpStg_We_Hl|->{W_gu})}
Enabled events:
{(grbStg_Wr_Hnv|->{W_gd}),(grbStg_Wie_Hnv|->{W_eod}),(grbStg_We_Hnv|->{H_grab}),
 (grbStg_We_Hp|->{H_eog}),(grbStg_We_Hv|->{W_gu}),(grbStg_Wir_Hv|->{W_eoa}),
 (drpStg_We_Hnv|->{W_gu}),(drpStg_Wr_Hv|->{W_gd}),(drpStg_Wir_Hnv|->{W_eoa}),
 (drpStg_Wie_Hv|->{W_eod}),(drpStg_We_Hv|->{H_drop}),(drpStg_We_Hl|->{H_eod})}
Closed-loop system:
 rec(Xm:{grbStg_Wr_Hnv},
     ctrlEvents:{W_gd,W_gu,H_drop,H_grab},
     events:{W_eoa,W_eod,W_gd,W_gu,H_drop,H_eog,H_eod,H_grab},
     states:{grbStg_Wr_Hnv,grbStg_Wie_Hnv,grbStg_We_Hnv,grbStg_We_Hp,
             grbStg_We_Hv,grbStg_Wir_Hv,drpStg_We_Hnv,drpStg_Wr_Hv,
             drpStg_Wir_Hnv,drpStg_Wie_Hv,drpStg_We_Hv,drpStg_We_Hl},
     tf:{((grbStg_Wr_Hnv|->W_gd)|->grbStg_Wie_Hnv),
         ((grbStg_Wie_Hnv|->W_eod)|->grbStg_We_Hnv),
         ((grbStg_We_Hnv|->H_grab)|->grbStg_We_Hp),
         ((grbStg_We_Hp|->H_eog)|->grbStg_We_Hv),
         ((grbStg_We_Hv|->W_gu)|->grbStg_Wir_Hv),
         ((grbStg_Wir_Hv|->W_eoa)|->drpStg_Wr_Hv),
         ((drpStg_We_Hnv|->W_gu)|->drpStg_Wir_Hnv),
         ((drpStg_Wr_Hv|->W_gd)|->drpStg_Wie_Hv),
         ((drpStg_Wir_Hnv|->W_eoa)|->grbStg_Wr_Hnv),
         ((drpStg_Wie_Hv|->W_eod)|->drpStg_We_Hv),
         ((drpStg_We_Hv|->H_drop)|->drpStg_We_Hl),
         ((drpStg_We_Hl|->H_eod)|->drpStg_We_Hnv)},
     x_0:grbStg_Wr_Hnv)
*/

/*
  ControlLaw(W_gd) = Wr
  ControlLaw(W_gu) = We & ((drpStg & Hnv) | (grbStg & Hv))
  ControlLaw(H_drop) = drpStg & We & Hv
  ControlLaw(H_grab) = grbStg & We & Hnv
*/

/*
% Runtime for SOLUTION for SETUP_CONSTANTS: 110 ms (walltime: 110 ms)
% Finished searching for valid initial states 130 ms walltime (130 ms runtime), since start: 1780 ms
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
% Finished processing file after 290 ms walltime (280 ms runtime), since start: 1900 ms

% Finished process_file (errors=0, warnings=0)
*/