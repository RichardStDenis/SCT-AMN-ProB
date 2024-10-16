// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBcrane_lr

SETS
 States = {BMe_Hnv, BMir_Hnv, BMe_Hp,
           BMr_Hnv, BMir_Hp, BMe_Hv,
           BMie_Hnv, BMr_Hp, BMir_Hv,
           BMe_Hl, BMie_Hp, BMr_Hv,
           BMir_Hl, BMie_Hv, BMr_Hl,
           BMie_Hl };
 Events = {BM_eogr, BM_eogl, BM_gr, BM_gl, H_drop, H_eog, H_eod, H_grab};
 LocalStates = {BMe, BMie, BMir, BMr, Hl, Hnv, Hp, Hv}

CONSTANTS
 G_FB,
 boom, hook,
 Qpred, BadStates

PROPERTIES
 G_FB =rec(   // Automaton modeling the free behavior
      states: States,
      events: Events,
      tf: {(BMe_Hnv,BM_gl,BMir_Hnv), (BMe_Hnv,H_grab,BMe_Hp), (BMir_Hnv,BM_eogl,BMr_Hnv),
           (BMir_Hnv,H_grab,BMir_Hp), (BMe_Hp,BM_gl,BMir_Hp), (BMe_Hp,H_eog,BMe_Hv),
           (BMr_Hnv,BM_gr,BMie_Hnv), (BMr_Hnv,H_grab,BMr_Hp), (BMir_Hp,BM_eogl,BMr_Hp),
           (BMir_Hp,H_eog,BMir_Hv), (BMe_Hv,BM_gl,BMir_Hv), (BMe_Hv,H_drop,BMe_Hl),
           (BMie_Hnv,BM_eogr,BMe_Hnv), (BMie_Hnv,H_grab,BMie_Hp), (BMr_Hp,BM_gr,BMie_Hp),
           (BMr_Hp,H_eog,BMr_Hv), (BMir_Hv,BM_eogl,BMr_Hv), (BMir_Hv,H_drop,BMir_Hl),
           (BMe_Hl,BM_gl,BMir_Hl), (BMe_Hl,H_eod,BMe_Hnv), (BMie_Hp,BM_eogr,BMe_Hp),
           (BMie_Hp,H_eog,BMie_Hv), (BMr_Hv,BM_gr,BMie_Hv), (BMr_Hv,H_drop,BMr_Hl),
           (BMir_Hl,BM_eogl,BMr_Hl), (BMir_Hl,H_eod,BMir_Hnv), (BMie_Hv,BM_eogr,BMe_Hv),
           (BMie_Hv,H_drop,BMie_Hl), (BMr_Hl,BM_gr,BMie_Hl), (BMr_Hl,H_eod,BMr_Hnv),
           (BMie_Hl,BM_eogr,BMe_Hl), (BMie_Hl,H_eod,BMie_Hnv) },
      x_0: BMe_Hnv,
      Xm: {BMe_Hnv},
      ctrlEvents: {BM_gr, BM_gl, H_drop, H_grab} )
 &
 boom = {
      (BMe_Hnv,BMe),(BMir_Hnv,BMir),(BMe_Hp,BMe),
      (BMr_Hnv,BMr),(BMir_Hp,BMir),(BMe_Hv,BMe),
      (BMie_Hnv,BMie),(BMr_Hp,BMr),(BMir_Hv,BMir),
      (BMe_Hl,BMe),(BMie_Hp,BMie),(BMr_Hv,BMr),
      (BMir_Hl,BMir),(BMie_Hv,BMie),(BMr_Hl,BMr),
      (BMie_Hl,BMie) }
 &
 hook = {
      (BMe_Hnv,Hnv),(BMir_Hnv,Hnv),(BMe_Hp,Hp),
      (BMr_Hnv,Hnv),(BMir_Hp,Hp),(BMe_Hv,Hv),
      (BMie_Hnv,Hnv),(BMr_Hp,Hp),(BMir_Hv,Hv),
      (BMe_Hl,Hl),(BMie_Hp,Hp),(BMr_Hv,Hv),
      (BMir_Hl,Hl),(BMie_Hv,Hv),(BMr_Hl,Hl),
      (BMie_Hl,Hl) }
 &
/*
  Implicit constraints:
    1. The boom and the hook commands must be strictly alternated.
    2. The crane cannot attempt to drop a piece when it is on the left.
    3. The crane cannot attempt to grab a piece when it is on the right.
    4. The crane cannot move a piece from right to left.
*/
 BadStates =
// The boom is in extension or in retraction and the hook is loosing or pumping air:
     {x | x : States & boom(x) : {BMie,BMir} & hook(x) : {Hl,Hp}} \/
// The boom is completely retracted and the hook is loosing air (dropping a piece):
  {x | x : States & boom(x) = BMr & hook(x) = Hl } \/
// The boom is completely extended and the hook is pumping air (grabbing a piece):
  {x | x : States & boom(x) = BMe & hook(x) = Hp } \/
// The boom is in retraction and there is vacuum:
  {x | x : States & boom(x) = BMir & hook(x) = Hv }
 &
 Qpred = States - BadStates
END

/*
OUTPUT:

The transition structure of the agent
 (i.e.,the solution of the control problem)
Number of states of the exhaustive model: 16
Number of transitions of the exhaustive model: 32
Cardinality of the predicate: 9
Supremal coreachable and controllable predicates (9):
{BMe_Hnv,BMir_Hnv,BMr_Hnv,BMe_Hv,BMie_Hnv,BMr_Hp,BMe_Hl,BMr_Hv,BMie_Hv}
Disabled events: 
{(BMe_Hnv|->{H_grab}),(BMir_Hnv|->{H_grab}),(BMr_Hnv|->{}),
 (BMe_Hv|->{BM_gl}),(BMie_Hnv|->{H_grab}),(BMr_Hp|->{BM_gr}),
 (BMe_Hl|->{BM_gl}),(BMr_Hv|->{H_drop}),(BMie_Hv|->{H_drop})}
Enabled events:
{(BMe_Hnv|->{BM_gl}),(BMir_Hnv|->{BM_eogl}),(BMr_Hnv|->{BM_gr,H_grab}),
 (BMe_Hv|->{H_drop}),(BMie_Hnv|->{BM_eogr}),(BMr_Hp|->{H_eog}),
 (BMe_Hl|->{H_eod}),(BMr_Hv|->{BM_gr}),(BMie_Hv|->{BM_eogr})}
Closed-loop system:
 rec(Xm:{BMe_Hnv},
     ctrlEvents:{BM_gr,BM_gl,H_drop,H_grab},
     events:{BM_eogr,BM_eogl,BM_gr,BM_gl,H_drop,H_eog,H_eod,H_grab},
     states:{BMe_Hnv,BMir_Hnv,BMr_Hnv,BMe_Hv,BMie_Hnv,BMr_Hp,BMe_Hl,BMr_Hv,BMie_Hv},
     tf:{((BMe_Hnv|->BM_gl)|->BMir_Hnv),((BMir_Hnv|->BM_eogl)|->BMr_Hnv),
         ((BMr_Hnv|->BM_gr)|->BMie_Hnv),((BMr_Hnv|->H_grab)|->BMr_Hp),
         ((BMe_Hv|->H_drop)|->BMe_Hl),((BMie_Hnv|->BM_eogr)|->BMe_Hnv),
         ((BMr_Hp|->H_eog)|->BMr_Hv),((BMe_Hl|->H_eod)|->BMe_Hnv),
         ((BMr_Hv|->BM_gr)|->BMie_Hv),((BMie_Hv|->BM_eogr)|->BMe_Hv)},
     x_0:BMe_Hnv)
*/

/*
  ControlLaw(BM_gl) = BMe & Hnv
  ControlLaw(BM_gr) = BMr & (Hnv | Hv)
  ControlLaw(H_drop) = BMe & Hv
  ControlLaw(H_grab) = BMr & Hnv
*/

/*
% Runtime for SOLUTION for SETUP_CONSTANTS: 100 ms (walltime: 100 ms)
% Finished searching for valid initial states 120 ms walltime (120 ms runtime), since start: 1740 ms
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
% Finished processing file after 280 ms walltime (270 ms runtime), since start: 1860 ms

% Finished process_file (errors=0, warnings=0)
*/