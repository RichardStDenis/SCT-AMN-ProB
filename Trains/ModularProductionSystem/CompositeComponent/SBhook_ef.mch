// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBhook_ef

SETS
 States = {grbStg_Wr_Hnv, grbStg_Wie_Hnv, grbStg_Wr_Hp,
           failStg_We_Hnv, grbStg_Wir_Hnv, grbStg_Wie_Hp,
           grbStg_Wr_Hv, failStg_Wir_Hnv, drpStg_Wr_Hnv,
           grbStg_Wir_Hp, failStg_We_Hp, grbStg_Wie_Hv,
           grbStg_Wr_Hl, failStg_Wr_Hnv, drpStg_Wie_Hnv,
           drpStg_Wr_Hp, grbStg_Wir_Hv, failStg_Wir_Hp,
           failStg_We_Hv, grbStg_Wie_Hl, failStg_Wie_Hnv,
           drpStg_We_Hnv, drpStg_Wie_Hp, drpStg_Wr_Hv,
           grbStg_Wir_Hl, failStg_Wr_Hp, failStg_Wir_Hv,
           failStg_We_Hl, drpStg_Wir_Hnv, drpStg_We_Hp,
           drpStg_Wie_Hv, drpStg_Wr_Hl, failStg_Wie_Hp,
           failStg_Wr_Hv, failStg_Wir_Hl, drpStg_Wir_Hp,
           drpStg_We_Hv, drpStg_Wie_Hl, failStg_Wie_Hv,
           failStg_Wr_Hl, drpStg_Wir_Hv, drpStg_We_Hl,
           failStg_Wie_Hl, drpStg_Wir_Hl };
 Events = {clr, W_cd, W_eoa, W_eod, H_cg, H_grab, W_gd, W_gu, H_drop, H_eog, H_eod};
 LocalStates = {drpStg, failStg, grbStg, We, Wie, Wir, Wr, Hl, Hnv, Hp, Hv}

CONSTANTS
 G_FB,
 seqH, winch, hook,
 Qpred, BadStates

PROPERTIES
 G_FB =rec(   // Automaton modeling the free behavior
      states: States,
      events: Events,
      tf: {(grbStg_Wr_Hnv,W_gd,grbStg_Wie_Hnv), (grbStg_Wr_Hnv,H_grab,grbStg_Wr_Hp), (grbStg_Wie_Hnv,W_eod,failStg_We_Hnv),
           (grbStg_Wie_Hnv,W_cd,grbStg_Wir_Hnv), (grbStg_Wie_Hnv,H_grab,grbStg_Wie_Hp), (grbStg_Wr_Hp,W_gd,grbStg_Wie_Hp),
           (grbStg_Wr_Hp,H_eog,grbStg_Wr_Hv), (failStg_We_Hnv,W_gu,failStg_Wir_Hnv), (grbStg_Wir_Hnv,W_eoa,drpStg_Wr_Hnv),
           (grbStg_Wir_Hnv,H_grab,grbStg_Wir_Hp), (grbStg_Wie_Hp,W_eod,failStg_We_Hp), (grbStg_Wie_Hp,H_eog,grbStg_Wie_Hv),
           (grbStg_Wie_Hp,W_cd,grbStg_Wir_Hp), (grbStg_Wr_Hv,W_gd,grbStg_Wie_Hv), (grbStg_Wr_Hv,H_drop,grbStg_Wr_Hl),
           (failStg_Wir_Hnv,W_eoa,failStg_Wr_Hnv), (drpStg_Wr_Hnv,W_gd,drpStg_Wie_Hnv), (grbStg_Wir_Hp,W_eoa,drpStg_Wr_Hp),
           (grbStg_Wir_Hp,H_eog,grbStg_Wir_Hv), (failStg_We_Hp,W_gu,failStg_Wir_Hp), (failStg_We_Hp,H_eog,failStg_We_Hv),
           (failStg_We_Hp,H_cg,failStg_We_Hnv), (grbStg_Wie_Hv,W_eod,failStg_We_Hv), (grbStg_Wie_Hv,H_drop,grbStg_Wie_Hl),
           (grbStg_Wie_Hv,W_cd,grbStg_Wir_Hv), (grbStg_Wr_Hl,W_gd,grbStg_Wie_Hl), (grbStg_Wr_Hl,H_eod,grbStg_Wr_Hnv),
           (failStg_Wr_Hnv,W_gd,failStg_Wie_Hnv), (failStg_Wr_Hnv,clr,grbStg_Wr_Hnv), (drpStg_Wie_Hnv,W_eod,drpStg_We_Hnv),
           (drpStg_Wr_Hp,W_gd,drpStg_Wie_Hp), (drpStg_Wr_Hp,H_eog,drpStg_Wr_Hv), (grbStg_Wir_Hv,W_eoa,drpStg_Wr_Hv),
           (grbStg_Wir_Hv,H_drop,grbStg_Wir_Hl), (failStg_Wir_Hp,W_eoa,failStg_Wr_Hp), (failStg_Wir_Hp,H_eog,failStg_Wir_Hv),
           (failStg_Wir_Hp,H_cg,failStg_Wir_Hnv), (failStg_We_Hv,W_gu,failStg_Wir_Hv), (failStg_We_Hv,H_drop,failStg_We_Hl),
           (grbStg_Wie_Hl,W_eod,failStg_We_Hl), (grbStg_Wie_Hl,H_eod,grbStg_Wie_Hnv), (grbStg_Wie_Hl,W_cd,grbStg_Wir_Hl),
           (failStg_Wie_Hnv,W_eod,failStg_We_Hnv), (drpStg_We_Hnv,W_gu,drpStg_Wir_Hnv), (drpStg_Wie_Hp,W_eod,drpStg_We_Hp),
           (drpStg_Wie_Hp,H_eog,drpStg_Wie_Hv), (drpStg_Wr_Hv,W_gd,drpStg_Wie_Hv), (drpStg_Wr_Hv,H_drop,drpStg_Wr_Hl),
           (grbStg_Wir_Hl,W_eoa,drpStg_Wr_Hl), (grbStg_Wir_Hl,H_eod,grbStg_Wir_Hnv), (failStg_Wr_Hp,W_gd,failStg_Wie_Hp),
           (failStg_Wr_Hp,H_eog,failStg_Wr_Hv), (failStg_Wr_Hp,H_cg,failStg_Wr_Hnv), (failStg_Wir_Hv,W_eoa,failStg_Wr_Hv),
           (failStg_Wir_Hv,H_drop,failStg_Wir_Hl), (failStg_We_Hl,W_gu,failStg_Wir_Hl), (failStg_We_Hl,H_eod,failStg_We_Hnv),
           (drpStg_Wir_Hnv,W_eoa,grbStg_Wr_Hnv), (drpStg_We_Hp,W_gu,drpStg_Wir_Hp), (drpStg_We_Hp,H_eog,drpStg_We_Hv),
           (drpStg_Wie_Hv,W_eod,drpStg_We_Hv), (drpStg_Wie_Hv,H_drop,drpStg_Wie_Hl), (drpStg_Wr_Hl,W_gd,drpStg_Wie_Hl),
           (drpStg_Wr_Hl,H_eod,drpStg_Wr_Hnv), (failStg_Wie_Hp,W_eod,failStg_We_Hp), (failStg_Wie_Hp,H_eog,failStg_Wie_Hv),
           (failStg_Wie_Hp,H_cg,failStg_Wie_Hnv), (failStg_Wr_Hv,W_gd,failStg_Wie_Hv), (failStg_Wr_Hv,H_drop,failStg_Wr_Hl),
           (failStg_Wir_Hl,W_eoa,failStg_Wr_Hl), (failStg_Wir_Hl,H_eod,failStg_Wir_Hnv), (drpStg_Wir_Hp,W_eoa,grbStg_Wr_Hp),
           (drpStg_Wir_Hp,H_eog,drpStg_Wir_Hv), (drpStg_We_Hv,W_gu,drpStg_Wir_Hv), (drpStg_We_Hv,H_drop,drpStg_We_Hl),
           (drpStg_Wie_Hl,W_eod,drpStg_We_Hl), (drpStg_Wie_Hl,H_eod,drpStg_Wie_Hnv), (failStg_Wie_Hv,W_eod,failStg_We_Hv),
           (failStg_Wie_Hv,H_drop,failStg_Wie_Hl), (failStg_Wr_Hl,W_gd,failStg_Wie_Hl), (failStg_Wr_Hl,H_eod,failStg_Wr_Hnv),
           (drpStg_Wir_Hv,W_eoa,grbStg_Wr_Hv), (drpStg_Wir_Hv,H_drop,drpStg_Wir_Hl), (drpStg_We_Hl,W_gu,drpStg_Wir_Hl),
           (drpStg_We_Hl,H_eod,drpStg_We_Hnv), (failStg_Wie_Hl,W_eod,failStg_We_Hl), (failStg_Wie_Hl,H_eod,failStg_Wie_Hnv),
           (drpStg_Wir_Hl,W_eoa,grbStg_Wr_Hl), (drpStg_Wir_Hl,H_eod,drpStg_Wir_Hnv) },
      x_0: grbStg_Wr_Hnv,
      Xm: {grbStg_Wr_Hnv},
      ctrlEvents: {clr, W_cd, H_cg, H_grab, W_gd, W_gu, H_drop} )
 &
 seqH = {
      (grbStg_Wr_Hnv,grbStg),(grbStg_Wie_Hnv,grbStg),(grbStg_Wr_Hp,grbStg),
      (failStg_We_Hnv,failStg),(grbStg_Wir_Hnv,grbStg),(grbStg_Wie_Hp,grbStg),
      (grbStg_Wr_Hv,grbStg),(failStg_Wir_Hnv,failStg),(drpStg_Wr_Hnv,drpStg),
      (grbStg_Wir_Hp,grbStg),(failStg_We_Hp,failStg),(grbStg_Wie_Hv,grbStg),
      (grbStg_Wr_Hl,grbStg),(failStg_Wr_Hnv,failStg),(drpStg_Wie_Hnv,drpStg),
      (drpStg_Wr_Hp,drpStg),(grbStg_Wir_Hv,grbStg),(failStg_Wir_Hp,failStg),
      (failStg_We_Hv,failStg),(grbStg_Wie_Hl,grbStg),(failStg_Wie_Hnv,failStg),
      (drpStg_We_Hnv,drpStg),(drpStg_Wie_Hp,drpStg),(drpStg_Wr_Hv,drpStg),
      (grbStg_Wir_Hl,grbStg),(failStg_Wr_Hp,failStg),(failStg_Wir_Hv,failStg),
      (failStg_We_Hl,failStg),(drpStg_Wir_Hnv,drpStg),(drpStg_We_Hp,drpStg),
      (drpStg_Wie_Hv,drpStg),(drpStg_Wr_Hl,drpStg),(failStg_Wie_Hp,failStg),
      (failStg_Wr_Hv,failStg),(failStg_Wir_Hl,failStg),(drpStg_Wir_Hp,drpStg),
      (drpStg_We_Hv,drpStg),(drpStg_Wie_Hl,drpStg),(failStg_Wie_Hv,failStg),
      (failStg_Wr_Hl,failStg),(drpStg_Wir_Hv,drpStg),(drpStg_We_Hl,drpStg),
      (failStg_Wie_Hl,failStg),(drpStg_Wir_Hl,drpStg) }
 &
 winch = {
      (grbStg_Wr_Hnv,Wr),(grbStg_Wie_Hnv,Wie),(grbStg_Wr_Hp,Wr),
      (failStg_We_Hnv,We),(grbStg_Wir_Hnv,Wir),(grbStg_Wie_Hp,Wie),
      (grbStg_Wr_Hv,Wr),(failStg_Wir_Hnv,Wir),(drpStg_Wr_Hnv,Wr),
      (grbStg_Wir_Hp,Wir),(failStg_We_Hp,We),(grbStg_Wie_Hv,Wie),
      (grbStg_Wr_Hl,Wr),(failStg_Wr_Hnv,Wr),(drpStg_Wie_Hnv,Wie),
      (drpStg_Wr_Hp,Wr),(grbStg_Wir_Hv,Wir),(failStg_Wir_Hp,Wir),
      (failStg_We_Hv,We),(grbStg_Wie_Hl,Wie),(failStg_Wie_Hnv,Wie),
      (drpStg_We_Hnv,We),(drpStg_Wie_Hp,Wie),(drpStg_Wr_Hv,Wr),
      (grbStg_Wir_Hl,Wir),(failStg_Wr_Hp,Wr),(failStg_Wir_Hv,Wir),
      (failStg_We_Hl,We),(drpStg_Wir_Hnv,Wir),(drpStg_We_Hp,We),
      (drpStg_Wie_Hv,Wie),(drpStg_Wr_Hl,Wr),(failStg_Wie_Hp,Wie),
      (failStg_Wr_Hv,Wr),(failStg_Wir_Hl,Wir),(drpStg_Wir_Hp,Wir),
      (drpStg_We_Hv,We),(drpStg_Wie_Hl,Wie),(failStg_Wie_Hv,Wie),
      (failStg_Wr_Hl,Wr),(drpStg_Wir_Hv,Wir),(drpStg_We_Hl,We),
      (failStg_Wie_Hl,Wie),(drpStg_Wir_Hl,Wir) }
 &
 hook = {
      (grbStg_Wr_Hnv,Hnv),(grbStg_Wie_Hnv,Hnv),(grbStg_Wr_Hp,Hp),
      (failStg_We_Hnv,Hnv),(grbStg_Wir_Hnv,Hnv),(grbStg_Wie_Hp,Hp),
      (grbStg_Wr_Hv,Hv),(failStg_Wir_Hnv,Hnv),(drpStg_Wr_Hnv,Hnv),
      (grbStg_Wir_Hp,Hp),(failStg_We_Hp,Hp),(grbStg_Wie_Hv,Hv),
      (grbStg_Wr_Hl,Hl),(failStg_Wr_Hnv,Hnv),(drpStg_Wie_Hnv,Hnv),
      (drpStg_Wr_Hp,Hp),(grbStg_Wir_Hv,Hv),(failStg_Wir_Hp,Hp),
      (failStg_We_Hv,Hv),(grbStg_Wie_Hl,Hl),(failStg_Wie_Hnv,Hnv),
      (drpStg_We_Hnv,Hnv),(drpStg_Wie_Hp,Hp),(drpStg_Wr_Hv,Hv),
      (grbStg_Wir_Hl,Hl),(failStg_Wr_Hp,Hp),(failStg_Wir_Hv,Hv),
      (failStg_We_Hl,Hl),(drpStg_Wir_Hnv,Hnv),(drpStg_We_Hp,Hp),
      (drpStg_Wie_Hv,Hv),(drpStg_Wr_Hl,Hl),(failStg_Wie_Hp,Hp),
      (failStg_Wr_Hv,Hv),(failStg_Wir_Hl,Hl),(drpStg_Wir_Hp,Hp),
      (drpStg_We_Hv,Hv),(drpStg_Wie_Hl,Hl),(failStg_Wie_Hv,Hv),
      (failStg_Wr_Hl,Hl),(drpStg_Wir_Hv,Hv),(drpStg_We_Hl,Hl),
      (failStg_Wie_Hl,Hl),(drpStg_Wir_Hl,Hl) }
 &

/*
  Explicit constraints:
    1. The failure stage ends when the winch is completely retracted and there is no vacuum.
       The event clr can only occur if the value of the state variable is <failStg, Wr, Hnv>.
    2. The command grab is allowed only in the grab stage.
       The event H_grab can only occur if the value of the state variable is <grbStg, _, _>.
    3. The command cancel descent is allowed only in the grab stage.
       The event W_cd can only occur if the value of the state variable is <grbStg, _, _>.
    4. The command cancel grab is allowed only in the failure stage.
       The event H_cg can only occur if the value of the state variable is <failStg, _, _>.
  Standard constraint:
    5. In the failure stage, the hook_ef cannot go down.
  Implicit constraints:
      6. When the winch is completely retracted, the hook must be either closed (vacuum)
         or open (no vacuum).
      7. In the grab stage, the winch cannot go down while dropping a piece.
    8-9. In the grab stage, the hook_ef cannot go up without a piece.
     10. In the failure stage, the hook_ef can go up with no vacuum.
     11. In the drop stage, the winch and hook commands must be strictly alternated.
         (the winch and the hook cannot proceed simultaneously).
     12. In the drop stage, the hook_ef cannot go down without vacuum.
     13. In the drop stage, the hook_ef cannot go up with vacuum.
     14. In the drop stage, the hook_ef cannot grab a piece.
*/
 BadStates =
// In the failure stage, the which in extension:
    {x | x : States & seqH(x) = failStg & winch(x) = Wie} \/
// The winch is completely retracted and the hook is loosing or pumping air
// (attempting to drop or grab a piece):
    {x | x : States & winch(x) = Wr & hook(x) : {Hl,Hp}} \/
// In the grab stage, the winch is in extension and the hook is loosing air
// (attempting to drop a piece):
    {x | x : States & seqH(x) = grbStg & winch(x) = Wie & hook(x) = Hl} \/
// In the grab stage, the winch is in retraction and the hook is loosing or pumping air
// (attempting to drop or grab a piece):
    {x | x : States & seqH(x) = grbStg & winch(x) = Wir & hook(x) : {Hl,Hp}} \/
// In the grab stage, the winch is in retraction and there is no vacuum
// (dropping a piece):
    {x | x : States & seqH(x) = grbStg & winch(x) = Wir & hook(x) = Hnv} \/
// In the failure stage, the winch is in retraction and the hook is not in a state with
// no vacuum (grabbing a piece or attempting to drop or grab a piece):
    {x | x : States & seqH(x) = failStg & winch(x) = Wir & hook(x) : {Hl,Hp,Hv}} \/
// In the drop stage, the winch is in extension or in retraction and the hook is 
// loosing or pumping air:
    {x | x : States & seqH(x) = drpStg & winch(x) : {Wie,Wir} & hook(x) : {Hl,Hp}} \/
// In the drop stage, the winch is in extension and there is no vacuum (dropping a piece):
    {x | x : States & seqH(x) = drpStg & winch(x) = Wie & hook(x) = Hnv} \/
// In the drop stage, the winch is in retraction and there is vacuum (grabbing a piece):
    {x | x : States & seqH(x) = drpStg & winch(x) = Wir & hook(x) = Hv} \/
// In the drop stage, the winch is completely extended and the hook is pumping air
// (attempting to grab a piece):
    {x | x : States & seqH(x) = drpStg & winch(x) = We & hook(x) = Hp}
 &
 Qpred = States - BadStates
END

/*
OUTPUT:
The transition structure of the agent
 (i.e.,the solution of the control problem)
Number of states of the exhaustive model: 44
Number of transitions of the exhaustive model: 89
Cardinality of the predicate: 20
Supremal coreachable and controllable predicates (17):
{grbStg_Wr_Hnv,grbStg_Wie_Hnv,failStg_We_Hnv,grbStg_Wie_Hp,failStg_Wir_Hnv,
 failStg_We_Hp,grbStg_Wie_Hv,failStg_Wr_Hnv,grbStg_Wir_Hv,failStg_We_Hv,
 drpStg_We_Hnv,drpStg_Wr_Hv,failStg_We_Hl,drpStg_Wir_Hnv,drpStg_Wie_Hv,
 drpStg_We_Hv,drpStg_We_Hl}
Disabled events:
{(grbStg_Wr_Hnv|->{H_grab}),(grbStg_Wie_Hnv|->{W_cd}),(failStg_We_Hnv|->{}),
 (grbStg_Wie_Hp|->{W_cd}),(failStg_Wir_Hnv|->{}),(failStg_We_Hp|->{W_gu}),
 (grbStg_Wie_Hv|->{H_drop}),(failStg_Wr_Hnv|->{W_gd}),(grbStg_Wir_Hv|->{H_drop}),
 (failStg_We_Hv|->{W_gu}),(drpStg_We_Hnv|->{}),(drpStg_Wr_Hv|->{H_drop}),
 (failStg_We_Hl|->{W_gu}),(drpStg_Wir_Hnv|->{}),(drpStg_Wie_Hv|->{H_drop}),
 (drpStg_We_Hv|->{W_gu}),(drpStg_We_Hl|->{W_gu})}
Enabled events:
{(grbStg_Wr_Hnv|->{W_gd}),(grbStg_Wie_Hnv|->{W_eod,H_grab}),(failStg_We_Hnv|->{W_gu}),
 (grbStg_Wie_Hp|->{W_eod,H_eog}),(failStg_Wir_Hnv|->{W_eoa}),(failStg_We_Hp|->{H_cg,H_eog}),
 (grbStg_Wie_Hv|->{W_cd,W_eod}),(failStg_Wr_Hnv|->{clr}),(grbStg_Wir_Hv|->{W_eoa}),
 (failStg_We_Hv|->{H_drop}),(drpStg_We_Hnv|->{W_gu}),(drpStg_Wr_Hv|->{W_gd}),
 (failStg_We_Hl|->{H_eod}),(drpStg_Wir_Hnv|->{W_eoa}),(drpStg_Wie_Hv|->{W_eod}),
 (drpStg_We_Hv|->{H_drop}),(drpStg_We_Hl|->{H_eod})}
Closed-loop system:
 rec(Xm:{grbStg_Wr_Hnv},
     ctrlEvents:{clr,W_cd,H_cg,H_grab,W_gd,W_gu,H_drop},
     events:{clr,W_cd,W_eoa,W_eod,H_cg,H_grab,W_gd,W_gu,H_drop,H_eog,H_eod},
     states:{grbStg_Wr_Hnv,grbStg_Wie_Hnv,failStg_We_Hnv,grbStg_Wie_Hp,failStg_Wir_Hnv,
             failStg_We_Hp,grbStg_Wie_Hv,failStg_Wr_Hnv,grbStg_Wir_Hv,failStg_We_Hv,
             drpStg_We_Hnv,drpStg_Wr_Hv,failStg_We_Hl,drpStg_Wir_Hnv,drpStg_Wie_Hv,
             drpStg_We_Hv,drpStg_We_Hl},
     tf:{((grbStg_Wr_Hnv|->W_gd)|->grbStg_Wie_Hnv),
         ((grbStg_Wie_Hnv|->W_eod)|->failStg_We_Hnv),
         ((grbStg_Wie_Hnv|->H_grab)|->grbStg_Wie_Hp),
         ((failStg_We_Hnv|->W_gu)|->failStg_Wir_Hnv),
         ((grbStg_Wie_Hp|->W_eod)|->failStg_We_Hp),
         ((grbStg_Wie_Hp|->H_eog)|->grbStg_Wie_Hv),
         ((failStg_Wir_Hnv|->W_eoa)|->failStg_Wr_Hnv),
         ((failStg_We_Hp|->H_cg)|->failStg_We_Hnv),
         ((failStg_We_Hp|->H_eog)|->failStg_We_Hv),
         ((grbStg_Wie_Hv|->W_cd)|->grbStg_Wir_Hv),
         ((grbStg_Wie_Hv|->W_eod)|->failStg_We_Hv),
         ((failStg_Wr_Hnv|->clr)|->grbStg_Wr_Hnv),
         ((grbStg_Wir_Hv|->W_eoa)|->drpStg_Wr_Hv),
         ((failStg_We_Hv|->H_drop)|->failStg_We_Hl),
         ((drpStg_We_Hnv|->W_gu)|->drpStg_Wir_Hnv),
         ((drpStg_Wr_Hv|->W_gd)|->drpStg_Wie_Hv),
         ((failStg_We_Hl|->H_eod)|->failStg_We_Hnv),
         ((drpStg_Wir_Hnv|->W_eoa)|->grbStg_Wr_Hnv),
         ((drpStg_Wie_Hv|->W_eod)|->drpStg_We_Hv),
         ((drpStg_We_Hv|->H_drop)|->drpStg_We_Hl),
         ((drpStg_We_Hl|->H_eod)|->drpStg_We_Hnv)},
     x_0:grbStg_Wr_Hnv)
*/

/*
  ControlLaw(clr) = failStg & Wr
  ControlLaw(W_cd) = grbStg & Wie & Hv
  ControlLaw(W_gd) = (grbStg | drpStg) & Wr
  ControlLaw(W_gu) = We & Hnv
  ControlLaw(H_cg) =failStg & Hp
  ControlLaw(H_grab) = Wie & Hnv
  ControlLaw(H_drop) = We & Hv
*/

/*
% Runtime for SOLUTION for SETUP_CONSTANTS: 100 ms (walltime: 100 ms)
% Finished searching for valid initial states 120 ms walltime (120 ms runtime), since start: 1780 ms
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
finished_random_animate(5,[runtime/130,total_runtime/130,walltime/140])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 310 ms walltime (300 ms runtime), since start: 1920 ms

% Finished process_file (errors=0, warnings=0)
*/