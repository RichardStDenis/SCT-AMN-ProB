// copyright  Richard St-Denis, Universite de Sherbrooke, 2022.
MODEL section5_1_5_2

SEES
 //---------------------------------------------------------------------
 // Machine for input data
 //---------------------------------------------------------------------
 SCP_94_2_Fig13 /*@file "../DataExamples/SCP_94_2_Fig13.mch" */

ABSTRACT_CONSTANTS GetsupCCP, GetsupCWCP

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 1000000;
 SET_PREF_SMT == TRUE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../Theories/SBCDES.def";
 "LibraryIO.def"

PROPERTIES
 G_fig13 : Generator_C(G_fig13'states,G_fig13'events) &
 GetsupCCP = lambdaGetsupCCP(G_fig13,GetsupCCP) &
 GetsupCWCP = lambdaGetsupCWCP(G_fig13,GetsupCWCP)

VARIABLES
 code,
 supCCP, supWCCP

INITIALISATION
 code := 0;
 supCCP := GetsupCCP(Q_fig13); supWCCP := GetsupCWCP(Q_fig13)

INVARIANT
 code >= 0 & code <= 3 &
 supCCP <: G_fig13'states & supWCCP <: G_fig13'states

OPERATIONS
 DisplaySup = PRE code = 0 THEN
  PRINT(supWCCP); PRINT(supCCP); code := 1 END;

 Display_SFBC = PRE code = 0 THEN
  LET sup BE sup = GetsupCCP(Q_fig13) IN
   ANY sfbc WHERE sfbc : SFBCf(G_fig13'states,G_fig13'events) &
    sfbc = f_a(G_fig13,sup) THEN PRINT(sfbc) END END END;

 ComputeSupCCP = PRE code = 1 THEN
  VAR Q_i, Q_succ_i IN
   Q_i := Q_fig13;   Q_succ_i := iStepSupCWCP(G_fig13,Q_fig13,Q_i);
   WHILE Q_i /= Q_succ_i DO
    Q_i := Q_succ_i;  Q_succ_i := iStepSupCWCP(G_fig13,Q_fig13,Q_i)
   INVARIANT card(Q_fig13) >= 0 VARIANT card(Q_succ_i) END;
  PRINT(R(G_fig13,Q_i)) END END;

 Display_SFBC_d =  PRE code = 1 THEN
  ANY sfbc_d, sfbc_e, sfbc_d_, sfbc_e_ WHERE 
   sfbc_d : SFBCf(G_fig13'states,G_fig13'events) &
   sfbc_e : SFBCf(G_fig13'states,G_fig13'events) &
   sfbc_d_ : SFBCf(G_fig13'states,G_fig13'events) &
   sfbc_e_ : SFBCf(G_fig13'states,G_fig13'events) &
   sfbc_d = f_d(G_fig13,supCCP) &
   sfbc_e = f_a(G_fig13,supCCP) &
   sfbc_d_ = SFBC(G_fig13,supCCP) &
   sfbc_e_ = SFBCEnabled(G_fig13,supCCP) THEN 
    PRINT(sfbc_d); PRINT(supCCP<|sfbc_d); PRINT(sfbc_d_);
    PRINT(sfbc_e); PRINT(supCCP<|sfbc_e); PRINT(sfbc_e_);
    IF f_e(G_fig13,supCCP,e1,x0) THEN PRINT("e1 from x0") END;
    IF not(f_e(G_fig13,supCCP,e2,x0)) THEN PRINT("not e2 from x0") END;
    IF f_e(G_fig13,supCCP,e6,x0) THEN PRINT("e6 from x0") END;
    LET G_f_tf BE G_f_tf = tf_closed_loop(G_fig13,sfbc_d,supCCP) IN
     PRINT(G_f_tf);
     LET G_f BE G_f = inducedCloseLoop(G_fig13,G_f_tf,supCCP) IN
      IF Reachable(G_f,supCCP) THEN
        PRINT("The closed-loop is reachable.") END;
      IF Coreachable(G_f,supCCP) THEN
        PRINT("The closed-loop is coreachable.") END;
      IF Nonblocking(G_f,supCCP) THEN
        PRINT("The closed-loop is nonblocking.") END END END END END;

 Display_unctrlEvents = PRE code = 1 THEN 
  PRINT(uEvents(G_fig13)) END;
 Display_unlblR_1 = PRE code = 1 THEN
  PRINT(unlblR(G_fig13'tf,G_fig13'events,Q_fig13)) END;
 Display_unlblTR_Q = PRE code = 1 THEN 
  PRINT(unlblTR_Q(G_fig13,Q_fig13)) END;
 Display_unlblR_2 = PRE code = 1 THEN
  PRINT(unlblR(G_fig13'tf,uEvents(G_fig13),G_fig13'states)) END;
 Display_unlblTR_E = PRE code = 1 THEN 
  PRINT(unlblTR_E(G_fig13,uEvents(G_fig13))) END;

 Verify_wp_false_1 = PRE code = 1 & not(wp_e(G_fig13,e5,Q_fig13,x4)) THEN
  PRINT("Unsatisfied.") END;
 Verify_wp_false_2 = PRE code = 1 & not(wp_e(G_fig13,e7,Q_fig13,x6)) THEN
  PRINT("Unsatisfied.") END;
 Verify_wp_true = PRE code = 1 & wp_e(G_fig13,e4,Q_fig13,x3) THEN
  PRINT("Satisfied.") END;

 Verify_wlp_false = PRE code = 1 & not(wlp_e(G_fig13,e5,Q_fig13,x4)) THEN
  PRINT("Unsatisfied.") END;
 Verify_wlp_true1 = PRE code = 1 & wlp_e(G_fig13,e8,Q_fig13,x3) THEN
  PRINT("Satisfied.") END;
 Verify_wlp_true2 = PRE code = 1 & wlp_e(G_fig13,e0,Q_fig13,x1) THEN
  PRINT("Satisfied.") END;

 DisplayCtrlField = PRE code = 1 THEN
  PRINT(CtrlField(G_fig13,Q_fig13)) END;
 DisplayCtrlField_ = PRE code = 1 THEN
  PRINT(CtrlField_(G_fig13,Q_fig13)) END;
 Display_DualCtrlField = PRE code = 1 THEN
  PRINT(DualCtrlField(G_fig13,Q_fig13)) END;
 Display_DualCtrlField_ = PRE code = 1 THEN
  PRINT(DualCtrlField_(G_fig13,Q_fig13)) END;
 Display_DualCtrlFieldNotQ = PRE code = 1 THEN
  PRINT(DualCtrlField(G_fig13,G_fig13'states-Q_fig13)) END;
 Display_NotDualCtrlFieldNotQ = PRE code = 1 THEN
  PRINT(G_fig13'states-DualCtrlField(G_fig13,G_fig13'states-Q_fig13)) END;

 Verify_Idempotent = PRE code = 1 & Idempotent(G_fig13,Q_fig13) THEN
  PRINT("Satisfied.") END;
 Verify_Idempotent_Dual = PRE code = 1 & Idempotent_Dual(G_fig13,Q_fig13) THEN
  PRINT("Satisfied.") END;
 Verify_Dual = PRE code = 1 & Dual(G_fig13,Q_fig13) THEN
  PRINT("Satisfied.") END;

 Display_R = PRE code = 1 THEN 
  PRINT(R(G_fig13,Q_fig13)) END;
 Display_RorCR_1 = PRE code = 1 THEN
  PRINT(RorCR(G_fig13,Q_fig13,unlblReachTR(G_fig13,Q_fig13),{G_fig13'x_0})) END;
 Display_CR = PRE code = 1 THEN
  PRINT(CR(G_fig13,Q_fig13)) END;
 Display_RorCR_2 = PRE code = 1 THEN
  PRINT(RorCR(G_fig13,Q_fig13,unlblReachTR(G_fig13,Q_fig13)~,G_fig13'Xm)) END;
 Verify_Reachable = PRE code = 1 & not(Reachable(G_fig13,Q_fig13)) THEN
  PRINT("Q is not reachable.") END;
 Verify_Coreachable = PRE code = 1 & Coreachable(G_fig13,Q_fig13) THEN
  PRINT("Q is coreachable.") END;
 Verify_Nonblocking_true = PRE code = 1 & Nonblocking(G_fig13,Q_fig13) THEN
  PRINT("Q is nonblocking.") END;
 Verify_Nonblocking_false = PRE code = 1 & not(Nonblocking(G_fig13,{G_fig13'x_0})) THEN
  PRINT("{x0} is blocking.") END;

 Verify_WeaklyControllable_1 = PRE code = 1 & not(WeaklyControllable(G_fig13,Q_fig13)) THEN
  PRINT("Q is not weakly controllable.") END;
 Verify_WeaklyControllabl_2 = PRE code = 1 & WeaklyControllable(G_fig13,CtrlField(G_fig13,Q_fig13)) THEN
  PRINT("[Q] is weakly controllable.") END;
 Verify_WeaklyControllabl_3 = PRE code = 1 & WeaklyControllable(G_fig13,{}) THEN
  PRINT("{} is weakly controllable.") END;
 Verify_WeaklyControllable_ = PRE code = 1 & WeaklyControllable_(G_fig13,CtrlField(G_fig13,Q_fig13))
  THEN PRINT("[Q] is weakly controllable.") END;
 Verify_WeaklyControllable__ = PRE code = 1 & WeaklyControllable__(G_fig13,CtrlField(G_fig13,Q_fig13)) THEN
  PRINT("[Q] is weakly controllable.") END;

 Verify_Controllable_1 = PRE code = 1 & not(Controllable(G_fig13,Q_fig13)) THEN
  PRINT("Q is not controllable.") END;
 Verify_Controllable_2 = PRE code = 1 & not(Controllable(G_fig13,CtrlField(G_fig13,Q_fig13))) THEN
  PRINT("[Q] is not controllable.") END;

 Verify_Property_1 = PRE code = 1 & Property(G_fig13,Q_fig13) THEN
  PRINT("The property is satisfied.") END;
 Verify_Property_2 = PRE code = 1 & Property(G_fig13,CtrlField(G_fig13,Q_fig13)) THEN
  PRINT("The property is satisfied.") END;

 SearchSupWCCP = // PRE code = 1 THEN     // A little bit slow
  ANY sup WHERE
   IsASupWCCP(G_fig13,CtrlField(G_fig13,Q_fig13),sup) THEN PRINT(sup) END; // END;

 SearchSupCCP =  PRE code = 1 THEN      // A little bit slow
  ANY sup WHERE
   IsASupCCP(G_fig13,CtrlField(G_fig13,Q_fig13),sup) THEN PRINT(sup) END END END

/*
OUTPUT:
{x0,x1,x6,x8,x9}
{x0,x1,x6}
cover(DisplaySup)
{(x0|->{e0,e1,e3,e4,e5,e6,e7,e8,e9}),(x1|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),
 (x2|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),(x3|->{e0,e1,e2,e3,e5,e6,e7,e8,e9}),
 (x4|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),(x5|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),
 (x6|->{e0,e1,e2,e3,e4,e5,e6,e8,e9}),(x7|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),
 (x8|->{e0,e1,e2,e3,e4,e5,e6,e7,e8}),(x9|->{e0,e1,e2,e3,e4,e5,e6,e7,e9})}
cover(Display_SFBC)
{x0,x1,x6,x8,x9}
cover(SearchSupWCCP)
{x0,x1,x6}
cover(ComputeSupCCP)
{(x0|->{e2}),(x1|->{}),(x2|->{}),(x3|->{e4}),(x4|->{}),(x5|->{}),(x6|->{e7}),
 (x7|->{}),(x8|->{e9}),(x9|->{e8})}
{(x0|->{e2}),(x1|->{}),(x6|->{e7})}
{(x0|->{e2}),(x1|->{}),(x6|->{e7})}
{(x0|->{e0,e1,e3,e4,e5,e6,e7,e8,e9}),(x1|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),
 (x2|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),(x3|->{e0,e1,e2,e3,e5,e6,e7,e8,e9}),
 (x4|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),(x5|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),
 (x6|->{e0,e1,e2,e3,e4,e5,e6,e8,e9}),(x7|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),
 (x8|->{e0,e1,e2,e3,e4,e5,e6,e7,e8}),(x9|->{e0,e1,e2,e3,e4,e5,e6,e7,e9})}
{(x0|->{e0,e1,e3,e4,e5,e6,e7,e8,e9}),(x1|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),
 (x6|->{e0,e1,e2,e3,e4,e5,e6,e8,e9})}
{(x0|->{e1,e6}),(x1|->{e0}),(x6|->{})}
"e1 from x0"
"not e2 from x0"
"e6 from x0"
{((x0|->e1)|->x1),((x0|->e6)|->x6),((x1|->e0)|->x0)}
"The closed-loop is reachable."
"The closed-loop is coreachable."
"The closed-loop is nonblocking."
cover(Display_SFBC_d)
{e0,e1,e3,e5,e6}
cover(Display_unctrlEvents)
{(x0|->x1),(x0|->x2),(x0|->x6),(x1|->x0),(x2|->x3),(x3|->x4),(x8|->x9),(x9|->x8)}
cover(Display_unlblR_1)
{(x0|->x1),(x0|->x2),(x0|->x6),(x1|->x0),(x2|->x3),(x3|->x4),(x8|->x9),(x9|->x8)}
cover(Display_unlblTR_Q)
{(x0|->x1),(x0|->x6),(x1|->x0),(x2|->x3),(x4|->x5)}
cover(Display_unlblR_2)
{(x0|->x1),(x0|->x6),(x1|->x0),(x2|->x3),(x4|->x5)}
cover(Display_unlblTR_E)
"Unsatisfied."
cover(Verify_wp_false_1)
"Unsatisfied."
cover(Verify_wp_false_2)
"Satisfied."
cover(Verify_wp_true)
"Unsatisfied."
cover(Verify_wlp_false)
"Satisfied."
cover(Verify_wlp_true1)
"Satisfied."
cover(Verify_wlp_true2)
{x0,x1,x2,x3,x6,x8,x9}
cover(DisplayCtrlField)
{x0,x1,x2,x3,x6,x8,x9}
cover(DisplayCtrlField_)
{x0,x1,x2,x3,x4,x6,x8,x9}
cover(Display_DualCtrlField)
{x0,x1,x2,x3,x4,x6,x8,x9}
cover(Display_DualCtrlField_)
{x4,x5,x7}
cover(Display_DualCtrlFieldNotQ)
{x0,x1,x2,x3,x6,x8,x9}
cover(Display_NotDualCtrlFieldNotQ)
"Satisfied."
cover(Verify_Idempotent)
"Satisfied."
cover(Verify_Idempotent_Dual)
"Satisfied."
cover(Verify_Dual)
{x0,x1,x2,x3,x4,x6}
cover(Display_R)
{x0,x1,x2,x3,x4,x6}
cover(Display_RorCR_1)
{x0,x1,x2,x3,x4,x6,x8,x9}
cover(Display_CR)
{x0,x1,x2,x3,x4,x6,x8,x9}
cover(Display_RorCR_2)
"Q is not reachable."
cover(Verify_Reachable)
"Q is coreachable."
cover(Verify_Coreachable)
"Q is nonblocking."
cover(Verify_Nonblocking_true)
"{x0} is blocking."
cover(Verify_Nonblocking_false)
"Q is not weakly controllable."
cover(Verify_WeaklyControllable_1)
"[Q] is weakly controllable."
cover(Verify_WeaklyControllabl_2)
"{} is weakly controllable."
cover(Verify_WeaklyControllabl_3)
"[Q] is weakly controllable."
cover(Verify_WeaklyControllable_)
"[Q] is weakly controllable."
cover(Verify_WeaklyControllable__)
"Q is not controllable."
cover(Verify_Controllable_1)
"[Q] is not controllable."
cover(Verify_Controllable_2)
"The property is satisfied."
cover(Verify_Property_1)
"The property is satisfied."
cover(Verify_Property_2)
{x0,x1,x6,x8,x9}
{x0,x1,x6}
cover(SearchSupCCP)
*/