// copyright  Richard St-Denis, Universite de Sherbrooke, 2022.
MODEL test_sbc_all

SEES
 // ----------------------------------------------------------------------------
 // Machine for input data
 // ----------------------------------------------------------------------------
   sbc_normality /*@file "../DataExamples/sbc_normality.mch" */

ABSTRACT_CONSTANTS GetsupCCP, GetsupCNCP, GetsupSMCP, GetmaxMCP

DEFINITIONS
 // ----------------------------------------------------------------------------
 // ProB settings
 // ----------------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 1000000;
 SET_PREF_SMT == FALSE;
 SET_PREF_KODKOD == FALSE;
 // ----------------------------------------------------------------------------
 // Import the theory
 // ----------------------------------------------------------------------------
 "../Theories/SBCDES.def";
 "LibraryIO.def";

PROPERTIES
 G0 : Generator_C(G0'states,G0'events) &
 mask : obsMask(States,ObservationSpace) &

 GetsupCCP = lambdaGetsupCCP(G0,GetsupCCP) &
 GetsupCNCP = lambdaGetsupCNCP(G0,mask,ObservationSpace,GetsupCNCP) &
 GetsupSMCP = lambdaGetsupSMCP(G0,mask,GetsupSMCP) &
 GetmaxMCP = lambdaGetmaxMCP(G0,mask,GetmaxMCP)

VARIABLES
 code
INITIALISATION
 code := 0
INVARIANT
 code = 0 or code = 1

OPERATIONS
 Display_M_ = PRINT(M_(G0,mask,ObservationSpace,Q0));
 Display_M_1M = PRINT(M_1M(G0,mask,Q0));
 Display_ObservabilityClasses = PRINT(ObservabilityClasses(G0,mask,ObservationSpace));

 Display_A_0 = PRINT(A(G0,Q0,x0));
 Display_A_1 = PRINT(A(G0,Q0,x1));
 Display_A_2 = PRINT(A(G0,Q0,x2));
 Display_A_3 = PRINT(A(G0,Q0,x3));
 Display_A_4 = PRINT(A(G0,Q0,x4));
 Display_A_5 = PRINT(A(G0,Q0,x5));
 Display_A_6 = PRINT(A(G0,Q0,x6));
 Display_A_hat_0 = PRINT(A_hat(G0,mask,Q0,y0));
 Display_A_hat_1 = PRINT(A_hat(G0,mask,Q0,y1));
 Display_A_hat_2 = PRINT(A_hat(G0,mask,Q0,y2));
 Display_A_hat_3 = PRINT(A_hat(G0,mask,Q0,y3));
 Display_A_breve_0 = PRINT(A_breve(G0,mask,Q0,y0));
 Display_A_breve_1 = PRINT(A_breve(G0,mask,Q0,y1));
 Display_A_breve_2 = PRINT(A_breve(G0,mask,Q0,y2));
 Display_A_breve_3 = PRINT(A_breve(G0,mask,Q0,y3));

 Display_unlblReachTR = PRINT(unlblReachTR(G0,Q0));
 Display_unlblReachTR_hat = PRINT(unlblReachTR_hat(G0,mask,Q0));
 Display_unlblReachTR_breve = PRINT(unlblReachTR_breve(G0,mask,Q0));

 Display_R_ = PRINT(R_(G0,Q0));
 Display_R_hat = PRINT(R_hat(G0,mask,Q0));
 Display_R_breve = PRINT(R_breve(G0,mask,Q0));

 Verify_WeaklyControllable_ =
  PRE WeaklyControllable(G0,Q0) THEN
   PRINT("Q is weakly controllable.") END;
 Verify_Controllable_ =
  PRE not(Controllable(G0,Q0)) THEN
   PRINT("Q is not controllable.") END;
 Verify_StrongMcontrollable =
  PRE not(StrongMcontrollable(G0,mask,Q0)) THEN
   PRINT("Q is not strongly M-controllable.") END;
 Verify_Mcontrollable =
  PRE not(Mcontrollable(G0,mask,Q0)) THEN
   PRINT("Q is not M-controllable.") END;
 Verify_Normal =
  PRE not(Normal(G0,mask,Q0)) THEN
   PRINT("Q is not normal.") END;

  Display_Normality = PRINT(Normality(G0,mask,ObservationSpace, Q0));

 Verify_C1 =
  PRE C1(G0,mask,{},Q0) THEN PRINT("C1 is satisfied.") END;
 Verify_C2 =
  PRE C2(G0,mask,CtrlField(G0,Q0)) THEN
   PRINT("C2 is satisfied.") END;

 Verify_Property_breve =
  PRE Property_breve(G0,mask,CtrlField(G0,Q0)) THEN
   PRINT("The property is satisfied.") END;

 ComputeSupCCP = PRINT(GetsupCCP(Q0));
 ComputeSupCNCP = PRINT(GetsupCNCP(Q0));
 ComputeSupSMCP = PRINT(GetsupSMCP(Q0));
 ComputeMaxMCP = PRINT(GetmaxMCP(Q0));
 ComputeGetmaxMCP = PRINT(GetmaxMCP(Q0));

 ComputeC = PRINT(C(G0,Q0));
 ComputeSMC = PRINT(SMC(G0,mask,Q0));
 ComputeMC = PRINT(MC(G0,mask,Q0));

 Display_SFBC_function =
  LET sup_ BE sup_ = GetsupCCP(Q0) IN
   IF f_e(G0,sup_,e1,x0) THEN PRINT("e1 from x0") END;
   IF f_e(G0,sup_,e2,x0) THEN PRINT("e2 from x0") END;
   IF not(f_e(G0,sup_,e3,x0)) THEN PRINT("not e3 from x0") END;
   PRINT(f_a(G0,sup_));
   PRINT(SFBC(G0,sup_));
   PRINT(SFBCEnabled(G0,sup_)) END;

 Display_SFBC_function_normal =
  LET sup_ BE sup_ = GetsupCNCP(Q0) IN
   IF f_normal_e(G0,sup_,e1,x0) THEN
    PRINT("e1 from x0") END;
   IF f_normal_e(G0,sup_,e2,x0) THEN
    PRINT("e2 from x0") END;
   IF not(f_normal_e(G0,sup_,e3,x0)) THEN
    PRINT("not e3 from x0") END;
   PRINT(f_normal(G0,mask,sup_));
   PRINT(SFBC(G0,sup_));
   PRINT(SFBCEnabled(G0,sup_));
   IF F_0(G0,mask,f_normal(G0,mask,sup_)) THEN
    PRINT("f belongs to F0.") END END;


 Display_SFBC_function_hat =
  LET sup_, f__ BE sup_ = GetsupSMCP(Q0) & f__ = f_a(G0,GetsupCCP(Q0)) IN
   IF f_hat_e(G0,mask,sup_,e1,x0)
    THEN PRINT("e1 from x0") END;
   IF f_hat_e(G0,mask,sup_,e2,x0)
    THEN PRINT("e2 from x0") END;
   IF not(f_hat_e(G0,mask,sup_,e3,x0)) THEN
    PRINT("not e3 from x0") END;
   PRINT(f_hat_a(G0,mask,f__));
   PRINT(SFBC_hat(G0,mask,sup_));
   PRINT(SFBC_hatEnabled(G0,mask,sup_));
   IF F_0(G0,mask,f_hat(G0,mask,sup_)) THEN
    PRINT("f belongs to F0.") END END;

 Display_SFBC_function_breve =
  LET max_, f__ BE max_ = GetmaxMCP(Q0) & f__ = f_a(G0,GetsupCCP(Q0)) IN
   IF f_breve_e(G0,mask,max_,e1,x0) THEN
    PRINT("e1 from x0") END;
   IF f_breve_e(G0,mask,max_,e2,x0) THEN
    PRINT("e2 from x0") END;
   IF not(f_breve_e(G0,mask,max_,e3,x0)) THEN
    PRINT("not e3 from x0") END;
   PRINT(f_breve_a(G0,mask,Q0,f__));
   PRINT(SFBC_breve(G0,mask,max_));
   PRINT(SFBC_breveEnabled(G0,mask,max_));
   IF F_0(G0,mask,f_breve(G0,mask,max_)) THEN
    PRINT("f belongs to F0.") END END 
END

/*
OUTPUT:
{y0,y1,y2,y4,y5,y6,y7}
cover(Display_M_)
{x0,x1,x2,x3,x4,x7,x8,x9,x10,x11}
cover(Display_M_1M)
{{x0},{x1,x2},{x3,x4},{x5,x6},{x7},{x8},{x9,x10},{x11},{x12}}
cover(Display_ObservabilityClasses)
{e3,e5,e6}
cover(Display_A_0)
{}
cover(Display_A_1)
{e9}
cover(Display_A_2)
{}
cover(Display_A_3)
{e4}
cover(Display_A_4)
{}
cover(Display_A_5)
{e6}
cover(Display_A_6)
{e3,e5,e6}
cover(Display_A_hat_0)
{e9}
cover(Display_A_hat_1)
{e4}
cover(Display_A_hat_2)
{e6}
cover(Display_A_hat_3)
{e3,e5,e6}
cover(Display_A_breve_0)
{e9}
cover(Display_A_breve_1)
{e4}
cover(Display_A_breve_2)
{}
cover(Display_A_breve_3)
{(x0|->x1),(x0|->x2),(x0|->x4),(x0|->x7),(x1|->x9),(x3|->x11),(x5|->x11),(x7|->x8)}
cover(Display_unlblReachTR)
{(x0|->x1),(x0|->x2),(x0|->x4),(x0|->x7),(x3|->x11),(x5|->x11),(x7|->x8)}
cover(Display_unlblReachTR_hat)
{(x0|->x1),(x0|->x2),(x0|->x4),(x0|->x7),(x3|->x11),(x5|->x11),(x7|->x8)}
cover(Display_unlblReachTR_breve)
{x0,x1,x2,x4,x7,x8,x9}
cover(Display_R_)
{x0,x1,x2,x4,x7,x8}
cover(Display_R_hat)
{x0,x1,x2,x4,x7,x8}
cover(Display_R_breve)
"Q is weakly controllable."
cover(Verify_WeaklyControllable_)
"Q is not controllable."
cover(Verify_Controllable_)
"Q is not strongly M-controllable."
cover(Verify_StrongMcontrollable)
"Q is not M-controllable."
cover(Verify_Mcontrollable)
"Q is not normal."
cover(Verify_Normal)
{x0,x1,x2,x7,x8,x11}
cover(Display_Normality)
"C1 is satisfied."
cover(Verify_C1)
"C2 is satisfied."
cover(Verify_C2)
"The property is satisfied."
cover(Verify_Property_breve)
{x0,x1,x2,x4,x7,x8,x9}
cover(ComputeSupCCP)
{x0,x1,x2,x7,x8}
cover(ComputeSupCNCP)
{x0,x1,x2,x4,x7,x8}
cover(ComputeSupSMCP)
{x0,x1,x2,x4,x7,x8}
cover(ComputeMaxMCP)
{x0,x1,x2,x4,x7,x8}
cover(ComputeGetmaxMCP)
{{},{x0,x1,x7,x8},{x0,x1,x2,x4,x7,x8},{x0,x1,x2,x7,x8},{x0,x1,x2,x7,x8,x9},
 {x0,x1,x4,x7,x8},{x0,x1,x4,x7,x8,x9},{x0,x1,x2,x4,x7,x8,x9},{x0,x1,x7,x8,x9}}
cover(ComputeC)
{{},{x0,x1,x7,x8},{x0,x1,x2,x4,x7,x8},{x0,x1,x2,x7,x8},{x0,x1,x4,x7,x8}}
cover(ComputeSMC)
{{},{x0,x1,x7,x8},{x0,x1,x2,x4,x7,x8},{x0,x1,x2,x7,x8},{x0,x1,x4,x7,x8},
 {x0,x1,x4,x7,x8,x9},{x0,x1,x7,x8,x9}}
cover(ComputeMC)
"e1 from x0"
"e2 from x0"
"not e3 from x0"
{(x0|->{e1,e2,e4,e7,e8,e9,e10}),(x1|->{e1,e2,e3,e4,e5,e6,e7,e8,e9,e10}),
 (x2|->{e1,e2,e3,e4,e5,e6,e7,e8,e10}),(x3|->{e1,e2,e4,e5,e6,e7,e8,e9,e10}),
 (x4|->{e1,e2,e3,e5,e6,e7,e8,e9,e10}),(x5|->{e1,e2,e3,e4,e6,e7,e8,e9,e10}),
 (x6|->{e1,e2,e3,e4,e5,e7,e8,e9,e10}),(x7|->{e1,e2,e3,e4,e5,e6,e7,e8,e9,e10}),
 (x8|->{e1,e2,e3,e4,e5,e6,e7,e8,e9,e10}),(x9|->{e1,e2,e3,e4,e5,e6,e7,e8,e9,e10}),
 (x10|->{e1,e2,e3,e4,e5,e6,e7,e8,e9,e10}),(x11|->{e1,e2,e3,e4,e5,e6,e7,e8,e9,e10}),
 (x12|->{e1,e2,e3,e4,e5,e6,e7,e8,e9,e10})}
{(x0|->{e3,e5,e6}),(x1|->{}),(x2|->{e9}),(x4|->{e4}),(x7|->{}),(x8|->{}),(x9|->{})}
{(x0|->{e1,e2,e4,e7}),(x1|->{e9}),(x2|->{}),(x4|->{}),(x7|->{e8}),(x8|->{}),(x9|->{})}
cover(Display_SFBC_function)
"e1 from x0"
"e2 from x0"
"not e3 from x0"
{(x0|->{e1,e2,e7}),(x1|->{}),(x2|->{}),(x3|->{}),(x4|->{}),(x5|->{}),(x6|->{}),
 (x7|->{e8}),(x8|->{}),(x9|->{}),(x10|->{}),(x11|->{}),(x12|->{})}
{(x0|->{e3,e4,e5,e6}),(x1|->{e9}),(x2|->{e9}),(x7|->{}),(x8|->{})}
{(x0|->{e1,e2,e7}),(x1|->{}),(x2|->{}),(x7|->{e8}),(x8|->{})}
"f belongs to F0."
cover(Display_SFBC_function_normal)
"e1 from x0"
"e2 from x0"
{(x0|->{e1,e2,e4,e7,e8,e9,e10}),(x1|->{e1,e2,e3,e4,e5,e6,e7,e8,e10}),
 (x2|->{e1,e2,e3,e4,e5,e6,e7,e8,e10}),(x3|->{e1,e2,e5,e6,e7,e8,e9,e10}),
 (x4|->{e1,e2,e5,e6,e7,e8,e9,e10}),(x5|->{e1,e2,e3,e4,e7,e8,e9,e10}),
 (x6|->{e1,e2,e3,e4,e7,e8,e9,e10}),(x7|->{e1,e2,e3,e4,e5,e6,e7,e8,e9,e10}),
 (x8|->{e1,e2,e3,e4,e5,e6,e7,e8,e9,e10}),(x9|->{e1,e2,e3,e4,e5,e6,e7,e8,e9,e10}),
 (x10|->{e1,e2,e3,e4,e5,e6,e7,e8,e9,e10}),(x11|->{e1,e2,e3,e4,e5,e6,e7,e8,e9,e10}),
 (x12|->{e1,e2,e3,e4,e5,e6,e7,e8,e9,e10})}
{(x0|->{e3,e5,e6}),(x1|->{e9}),(x2|->{e9}),(x4|->{e3,e4}),(x7|->{}),(x8|->{})}
{(x0|->{e1,e2,e4,e7}),(x1|->{}),(x2|->{}),(x4|->{}),(x7|->{e8}),(x8|->{})}
"not e3 from x0"
"f belongs to F0."
cover(Display_SFBC_function_hat)
"e1 from x0"
"e2 from x0"
"not e3 from x0"
{(x0|->{e1,e2,e4,e7,e8,e9,e10}),(x1|->{e1,e2,e3,e4,e5,e6,e7,e8,e10}),
 (x2|->{e1,e2,e3,e4,e5,e6,e7,e8,e10}),(x3|->{e1,e2,e3,e5,e6,e7,e8,e9,e10}),
 (x4|->{e1,e2,e3,e5,e6,e7,e8,e9,e10}),(x7|->{e1,e2,e3,e4,e5,e6,e7,e8,e9,e10}),
 (x8|->{e1,e2,e3,e4,e5,e6,e7,e8,e9,e10}),(x9|->{e1,e2,e3,e4,e5,e6,e7,e8,e9,e10}),
 (x10|->{e1,e2,e3,e4,e5,e6,e7,e8,e9,e10}),(x11|->{e1,e2,e3,e4,e5,e6,e7,e8,e9,e10})}
{(x0|->{e3,e5,e6}),(x1|->{e9}),(x2|->{e9}),(x4|->{e4}),(x7|->{}),(x8|->{})}
{(x0|->{e1,e2,e4,e7}),(x1|->{}),(x2|->{}),(x4|->{}),(x7|->{e8}),(x8|->{})}
cover(Display_SFBC_function_breve)

ALL OPERATIONS COVERED
*/