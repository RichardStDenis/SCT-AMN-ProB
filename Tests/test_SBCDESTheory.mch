// copyright  Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL test_SBCDESTheory

SEES
 // ----------------------------------------------------------------------------
 // Machine for input data
 // ----------------------------------------------------------------------------
 sbc_normality /*@file "../DataExamples/sbc_normality.mch" */

ABSTRACT_CONSTANTS
 GetSupCWCP,
 GetSupCCP,
 GetSupCNCP,
 GetSupSMCP,
 GetGetmaxMCP

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
 "LibraryIO.def"

PROPERTIES
 G0 : Generator_C(G0'states,G0'events) &
 mask : obsMask(States,ObservationSpace) &

 GetSupCWCP = lambdaGetsupCWCP(G0,GetSupCWCP) &
 GetSupCCP = lambdaGetsupCCP(G0,GetSupCCP) &
 GetSupCNCP = lambdaGetsupCNCP(G0,mask,ObservationSpace,GetSupCNCP) &
 GetSupSMCP = lambdaGetsupSMCP(G0,mask,GetSupSMCP) &
 GetGetmaxMCP = lambdaGetmaxMCP(G0,mask,GetGetmaxMCP)

VARIABLES
 code

INITIALISATION
 code := 0

INVARIANT
 code = 0 or code = 1

OPERATIONS
 Use_M_ = PRINT(M_(G0,mask,ObservationSpace,Q0));
 USE_M_1M = PRINT(M_1M(G0,mask,Q0));
 USE_ObservabilityClasses = PRINT(ObservabilityClasses(G0,mask,ObservationSpace));

 USE_CtrlField = PRINT(CtrlField(G0,Q0));
 Test_Idempotent = PRE Idempotent(G0,Q0) THEN
  PRINT("Idempotent verified") END;
 Verify_WeaklyControllable =
  SELECT WeaklyControllable(G0,Q0) THEN
   code := 1 || PRINT("Q0 is weakly controllable.") ELSE
   code := 0 || PRINT("Q0 is not weakly 1controllable.") END;

 Verify_Controllable =
  SELECT Controllable(G0,Q0) THEN
   code := 1 || PRINT("Q0 is controllable.") ELSE
   code := 0 || PRINT("Q0 is not controllable.") END;

 Verify_Normal =
  SELECT Normal(G0,mask,Q0) THEN
   code := 1 || PRINT("Q0 is normal.") ELSE
   code := 0 || PRINT("Q0 is not normal.") END;

 Use_Normality = PRINT(Normality(G0,mask,ObservationSpace,Q0));

 Verify_StrongMcontrollable =
  SELECT StrongMcontrollable(G0,mask,Q0) THEN
   code := 1 || PRINT("Q0 is strong M-controllable.") ELSE
   code := 0 || PRINT("Q0 is not strong M-controllable.") END;

 Verify_Mcontrollable =
  SELECT Mcontrollable(G0,mask,Q0) THEN
   code := 1 || PRINT("Q0 is M-controllable.") ELSE
   code := 0 || PRINT("Q0 is not M-controllable.") END;

 USE_Approx = PRINT(ApproxQbyMCpred(G0,mask,Q0));

 USE_GetSupWCCP =
  LET sup BE sup = GetSupCWCP(Q0) IN
   PRINT(sup);
   PRINT(SFBC(G0,sup));
   PRINT(SFBCEnabled(G0,sup))
  END;

 USE_GetSupCCP =
  LET sup BE sup = GetSupCCP(Q0) IN
   PRINT(sup);
   PRINT(SFBC(G0,sup));
   PRINT(SFBCEnabled(G0,sup))
  END;

 USE_GetSupNCCP =
  LET sup BE sup = GetSupCNCP(Q0) IN
   PRINT(sup);
   PRINT(SFBC(G0,sup));
   PRINT(SFBCEnabled(G0,sup))
  END;

 USE_GetSupSMCP =
  LET sup BE sup = GetSupSMCP(Q0) IN
   PRINT(sup);
   PRINT(SFBC_hat(G0,mask,sup));
   PRINT(SFBC_hatEnabled(G0,mask,sup))
  END;

 USE_GetmaxMCP = 
  LET sup BE sup = GetGetmaxMCP(Q0) IN
   PRINT(sup);
   PRINT(SFBC_breve(G0,mask,sup));
   PRINT(SFBC_breveEnabled(G0,mask,sup))
  END;

 USE_NC = PRINT(NC(G0,mask,Q0)) END

/*
OUTPUT:
{y0,y1,y2,y4,y5,y6,y7}
cover(Use_M_)
{x0,x1,x2,x3,x4,x7,x8,x9,x10,x11}
cover(USE_M_1M)
{{x0},{x1,x2},{x3,x4},{x5,x6},{x7},{x8},{x9,x10},{x11},{x12}}
cover(USE_ObservabilityClasses)
{x0,x1,x2,x4,x7,x8,x9,x11}
cover(USE_CtrlField)
"Idempotent verified"
cover(Test_Idempotent)
"Q0 is weakly controllable."
cover(Verify_WeaklyControllable)
"Q0 is not controllable."
cover(Verify_Controllable)
"Q0 is not normal."
cover(Verify_Normal)
{x0,x1,x2,x7,x8,x11}
cover(Use_Normality)
"Q0 is not strong M-controllable."
cover(Verify_StrongMcontrollable)
"Q0 is not M-controllable."
cover(Verify_Mcontrollable)
{x0,x1,x2,x4,x7,x8}
cover(USE_Approx)
{x0,x1,x2,x4,x7,x8,x9,x11}
{(x0|->{e3,e5,e6}),(x1|->{}),(x2|->{e9}),(x4|->{e4}),(x7|->{}),(x8|->{}),(x9|->{}),(x11|->{})}
{(x0|->{e1,e2,e4,e7}),(x1|->{e9}),(x2|->{}),(x4|->{}),(x7|->{e8}),(x8|->{}),(x9|->{}),(x11|->{})}
cover(USE_GetSupWCCP)
{x0,x1,x2,x4,x7,x8,x9}
{(x0|->{e3,e5,e6}),(x1|->{}),(x2|->{e9}),(x4|->{e4}),(x7|->{}),(x8|->{}),(x9|->{})}
{(x0|->{e1,e2,e4,e7}),(x1|->{e9}),(x2|->{}),(x4|->{}),(x7|->{e8}),(x8|->{}),(x9|->{})}
cover(USE_GetSupCCP)
{x0,x1,x2,x7,x8}
{(x0|->{e3,e4,e5,e6}),(x1|->{e9}),(x2|->{e9}),(x7|->{}),(x8|->{})}
{(x0|->{e1,e2,e7}),(x1|->{}),(x2|->{}),(x7|->{e8}),(x8|->{})}
cover(USE_GetSupNCCP)
{x0,x1,x2,x4,x7,x8}
{(x0|->{e3,e5,e6}),(x1|->{e9}),(x2|->{e9}),(x4|->{e3,e4}),(x7|->{}),(x8|->{})}
{(x0|->{e1,e2,e4,e7}),(x1|->{}),(x2|->{}),(x4|->{}),(x7|->{e8}),(x8|->{})}
cover(USE_GetSupSMCP)
{x0,x1,x2,x4,x7,x8}
{(x0|->{e3,e5,e6}),(x1|->{e9}),(x2|->{e9}),(x4|->{e4}),(x7|->{}),(x8|->{})}
{(x0|->{e1,e2,e4,e7}),(x1|->{}),(x2|->{}),(x4|->{}),(x7|->{e8}),(x8|->{})}
cover(USE_GetmaxMCP)
{{},{x0,x1,x2,x7,x8}}
cover(USE_NC)

ALL OPERATIONS COVERED
*/