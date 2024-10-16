// copyright  Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL test_ICJ_67_4_Ex1_6

SEES
 // ----------------------------------------------------------------------------
 // Machine for input data
 // ----------------------------------------------------------------------------
 sbc_ICJ_67_4_Ex1_6 /*@file "../DataExamples/sbc_ICJ_67_4_Ex1_6.mch" */

ABSTRACT_CONSTANTS
 GetSupWCP,
 GetSupSMCP_1, GetSupSMCP_2,
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
 G1 : Generator_C(G1'states,G1'events) &
 G2 : Generator_C(G2'states,G2'events) &
 mask : obsMask(States,ObservationSpace) &

 GetSupWCP = lambdaGetsupCWCP(G1,GetSupWCP) &
 GetSupSMCP_1 = lambdaGetsupSMCP(G1,mask,GetSupSMCP_1) &
 GetSupSMCP_2 = lambdaGetsupSMCP(G2,mask,GetSupSMCP_2) &
 GetGetmaxMCP = lambdaGetmaxMCP(G1,mask,GetGetmaxMCP)

VARIABLES
 code,
 supWCP_Q0, supWCP_Q1, supWCP_Q2,
 sfbc_d_to_0, sfbc_e_to_0, sfbc_d_to_1, sfbc_e_to_1, sfbc_d_to_2, sfbc_e_to_2

INITIALISATION
 code := 0;
 supWCP_Q0 := GetSupWCP(Q0);
 sfbc_d_to_0 := f_d(G1,supWCP_Q0);
 sfbc_e_to_0 := f_a(G1,supWCP_Q0);
 supWCP_Q1 := GetSupWCP(Q1);
 sfbc_d_to_1 := f_d(G1,supWCP_Q1);
 sfbc_e_to_1 := f_a(G1,supWCP_Q1);
 supWCP_Q2 := GetSupWCP(Q2);
 sfbc_d_to_2 := f_d(G1,supWCP_Q2);
 sfbc_e_to_2 := f_a(G1,supWCP_Q2)

INVARIANT
 code = 0 or code = 1 &
 supWCP_Q0 <: Q0 &
 sfbc_e_to_0 : SFBCf(G1'states,G1'ctrlEvents) &
 sfbc_d_to_0 : SFBCf(G1'states,G1'ctrlEvents) &
 supWCP_Q1 <: Q0 &
 sfbc_e_to_1 : SFBCf(G1'states,G1'ctrlEvents) &
 sfbc_d_to_1 : SFBCf(G1'states,G1'ctrlEvents) &
 supWCP_Q2 <: Q0 &
 sfbc_e_to_2 : SFBCf(G1'states,G1'ctrlEvents) &
 sfbc_d_to_2 : SFBCf(G1'states,G1'ctrlEvents)

OPERATIONS
 Use_M_ = PRINT(M_(G1,mask,ObservationSpace,Q0));
 USE_M_1M = PRINT(M_1M(G1,mask,Q0));
 USE_ObservabilityClasses = PRINT(ObservabilityClasses(G1,mask,ObservationSpace));

 Verify_wp_true =
  SELECT wp_e(G1,s1,Q0,x0) THEN
   code := 1 || PRINT("Satisfied.") ELSE
   code := 0 || PRINT("Unsatisfied.") END;
 Verify_wp_false =
  PRE not(wp_e(G1,s2,Q0,x1)) THEN
   code := 1 || PRINT("Satisfied.") END;

 Verify_wlp_true =
  SELECT wlp_e(G1,s2,Q0,x1) THEN
   code := 1 || PRINT("Satisfied.") ELSE
   code := 0 || PRINT("Unsatisfied.") END;
 Verify_wlp_false =
  PRE not(wlp_e(G1,s1,Q0,x1)) THEN
   code := 1 || PRINT("Satisfied.") END;

 USE_CtrlField = PRINT(CtrlField(G1,Q0));
 Test_Idempotent = PRE Idempotent(G1,Q0) THEN
  PRINT("Idempotent verified") END;
 Verify_WeaklyControllable =
  SELECT WeaklyControllable(G1,Q0) THEN
   code := 1 || PRINT("Q0 is weakly controllable.") ELSE
   code := 0 || PRINT("Q0 is not weakly controllable.") END;

 USE_A_hat_0 = PRINT(A_hat(G1,mask,Q0,y));
 USE_A_hat_1 = PRINT(A_hat(G1,mask,Q1,y));
 USE_A_hat_2 = PRINT(A_hat(G1,mask,Q2,y));
 USE_A_hat_3 = PRINT(A_hat(G1,mask,Q3,y));
 USE_A_hat_4 = PRINT(A_hat(G1,mask,Q4,y));
 USE_A_breve_0 = PRINT(A_breve(G1,mask,Q0,y));
 USE_A_breve_1 = PRINT(A_breve(G1,mask,Q1,y));
 USE_A_breve_2 = PRINT(A_breve(G1,mask,Q2,y));
 USE_A_breve_3 = PRINT(A_breve(G1,mask,Q3,y));
 USE_A_breve_4 = PRINT(A_breve(G1,mask,Q4,y));

 Verify_inclusion_1 = PRE
  A_hat(G1,mask,Q0,y) <: A_hat(G1,mask,Q4,y) &
  A_hat(G1,mask,Q1,y) <: A_hat(G1,mask,Q4,y) &
  A_hat(G1,mask,Q2,y) <: A_hat(G1,mask,Q4,y) THEN
   code := 1 || PRINT("Definitions are consistent.") END;

 Verify_inclusion_2 = PRE
  A_breve(G1,mask,Q0,y) <: A_hat(G1,mask,Q0,y) &
  A_breve(G1,mask,Q1,y) <: A_hat(G1,mask,Q1,y) &
  A_breve(G1,mask,Q2,y) <: A_hat(G1,mask,Q2,y) &
  A_breve(G1,mask,Q3,y) <: A_hat(G1,mask,Q3,y) &
  A_breve(G1,mask,Q4,y) <: A_hat(G1,mask,Q4,y) THEN
   code := 1 || PRINT("Definitions are consistent.") END;

 USE_R_hat_0 = PRINT(R_hat(G1,mask,Q0));
 USE_R_hat_1 = PRINT(R_hat(G1,mask,Q1));
 USE_R_hat_2 = PRINT(R_hat(G1,mask,Q2));
 USE_R_hat_4 = PRINT(R_hat(G1,mask,Q4));
 USE_R_breve_0 = PRINT(R_breve(G1,mask,Q0));
 USE_R_breve_1 = PRINT(R_breve(G1,mask,Q1));
 USE_R_breve_2 = PRINT(R_breve(G1,mask,Q2));
 USE_R_breve_4 = PRINT(R_breve(G1,mask,Q4));

 Verify_inclusion_3 = PRE
  R_hat(G1,mask,Q0) <: R_hat(G1,mask,Q4) &
  R_hat(G1,mask,Q1) <: R_hat(G1,mask,Q4) &
  R_hat(G1,mask,Q2) <: R_hat(G1,mask,Q4) THEN
   code := 1 || PRINT("Definitions are consistent.") END;

 Verify_inclusion_4 = PRE // not to be generalized, since R_breve is not monotone
  R_hat(G1,mask,Q0) <: R_breve(G1,mask,Q0) &
  R_hat(G1,mask,Q1) <: R_breve(G1,mask,Q1) &
  R_hat(G1,mask,Q2) <: R_breve(G1,mask,Q2) &
  R_hat(G1,mask,Q4) <: R_breve(G1,mask,Q4) THEN
   code := 1 || PRINT("Definitions are consistent.") END;

 Verify_C1 =
  SELECT C1(G1,mask,Q1,Q0) THEN
   code := 1 || PRINT("Condition 1 is satisfied.") ELSE
   code := 0 || PRINT("Condition 1 is not satisfied.") END;

 Verify_C2 =
  SELECT C2(G1,mask,Q3) THEN
   code := 1 || PRINT("Condition 2 is satisfied.") ELSE
   code := 0 || PRINT("Condition 2 is not satisfied.") END;

 Verify_assertion =
  SELECT Property_breve(G1,mask,Q1) THEN
   code := 1 || PRINT("The assertion is satisfied.") ELSE
   code := 0 || PRINT("The assertion is not satisfied.") END;

 Verify_StrongMcontrollable =
  SELECT StrongMcontrollable(G1,mask,Q0) THEN
   code := 1 || PRINT("Q0 is strong M-controllable.") ELSE
   code := 0 || PRINT("Q0 is not strong M-controllable.") END;

 Verify_StrongMcontrollable_1 =
  SELECT StrongMcontrollable(G1,mask,Q1) THEN
   code := 1 || PRINT("{x0,x1} is strong M-controllable.") ELSE
   code := 0 || PRINT("{x0,x1} is not strong M-controllable.") END;

 Verify_StrongMcontrollable_2 =
  SELECT StrongMcontrollable(G1,mask,Q2) THEN
   code := 1 || PRINT("{x0,x2} is strong M-controllable.") ELSE
   code := 0 || PRINT("{x0,x2} is not strong M-controllable.") END;

 Verify_StrongMcontrollable_4 =
  SELECT StrongMcontrollable(G2,mask,Q4) THEN
   code := 1 || PRINT("{x0,x1,x2,x3} is strong M-controllable.") ELSE
   code := 0 || PRINT("{x0,x1,x2,x3} is not strong M-controllable.") END;

 Verify_StrongMcontrollable_2_0 =
  SELECT Mcontrollable(G2,mask,Q0) THEN
   code := 1 || PRINT("{x0,x1,x2} is strong M-controllable.") ELSE
   code := 0 || PRINT("{x0,x1,x2} is not strong M-controllable.") END;

 Verify_StrongMcontrollable_2_1 =
  SELECT Mcontrollable(G2,mask,Q1) THEN
   code := 1 || PRINT("{x0,x1} is strong M-controllable.") ELSE
   code := 0 || PRINT("{x0,x1} is not strong M-controllable.") END;

 USE_A_hat6 = PRINT(A_hat(G2,mask,{x0,x1},y));

 Verify_Mcontrollable =
  SELECT Mcontrollable(G1,mask,Q0) THEN
   code := 1 || PRINT("Q0 is M-controllable.") ELSE
   code := 0 || PRINT("Q0 is not M-controllable.") END;

 Verify_Mcontrollable_1 =
  SELECT Mcontrollable(G1,mask,Q1) THEN
   code := 1 || PRINT("{x0,x1} is M-controllable.") ELSE
   code := 0 || PRINT("{x0,x1} is not M-controllable.") END;

 Verify_Mcontrollable_2 =
  SELECT Mcontrollable(G1,mask,Q2) THEN
   code := 1 || PRINT("{x0,x2} is M-controllable.") ELSE
   code := 0 || PRINT("{x0,x2} is not M-controllable.") END;

 USE_Approx = PRINT(ApproxQbyMCpred(G1,mask,Q0));

 USE_GetSupSMCP_1 =
  LET sup BE sup = GetSupSMCP_1(Q0) IN
   PRINT(sup);
   PRINT(SFBC_hat(G1,mask,sup)); PRINT(sup<|f_hat_d(G1,mask,sfbc_d_to_0));
   PRINT(SFBC_hatEnabled(G1,mask,sup)); PRINT(sup<|f_hat_a(G1,mask,sfbc_e_to_0));
   PRINT(f_hat(G1,mask,sup))
  END;
 USE_GETSupSMCP_2 =
  LET sup BE sup = GetSupSMCP_2(Q4) IN
   PRINT(sup); PRINT(SFBC_hat(G2,mask,sup));
   PRINT(SFBC_hatEnabled(G2,mask,sup)); PRINT(f_hat(G2,mask,sup))
  END;

 ComputesupSMCP = VAR Q_i, Q_succ_i IN
  Q_i := {};   Q_succ_i := Q4;
  WHILE Q_i /= Q_succ_i DO
   Q_i := Q_succ_i;
   Q_succ_i := iStepSupSMCP(G2,mask,Q4,Q_i)
  INVARIANT card(Q4) >= 0 VARIANT card(Q_succ_i) END;
  PRINT(Q_i) END;

 USE_GetmaxMCP_0 = 
  LET sup BE sup = GetGetmaxMCP(Q0) IN
   PRINT(sup);
   PRINT(SFBC_breve(G1,mask,sup)); PRINT(sup<|f_breve_d(G1,mask,supWCP_Q0,sfbc_d_to_0));
   PRINT(SFBC_breveEnabled(G1,mask,sup)); PRINT(sup<|f_breve_a(G1,mask,supWCP_Q0,sfbc_e_to_0));
   PRINT(f_breve(G1,mask,sup))
  END;
 USE_GetmaxMCP_1 =
  LET sup BE sup = GetGetmaxMCP(Q1) IN
   PRINT(sup);
   PRINT(SFBC_breve(G1,mask,sup)); PRINT(sup<|f_breve_d(G1,mask,supWCP_Q1,sfbc_d_to_1));
   PRINT(SFBC_breveEnabled(G1,mask,sup)); PRINT(sup<|f_breve_a(G1,mask,supWCP_Q1,sfbc_e_to_1));
   PRINT(f_breve(G1,mask,sup))
  END;
 USE_GetmaxMCP_2 =
  LET sup BE sup = GetGetmaxMCP(Q2) IN
   PRINT(sup);
   PRINT(SFBC_breve(G1,mask,sup)); PRINT(sup<|f_breve_d(G1,mask,supWCP_Q2,sfbc_d_to_2));
   PRINT(SFBC_breveEnabled(G1,mask,sup)); PRINT(sup<|f_breve_a(G1,mask,supWCP_Q2,sfbc_e_to_2));
   PRINT(f_breve(G1,mask,sup))
  END;

 USE_C = PRINT(C(G1,Q0));
 USE_SMC = PRINT(SMC(G1,mask,Q0));
 USE_MC = PRINT(MC(G1,mask,Q0)) END