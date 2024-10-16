// Richard St-Denis, Universite de Sherbrooke, 2022.
MACHINE test_DES_Simple_Properties

SEES
  // ----------------------------------------------------------------------------
  // Machine for input data
  // ----------------------------------------------------------------------------
  all_cases_for_simple_properties /*@file "../DataExamples/all_cases_for_simple_properties.mch" */

CONSTANTS
  prL_G, prL_H, T_G, T_H,    // Languages for free and legal behaviors
  bound                      // Bound on the length of sequences

DEFINITIONS
  // ----------------------------------------------------------------------------
  // ProB settings
  // ----------------------------------------------------------------------------
  SET_PREF_MAX_INITIALISATIONS == 1;
  SET_PREF_MAX_OPERATIONS == 1;
  SET_PREF_TIME_OUT == 1000000;
  SET_PREF_SOLVER_STRENGTH == 1000;
  SET_PREF_SMT == TRUE;
  SET_PREF_KODKOD == FALSE;
  SET_PREF_ALLOW_INCOMPLETE_SETUP_CONSTANTS == TRUE;
  // ----------------------------------------------------------------------------
  // Import the theory
  // ----------------------------------------------------------------------------
  "../Theories/SCT_core.def";
  "../Theories/SCT_decentralized.def";
  "LibraryIO.def"

PROPERTIES
  bound = 4 &
  T_G = GetTraces(G, bound) & prL_G = GetStrings(G, bound) &
  T_H = GetTraces(H, bound) & prL_H = GetStrings(H, bound)

VARIABLES
  code
INITIALISATION
  code := 0
INVARIANT
  code = 0 or code = 1

OPERATIONS
 VerifyPrefixClosed = 
   SELECT Prefix_closed(prL_H, Traces2Tasks(T_H, H'Xm)) THEN
     code := 1 || PRINT("K is prefix-closed.") ELSE
     code := 0 || PRINT("K is not prefix-closed.") END;

  VerifyLmClosure =
    SELECT Lm_closed(prL_H, Traces2Tasks(T_H, H'Xm), Traces2Tasks(T_G, G'Xm)) THEN
      code := 1 || PRINT("K is Lm(G)-closed.") ELSE
      code := 0 || PRINT("K is not Lm(G)-closed.")
    END;

  VerifyCtrl =
    SELECT Controllability(prL_H, prL_G, G'events-G'ctrlEvents) THEN
      code := 1 || PRINT("K is controllable.") ELSE
      code := 0 || PRINT("K is uncontrollable.")
  END;

  VerifyNormality =
    SELECT Normality(prL_H, prL_G, G'obsEvents) THEN
      code := 1 || PRINT("K is normal.") ELSE
      code := 0 || PRINT("K is not normal.")
  END;

  VerifyObservability =
    PRE Observability(prL_H, prL_G, G'obsEvents, G'events) THEN
      code := 1 || PRINT("K is observable.")

//    SELECT Observability(prL_H, prL_G, G'obsEvents, G'events) THEN
//      code := 1 || PRINT("K is observable.") ELSE
//      code := 0 || PRINT("K is not observable.")
  END


//  VerifyObservability =
//  SELECT 
//    code = 1 & Observability(G, M, H, K) THEN
//      PRINT("K is observable.")
//    ELSE
//      code := 0
//  END
END

/*
OUTPUT:
"K is not prefix-closed."
cover(VerifyPrefixClosed)
"K is Lm(G)-closed."
cover(VerifyLmClosure)
"K is uncontrollable."
cover(VerifyCtrl)
"K is not normal."
cover(VerifyNormality)
"K is observable."
cover(VerifyObservability)
*/