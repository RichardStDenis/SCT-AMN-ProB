// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL section7_projection

SEES
  GandH_1_section7 /*@file "../DataExamples/GandH_1_section7.mch" */
//GandH_2_section7 /*@file "../DataExamples/GandH_2_section7.mch" */
//GandH_3_section7 /*@file "../DataExamples/GandH_3_section7.mch" */
//GandH_4_section7 /*@file "../DataExamples/GandH_4_section7.mch" */

DEFINITIONS
 "../Theories/SCT_core.def";
 "../Theories/SCT_decentralized.def";
 "LibraryIO.def"

CONSTANTS
 L_G, T_G, SfromT_G, Lm_G,   // Two sets of strings, one set of traces, and one set of tasks
 L_H, T_H, SfromT_H, Lm_H,   // Two sets of strings, one set of traces, and one set of tasks

 as, as_P, asP_1, as1_P_1P, as2_P_1P,  // Strings for testing natural projections
 aL_P, aL_P_1, aL_P_1P,                // Languages for testing natural projections

 Pr_as,                                // Set of prefixes of aw
 K1, K2, K3                            // Languages for testing nonconflicting

PROPERTIES
 T_G = GetTraces(G_fb,bound) & T_H = GetTraces(H_lb,bound) &
 SfromT_G = Traces2Strings(T_G) & Lm_G = Traces2Tasks(T_G,G_fb'Xm) &
 SfromT_H = Traces2Strings(T_H) & Lm_H = Traces2Tasks(T_H,H_lb'Xm) &
 L_G = GetStrings(G_fb,bound) & L_H = GetStrings(H_lb,bound) &

 as = [e3,e4,e6,e10] &
 as_P = Ps(as,G_fb'obsEvents) &
 asP_1 = P_1s(as_P,G_fb'obsEvents,G_fb'events) /\ L_G &
 as1_P_1P = P_1Ps(as,G_fb'obsEvents,G_fb'events) /\ L_G &
 as2_P_1P = P_1({as_P},G_fb'obsEvents,G_fb'events) /\ L_G &

 aL_P = P(L_G,G_fb'obsEvents) &
 aL_P_1 = P_1(aL_P,G_fb'obsEvents,G_fb'events) /\ L_G &
 aL_P_1P = P_1P(L_G,G_fb'obsEvents,G_fb'events) /\ L_G &

 Pr_as = Prefix(as) &
 K1 = {[e1,e2],[e3,e4]} &
 K2 = {[e1,e5],[e3,e4]} &  // K1 and K2 are conflicting
 K3 = {[e3,e4]}            // K1 and K3 are nonconflicting

OPERATIONS
 VerifyConsistency = PRE G_fb : Generator_C_O(G_fb'states,G_fb'events) &
                         H_lb : FSA(H_lb'states,H_lb'events) &
                         L_G = SfromT_G &
                         L_H = SfromT_H &
                         asP_1 = as1_P_1P &
                         asP_1 = as2_P_1P &
                         aL_P_1 = L_G &
                         aL_P_1 = aL_P_1P &
                         P_1sR(as_P,G_fb'obsEvents,L_G) = asP_1 &
                         P_1PsR(as,G_fb'obsEvents,L_G) = asP_1 &
                         P_1PR(L_G,G_fb'obsEvents,L_G) = aL_P_1P THEN
                          PRINTF("Equality: ~w.~n",[1]) END;
 ConsistencyPrefixClosed = PRE
  Prefix_closed_(L_H,Lm_H) &
  (Prefix_closed_(L_H,Lm_H) <=> Prefix_closed(L_H,Lm_H)) THEN skip END;
 ConsistencyLmClosed = PRE
  Lm_closed_(L_H,Lm_H,Lm_G) &
  (Lm_closed_(L_H,Lm_H,Lm_G) <=> Lm_closed(L_H,Lm_H,Lm_G)) THEN skip END;
 ConsistencyControllability = PRE
  Controllability_(L_H,L_G,G_fb'events-G_fb'ctrlEvents) &
  (Controllability_(L_H,L_G,G_fb'events-G_fb'ctrlEvents) <=>
   Controllability(L_H,L_G,G_fb'events-G_fb'ctrlEvents) ) THEN skip END;
 ConsistencyNormality = PRE
  Normality_(L_H,L_G,G_fb'obsEvents) &
  (Normality_(L_H,L_G,G_fb'obsEvents) <=>
   Normality(L_H,L_G,G_fb'obsEvents) ) THEN skip END;
 ConsistencyObservability = PRE
  Observability_(L_H,L_G,G_fb'obsEvents,G_fb'events) &
  (Observability_(L_H,L_G,G_fb'obsEvents,G_fb'events) <=>
   Observability(L_H,L_G,G_fb'obsEvents,G_fb'events) ) THEN skip END;

 // To show the difference between PRE and SELECT
 VerifyCtrl1 = PRE                     // with counterexample if unsatisfied
  Controllability(L_H,L_G,G_fb'events-G_fb'ctrlEvents) THEN skip END;
 VerifyCtrl2 = SELECT                  // without counterexample if unsatisfied
  Controllability(L_H,L_G,G_fb'events-G_fb'ctrlEvents) THEN
   PRINT("L_H is controllable.") ELSE PRINT("L_H is uncontrollable.") END;

 VerifyObservability1 = PRE            // according to Wonham and Kumar, Garg
  Observability(L_H,L_G,G_fb'obsEvents,G_fb'events) THEN skip END;
 VerifyObservability2 = PRE            // according to Cassandras, Lafortune
  Observability(L_H,L_G,G_fb'obsEvents, G_fb'ctrlEvents) THEN skip END;
 VerifyObservability3 = PRE            // according to Wonham and Kumar, Garg
  Observability_(L_H,L_G,G_fb'obsEvents,G_fb'events) THEN skip END;
 VerifyObservability4 = PRE            // according to Cassandras, Lafortune
  Observability_(L_H,L_G,G_fb'obsEvents,G_fb'ctrlEvents) THEN skip END;
 VerifyNormality = PRE
  Normality(L_H,L_G,G_fb'obsEvents) THEN skip END;
 VerifyNonconflictingK1K2 = PRE
  Nonconflicting_(K1,K2) THEN skip END;
 VerifyNonconflictingK1K3 = PRE
  Nonconflicting_(K1,K3) THEN skip END END