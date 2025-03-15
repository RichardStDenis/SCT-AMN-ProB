// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL section4_1
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------
SETS
 States = {s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10};
 Events = {e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10}

CONSTANTS G_FB, T_G_FB, L_G_FB, Lm_G_FB, bound_T_G_FB,
          G_LB, T_G_LB, L_G_LB, Lm_G_LB, bound_T_G_LB

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 5000000;
 SET_PREF_SMT == FALSE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../Theories/SCT_core.def";
 "LibraryIO.def";

PROPERTIES
 G_FB = rec(            // Automaton modeling the free behavior
         states: States,
         events: Events,
         tf: {(s0,e1,s1), (s1,e2,s2), (s2,e10,s10),
              (s0,e3,s3), (s3,e4,s4), (s4,e5,s5), (s5,e10,s10),
                                      (s4,e6,s6), (s6,e10,s10),
              (s0,e7,s7), (s7,e8,s8), (s8,e9,s9), (s9,e10,s10) },
         x_0: s0,
         Xm: // States \/         /* prefix closed  */
             {s4, s10},
         ctrlEvents: {e1, e2, e4, e8, e9} ) &
 G_FB : Generator_C(G_FB'states,G_FB'events)
 &
 bound_T_G_FB = 8
 &
 G_LB = rec(            // Automaton modeling the legal behavior
         states: States
//                 - {s6}    /* uncontrollable */ 
                 - {s2},
         events: Events,
         tf: {(s0,e1,s1), (s1,e2,s2), (s2,e10,s10),
              (s0,e3,s3), (s3,e4,s4), (s4,e5,s5), (s5,e10,s10),
                                      (s4,e6,s6), (s6,e10,s10),
              (s0,e7,s7), (s7,e8,s8), (s8,e9,s9), (s9,e10,s10) }
               - {(s1,e2,s2)}
//               - {(s4,e6,s6)} - {(s6,e10,s10)} /* uncontrollable */
               - {(s2,e10,s10)},
         x_0: s0,
         Xm: // States - {s2}     /* prefix closed  */
             {s10} //)
             /* LmG-closed */ \/ {s4} )
 & 
 G_LB: FSA(G_LB'states,G_LB'events)
 &
 bound_T_G_LB = 8

// Prefix-closed: G_FB'Xm = States" and "G_LB'Xm = States - {x2}".
// not relatively closed: remove "\/ {s4}" in marked states of G_LB.
// controllable: remove "- {s6}" in states of H and
//               remove "- {(s4,e6,s6)} - {(s6,e10,s10)}" in
//               transitions of G_LB.
 &
 T_G_FB = GetTraces(G_FB,bound_T_G_FB) &
 T_G_LB = GetTraces(G_LB,bound_T_G_LB) &
 L_G_FB = Traces2Strings(T_G_FB) & Lm_G_FB = Traces2Tasks(T_G_FB,G_FB'Xm) &
 L_G_LB = Traces2Strings(T_G_LB) & Lm_G_LB = Traces2Tasks(T_G_LB,G_LB'Xm)

OPERATIONS
 VerifyProperties = PRE Lm_G_LB <: Lm_G_FB THEN
  IF Controllability(pr(Lm_G_LB),L_G_FB,G_FB'events-G_FB'ctrlEvents) THEN
     PRINT("Controllability.") END;
  IF Relative_closure(pr(Lm_G_LB),Lm_G_LB,Lm_G_FB) THEN
     PRINT("Relative closure.") END;
  IF Prefix_closed_(Lm_G_LB,L_G_LB) THEN 
     PRINT("Prefix closed.") END END;

 checkResults = PRE
  Controllability_(pr(Lm_G_LB),L_G_FB,G_FB'events-G_FB'ctrlEvents) <=>
   Controllability(pr(Lm_G_LB),L_G_FB,G_FB'events-G_FB'ctrlEvents) &
  Relative_closure_(pr(Lm_G_LB),Lm_G_LB,Lm_G_FB) <=>
   Relative_closure(pr(Lm_G_LB),Lm_G_LB,Lm_G_FB)
 THEN PRINT("The results are consistent.") END
END