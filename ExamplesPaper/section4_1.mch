// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL section4_1
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------
SETS
 States = {s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10};
 Events = {e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10}

CONSTANTS G, T_G, L_G, Lm_G, bound_T_G,
          H, T_H, L_H, Lm_H, bound_T_H

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
 G = rec(               // Automaton modeling the free behavior
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
 G : Generator_C(G'states,G'events)
 &
 bound_T_G = 8
 &
 H = rec(               // Automaton modeling the legal behavior
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
 H: FSA(H'states,H'events)
 &
 bound_T_H = 8

// Prefix-closed: G'Xm = States" and "H'Xm = States - {x2}".
// not relatively closed: remove "\/ {s4}" in marked states of H.
// controllable: remove "- {s6}" in states of H and
//               remove "- {(s4,e6,s6)} - {(s6,e10,s10)}" in
//               transitions of H.
 &
 T_G = GetTraces(G,bound_T_G) &
 T_H = GetTraces(H,bound_T_H) &
 L_G = Traces2Strings(T_G) & Lm_G = Traces2Tasks(T_G,G'Xm) &
 L_H = Traces2Strings(T_H) & Lm_H = Traces2Tasks(T_H,H'Xm)

OPERATIONS
 VerifyProperties = PRE Lm_H <: Lm_G &
  Controllability(pr(Lm_H),L_G,G'events-G'ctrlEvents) &
  Relative_closure(pr(Lm_H),Lm_H,Lm_G) &
  Prefix_closed_(Lm_H,L_H) THEN skip END;

 checkResults = PRE
  Controllability_(pr(Lm_H),L_G,G'events-G'ctrlEvents) <=>
   Controllability(pr(Lm_H),L_G,G'events-G'ctrlEvents) &
  Relative_closure_(pr(Lm_H),Lm_H,Lm_G) <=>
   Relative_closure(pr(Lm_H),Lm_H,Lm_G)
 THEN skip END
END