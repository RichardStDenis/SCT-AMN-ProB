// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL section4_2
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------
SETS
 States = {s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12};
 Events = {e1, e2, e3, e4, e5, e6, e7, e8, e9, e10}

CONSTANTS G, T_G, L_G, Lm_G, bound_T_G,
          H, T_H, Lm_H, bound_T_H

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
              (s3,e2,s11), (s11,e5,s12),
              (s0,e3,s3), (s3,e4,s4), (s4,e5,s5), (s5,e10,s10),
                                      (s4,e6,s6), (s6,e10,s10),
              (s0,e7,s7), (s7,e8,s8), (s8,e9,s9), (s9,e10,s10) },
         x_0: s0,
         Xm: {s2, s4, s10, s12},
         ctrlEvents: // {e3} \/        /* for a nonempty sublanguage */
                     {e1, e2} )
 &
 bound_T_G = 8
 &
 H = rec(               // Automaton modeling the legal behavior
         states: States
                 - {s6},     /* uncontrollable */ 
         events: Events,
         tf: {(s0,e1,s1), (s1,e2,s2), (s2,e10,s10),
              (s3,e2,s11), (s11,e5,s12),
              (s0,e3,s3), (s3,e4,s4), (s4,e5,s5), (s5,e10,s10),
                                      (s4,e6,s6), (s6,e10,s10),
              (s0,e7,s7), (s7,e8,s8), (s8,e9,s9), (s9,e10,s10) }
               - {(s4,e6,s6)} - {(s6,e10,s10)}, /* uncontrollable */
         x_0: s0,
         Xm: {s4, s10, s12} )
 & 
 H: FSA(H'states,H'events)
 &
 bound_T_H = 8
 &
 T_G = GetTraces(G,bound_T_G) &
 T_H = GetTraces(H,bound_T_H) &
 L_G = Traces2Strings(T_G) & Lm_G = Traces2Tasks(T_G,G'Xm) &
 Lm_H = Traces2Tasks(T_H,H'Xm)

VARIABLES K_i, K_succ_i
INITIALISATION
  PRINT(Lm_H);
  K_i := supR(Lm_H,Lm_G);
  PRINT(K_i);
//K_succ_i := iStepSupC(K_i,pr(K_i),L_G,G'events-G'ctrlEvents);
  K_succ_i := iStepSupC_(K_i,pr(K_i),L_G,G'events-G'ctrlEvents);
  PRINT(K_succ_i)
INVARIANT K_i <: Lm_H & K_succ_i <: K_i

OPERATIONS
 GetSupRC = PRE Lm_H <: Lm_G THEN
  WHILE K_i /= K_succ_i DO
   K_i := K_succ_i;
// K_succ_i := iStepSupC(K_i,pr(K_i),L_G,G'events-G'ctrlEvents);
   K_succ_i := iStepSupC_(K_i,pr(K_i),L_G,G'events-G'ctrlEvents);
   PRINT(K_succ_i)
  INVARIANT card(K_i) >= card(K_succ_i)
  VARIANT card(K_i)
  END
 END;

 VerifyProperties = PRE
  Controllability_(pr(K_i),L_G,G'events-G'ctrlEvents) &
  Relative_closure_(pr(K_i),K_i,Lm_G) THEN skip END
END

/*
OUTPUT:
{{(1|->e3),(2|->e4)},[e1,e2,e10],[e3,e2,e5],[e3,e4,e5,e10],[e7,e8,e9,e10]}
{{(1|->e3),(2|->e4)},[e3,e2,e5],[e3,e4,e5,e10],[e7,e8,e9,e10]}
{[e3,e2,e5],[e7,e8,e9,e10]}
{[e7,e8,e9,e10]}
{}
{}
cover(GetSupRC)
cover(VerifyProperties)

or (when e3 is also controllable)

{{(1|->e3),(2|->e4)},[e1,e2,e10],[e3,e2,e5],[e3,e4,e5,e10],[e7,e8,e9,e10]}
{{(1|->e3),(2|->e4)},[e3,e2,e5],[e3,e4,e5,e10],[e7,e8,e9,e10]}
{[e3,e2,e5],[e7,e8,e9,e10]}
{[e7,e8,e9,e10]}
{[e7,e8,e9,e10]}
cover(GetSupRC)
cover(VerifyProperties)
*/