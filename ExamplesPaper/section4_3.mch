// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL section4_3
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
         ctrlEvents: {e3, e4} \/       /* for a nonempty sublanguage */
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
               - {(s4,e6,s6)} - {(s6,e10,s10)},  /* uncontrollable */
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

VARIABLES
 code, 
 K_i, K_succ_i
INITIALISATION
 code := 0;
 PRINT(Lm_H);
 K_i := supR(Lm_H,Lm_G);
 PRINT(K_i);
 K_succ_i := iStepSupC(K_i,pr(K_i),L_G,G'events-G'ctrlEvents);
 PRINT(K_succ_i)
INVARIANT
 code >= 0 & code <= 4 &
 K_i <: Lm_H & K_succ_i <: K_i

OPERATIONS
 GetSupRC = PRE code = 0 & Lm_H <: Lm_G THEN
  WHILE K_i /= K_succ_i DO
   K_i := K_succ_i;
   K_succ_i := iStepSupC(K_i,pr(K_i),L_G,G'events-G'ctrlEvents);
   PRINT(K_succ_i)
  INVARIANT card(K_i) >= 0
  VARIANT card(K_i)
  END;
  code := 1
 END;

 GetSupervisors = PRE code = 1 THEN
  ANY phi WHERE
   phi : Supervisor(pr(K_i),G'events) &
   phi = supervisorENBL(pr(K_i),G'events) THEN PRINT(phi) END;
  ANY phi WHERE
   phi : SRsupervisor(G'states,H'states,G'events) &
   phi = SRsupervisorENBL(T_G,T_H, pr(K_i),G'events) THEN PRINT(phi) END;
  ANY phi WHERE
   phi : SRsupervisor_(G'states,G'events) &
   phi = SRsupervisorENBL_(T_H, pr(K_i),G'events) THEN PRINT(phi) END;
  ANY phi WHERE
   phi : SRsupervisor(G'states,H'states,G'ctrlEvents) &
   phi = SRsupervisorDISBL(T_G,T_H,pr(K_i),L_G,G'ctrlEvents) THEN PRINT(phi) END;
  ANY phi WHERE
   phi : SRsupervisor_(G'states,G'ctrlEvents) &
   phi = SRsupervisorDISBL_(T_G,pr(K_i),L_G,G'ctrlEvents) THEN PRINT(phi) END;
  code := 2
 END;

 GetC_K = PRE code = 2 THEN
  PRINT(C(K_i,L_G,G'events-G'ctrlEvents));
  IF Controllability(pr({[e7,e8,e9,e10]}),L_G,G'events-G'ctrlEvents) THEN
     PRINT("{[e7,e8,e9,e10]} is controllable") ELSE
     PRINT("{[e7,e8,e9,e10]} is not controllable") END;
  IF Controllability(pr({[e3,e2,e5]}),L_G,G'events-G'ctrlEvents) THEN
     PRINT("{[e3,e2,e5]} is controllable") ELSE
     PRINT("{[e3,e2,e5]} is not controllable") END;
  IF Controllability(pr({[e3,e2,e5],[e7,e8,e9,e10]}),L_G,G'events-G'ctrlEvents) THEN
     PRINT("{[e3,e2,e5],[e7,e8,e9,e10]} is controllable") ELSE
     PRINT("{[e3,e2,e5],[e7,e8,e9,e10]} is not controllable") END;
   IF Controllability(pr({}),L_G,G'events-G'ctrlEvents) THEN
     PRINT("{} is controllable") ELSE
     PRINT("{} is not controllable") END;
  code := 3 END
END

/* 
OUTPUT:
{{(1|->e3),(2|->e4)},[e1,e2,e10],[e3,e2,e5],[e3,e4,e5,e10],[e7,e8,e9,e10]}
{{(1|->e3),(2|->e4)},[e3,e2,e5],[e3,e4,e5,e10],[e7,e8,e9,e10]}
{[e3,e2,e5],[e7,e8,e9,e10]}
{[e3,e2,e5],[e7,e8,e9,e10]}
cover(GetSupRC)
{({}|->{e3,e7}),({(1|->e3)}|->{e2}),({(1|->e3),(2|->e2)}|->{e5}),({(1|->e7)}|->{e8}),({(1|->e7),(2|->e8)}|->{e9}),([e3,e2,e5]|->{}),([e7,e8,e9]|->{e10}),([e7,e8,e9,e10]|->{})}
{((s0|->s0)|->{e3,e7}),((s3|->s3)|->{e2}),((s7|->s7)|->{e8}),((s8|->s8)|->{e9}),((s9|->s9)|->{e10}),((s11|->s11)|->{e5})}
{(s0|->{e3,e7}),(s3|->{e2}),(s7|->{e8}),(s8|->{e9}),(s9|->{e10}),(s11|->{e5})}
{((s0|->s0)|->{e1}),((s3|->s3)|->{e4})}
{(s0|->{e1}),(s3|->{e4})}
cover(GetSupervisors)
{{},{[e3,e2,e5],[e7,e8,e9,e10]},{[e7,e8,e9,e10]}}
"{[e7,e8,e9,e10]} is controllable"
"{[e3,e2,e5]} is not controllable"
"{[e3,e2,e5],[e7,e8,e9,e10]} is controllable"
"{} is controllable"
cover(GetC_K)
*/