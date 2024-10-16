// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL GandH_3_section7

SETS
 States = {s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10};
 Events = {e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10}

CONSTANTS
 G_fb, H_lb,  // Automata modeling the free and legal behaviors
 bound        // Bound on the length of sequences

PROPERTIES
 G_fb = rec(states: States,
            events: Events,
            tf: {(s0,e1,s1), (s1,e2,s2), (s2,e10,s10),
                 (s0,e3,s3), (s3,e4,s4),
                 (s4,e5,s5), (s5,e10,s10), (s4,e6,s6), (s6,e10,s10),
                 (s0,e7,s7), (s7,e8,s8), (s8,e9,s9), (s9,e10,s10) },
            x_0: s0,
            Xm: {s4, s10},
            ctrlEvents: {e2, e4, e8, e9},
            obsEvents: Events - {e4, e5, e6, e8} ) &
 H_lb = rec(states: States - {s2},
            events: Events - {e2},
            tf: {(s0,e1,s1),
                 (s0,e3,s3), (s3,e4,s4),
                 (s4,e5,s5), (s5,e10,s10), (s4,e6,s6),
                 (s0,e7,s7), (s7,e8,s8), (s8,e9,s9), (s9,e10,s10) },
            x_0: s0,
            Xm: States - {s2} ) &
 bound = 4
END

/*
OUTPUT:
Equality: 1.
cover(VerifyConsistency)
cover(ConsistencyPrefixClosed)
"L_H is uncontrollable."
cover(VerifyCtrl2)
cover(VerifyObservability2)
cover(VerifyObservability4)
cover(VerifyNonconflictingK1K3)

 Lm(H) is prefix-closed.
 Verifying Lm(G)-closed is irrelevant because the
  assumption "Lm(H) included in Lm(G)" is not verified.
 L(H) is not controllable.
 L(H) is not normal.
 L(H) is not observable according to the definitions of Wonham and Kumar, Garg.
 L(H) is observable according to the definition of Cassandras, Lafortune
      because e10 is not controllable.
 K1 and K2 are conflicting.
 K1 and K3 are nonconflicting.
*/
