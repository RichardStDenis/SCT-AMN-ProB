// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL GandH_4_section7

SETS
 States = {s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15};
 Events = {e1, e2, e3, e4, e5, e6, e7, e10}

CONSTANTS
 G_fb, H_lb,  // Automata modeling the free and legal behaviors
 bound        // Bound on the length of sequences

PROPERTIES
 G_fb = rec(states: States,
            events: Events,
            tf: {(s0,e1,s1), (s1,e4,s2), (s2,e5,s3),
                 (s3,e5,s4), (s4,e4,s5),
                 (s3,e6,s6), (s6,e7,s7),
                 (s3,e7,s8), (s8,e7,s9),
                 (s0,e10,s10), (s10,e6,s11), (s11,e6,s12),
                 (s1,e2,s13), (s13,e3,s14), (s13,e4,s15) },
            x_0: s0,
            Xm: {s1, s3, s5, s7, s9, s12, s13, s14, s15},
            ctrlEvents: Events - {e5, e10},
            obsEvents: Events ) &
 H_lb = rec(states: States -{s4, s5},
            events: Events,
            tf: {(s0,e1,s1), (s1,e4,s2), (s2,e5,s3),
                 (s3,e6,s6), (s6,e7,s7),
                 (s3,e7,s8), (s8,e7,s9),
                 (s0,e10,s10), (s10,e6,s11), (s11,e6,s12),
                 (s1,e2,s13), (s13,e3,s14), (s13,e4,s15) },
            x_0: s0,
            Xm: {s1, s7, s9, s12, s14, s15} )
 &
 bound = 5
END

/*
OUTPUT:
Equality: 1.
cover(VerifyConsistency)
cover(ConsistencyNormality)
cover(ConsistencyObservability)
"L_H is uncontrollable."
cover(VerifyCtrl2)
cover(VerifyObservability1)
cover(VerifyObservability2)
cover(VerifyObservability3)
cover(VerifyObservability4)
cover(VerifyNormality)
cover(VerifyNonconflictingK1K3)

 Lm(H) is not prefix-closed.
 Lm(H) is not Lm(G)-closed.
 L(H) is not controllable.
 L(H) is normal.
 L(H) is observable.
 K1 and K2 are conflicting.
 K1 and K3 are nonconflicting.
*/