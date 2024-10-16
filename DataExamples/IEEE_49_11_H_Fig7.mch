// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL IEEE_49_11_H_Fig7

SETS
 States = {s1, s2, s3, s4, s5, s6, s7, s8, s9, s10};
 Events = {a, ap, b, bp, c}

CONSTANTS
 A_d,              // Conditional architecture with decentralized fusion rules
 G_fb, H_lb,       // Automata for uncontrolled and desired behaviors
 bound             // Bound on the length of sequences

PROPERTIES
 //---------------------------------------------------------------------
 // A simple example for checking co-observability properties.
 //   T.-S. Yoo and S. Lafortune,
 //     Decentralized supervisory control with conditional decisions:
 //     Supervisor exitence,
 //     IEEE Transactions on Automatic Control,
 //     49 (11), 1886-1904, 2004.
 // Theorem 2 with H of Fig. 7.
 //----------------------------------------------------------------------
 bound = 3
 &
 G_fb = rec(       // Automaton modeling the uncontrolled behavior
            states: States,
            events: Events,
            tf: {(s1,b,s2), (s1,c,s3), (s1,a,s4),
                 (s2,ap,s5), (s2,c,s6),
                 (s4,c,s7), (s4,bp,s8),
                 (s5,c,s9), (s8,c,s10) },
            x_0: s1,
            Xm: States,
            ctrlEvents: {c},
            obsEvents: {a, ap, b, bp, c} )
 &
 H_lb = rec(       // Automaton modeling the desired behavior
            states: {s1, s2, s4, s5, s6, s7, s8},
            events: Events,
            tf: {(s1,b,s2), (s1,a,s4),
                 (s2,ap,s5), (s2,c,s6),
                 (s4,c,s7), (s4,bp,s8) },
            x_0: s1,
            Xm: {s1, s2, s4, s5, s6, s7, s8} )
 &
 A_d = rec(          // Decentralized architecture
           CtrlEvents: G_fb'ctrlEvents,
           Events_ce: {c},
           Events_cd: {c},
           NoSupervisors: 2,
           Events_c: {(1,c), (2,c)},
           Events_o: {(1,a), (1,ap), (1,c),
                      (2,b), (2,bp), (2,c) } )
 &
 A_d'NoSupervisors : NAT1
 &
 !i_.(i_ : 1..A_d'NoSupervisors => A_d'Events_c[{i_}] /= {})
 &
 !i_.(i_ : 1..A_d'NoSupervisors => A_d'Events_o[{i_}] /= {})
 &
 G_fb'ctrlEvents = UNION (i_).(i_ : 1..A_d'NoSupervisors | A_d'Events_c[{i_}])
 &
 G_fb'obsEvents = UNION (i_).(i_ : 1..A_d'NoSupervisors | A_d'Events_o[{i_}]) END

/*
OUTPUT (test_section7_1_IEEE_49_11_H_Fig7_9):
"K is not conditionally C&P coobservable _."
cover(VerifyGCCPCoobservability_)
"K is not conditionally C&P coobservable."
cover(VerifyGCCPCoobservability)
"K is D&A conditionally coobservable _."
cover(VerifyGCDACoobservability_)
"K is D&A conditionally coobservable."
cover(VerifyGCDACoobservability)
cover(cntrexGCDACoobservability)
*/