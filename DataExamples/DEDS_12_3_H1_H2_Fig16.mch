// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL DEDS_12_3_H1_H2_Fig16

SETS
 States = {s1, s2, s3, s4, s5, s6};
 Events = {alpha, beta, gamma}

CONSTANTS
 A_d,                   // Conjunctive or disjunctive decentralized architecture
 G_fb, H1_lb, H2_lb,    // Automata for uncontrolled and desired behaviors
 bound                  // Bound on the length of sequences

PROPERTIES
 //---------------------------------------------------------------------
 // A simple example for checking that co-observability is not preserved
 // under intersection of languages
 //   T.-S. Yoo and S. Lafortune,
 //     A general architecture for decentralized supervisory control
 //     of dicrete-event systems,
 //     Discrete Event Dynamic Systems: Theory and Applications,
 //     12 (3), 335-377, 2002.
 // Proposition 7 with H of Fig. 16.
 //---------------------------------------------------------------------
 bound = 2
 &
 G_fb = rec(       // Automaton modeling the uncontrolled behavior
            states: States,
            events: Events,
            tf: {(s1,gamma,s2), (s1,beta,s3), (s1,alpha,s4),
                 (s3,gamma,s5), (s4,gamma,s6) },
            x_0: s1,
            Xm: States,
            ctrlEvents: {gamma},
            obsEvents: {alpha, beta} )
 &
 H1_lb = rec(      // Automaton modeling the desired behavior
             states: {s1, s2, s3, s4, s5},
             events: Events,
             tf: {(s1,gamma,s2), (s1,beta,s3), (s1,alpha,s4),
                  (s3,gamma,s5) },
             x_0: s1,
             Xm: {s1, s2, s3, s4, s5} )
 &
 H2_lb = rec(      // Automaton modeling the desired behavior
             states: {s1, s2, s3, s4, s6},
             events: Events,
             tf: {(s1,gamma,s2), (s1,beta,s3), (s1,alpha,s4),
                  (s4,gamma,s6) },
             x_0: s1,
             Xm: {s1, s2, s3, s4, s6} )
 &
 A_d = rec(    // Decentralized architecture
           CtrlEvents: G_fb'ctrlEvents,
           NoSupervisors: 2,
           Events_c: {(1,gamma), (2,gamma)},
           Events_o: {(1,alpha), (2,beta)} )
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
OUTPUT (test_section7_1_DEDS_12_3_H1_H2_Figs16b):
"L(H1) is D&A coobservable."
cover(VerifyDACoobservabilityH1)
"L(H1) is D&A coobservable _."
cover(VerifyDACoobservability_H1)
"L(H2) is D&A coobservable."
cover(VerifyDACoobservabilityH2)
"L(H2) is D&A coobservable _."
cover(VerifyDACoobservability_H2)
"L(H1) /\ L(H2) is not D&A coobservable."
cover(VerifyDACoobservabilityH1H2)
"L(H1) /\ L(H2) is not D&A coobservable _."
cover(VerifyDACoobservability_H1H2)
*/