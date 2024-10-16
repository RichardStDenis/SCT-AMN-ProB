// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL DEDS_12_3_H_Fig11b

SETS 
 States = {s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11};
 Events = {alpha, beta, gamma}

CONSTANTS
 A_d,              // General decentralized  architecture
 G_fb, H_lb,       // Automata for uncontrolled and desired behaviors
 bound             // Bound on the length of sequences

PROPERTIES
 //---------------------------------------------------------------------
 // A simple example for checking co-observability properties.
 //   T.-S. Yoo and S. Lafortune,
 //     A general architecture for decentralized supervisory control
 //     of dicrete-event systems,
 //     Discrete Event Dynamic Systems: Theory and Applications,
 //     12 (3), 335-377, 2002.
 // Proposition 5 with H of Fig. 11b.
 //---------------------------------------------------------------------
 bound = 4
 &
 G_fb = rec(       // Automaton modeling the uncontrolled behavior
            states: States,
            events: Events,
            tf: {(s1,gamma,s2), (s1,beta,s3), (s1,alpha,s4),
                 (s3,gamma,s5), (s4,gamma,s6),
                 (s5,gamma,s7), (s5,beta,s8), (s5,alpha,s9),
                 (s8,gamma,s10), (s9,gamma,s11) },
            x_0: s1,
            Xm: States,
            ctrlEvents: {gamma},
            obsEvents: {alpha, beta, gamma} )
 &
 H_lb = rec(       // Automaton modeling the desired behavior
            states: {s1, s3, s4, s5, s6, s7, s8, s9},
            events: Events,
            tf: {(s1,beta,s3), (s1,alpha,s4),
                 (s3,gamma,s5), (s4,gamma,s6),
                 (s5,gamma,s7), (s5,beta,s8), (s5,alpha,s9) },
            x_0: s1,
            Xm: {s1, s3, s4, s5, s6, s7, s8, s9} )
 &
 A_d = rec(        // Decentralized architecture
           CtrlEvents: G_fb'ctrlEvents,
           Events_ce: {gamma},
           Events_cd: {gamma}-{gamma},     // To infer type of A_d
//         Events_ce: {gamma}-{gamma},     // To infer type of A_a
//         Events_cd: {gamma},
           NoSupervisors: 2,
           Events_c: {(1,gamma), (2,gamma)},
           Events_o: {(1,alpha), (1,gamma),
                      (2,beta), (2,gamma) } )
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
OUTPUT for the first case (test_section7_1_DEDS_12_3_H_Figs8b_11b):
"K is observable."
cover(VerifyObservability)
"K is not C&P coobservable -."
cover(VerifyGCPCoobservability_)
"K is not C&P coobservable."
cover(VerifyGCPCoobservability)
"K is D&A coobservable _."
cover(VerifyGDACoobservability_)
"K is D&A coobservable."
cover(VerifyGDACoobservability)
"K is not coobservable."
cover(VerifyCoobservability)
cover(cntrexGDACoobservability)

----------------------------------------

OUTPUT for the second case (test_section7_1_DEDS_12_3_H_Figs8b_11b):
"K is observable."
cover(VerifyObservability)
"K is C&P coobservable _."
cover(VerifyGCPCoobservability_)
"K is C&P coobservable."
cover(VerifyGCPCoobservability)
"K is not D&A coobservable _."
cover(VerifyGDACoobservability_)
"K is not D&A coobservable."
cover(VerifyGDACoobservability)
"K is not coobservable."
cover(VerifyCoobservability)
cover(cntrexGCPCoobservability)

----------------------------------------

OUTPUT for the third case (section7_1_DEDS_12_3_H_Figs8b_11bPartition):
 =INIT=> i := 1
 [=OK= 0 ms]
*/