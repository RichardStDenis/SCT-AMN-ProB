// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL DEDS_12_3_H_Fig6

SETS
 States = {s1, s2, s3, s4, s5, s6};
 Events = {alpha, beta, gamma}

CONSTANTS
 A_d,              // Conjunctive or disjunctive decentralized architecture
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
 // Proposition 3 with H of Fig. 6.
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
            obsEvents: {alpha, beta, gamma} )
 &
 H_lb = rec(       // Automaton modeling the desired behavior
            states: {s1, s2, s3, s4},
            events: Events,
            tf: {(s1,gamma,s2), (s1,beta,s3), (s1,alpha,s4)},
            x_0: s1,
            Xm: {s1, s2, s3, s4} )
 &
 A_d = rec(        // Decentralized architecture
           CtrlEvents: G_fb'ctrlEvents,
           Events_ce: {gamma},
           Events_cd: {gamma},
           NoSupervisors: 2,
           Events_c: {(1,gamma), (2,gamma)},
           Events_o: {(1,alpha), (1,gamma), (2,beta), (2,gamma)} )
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
OUTPUT (test_section7_1_DEDS_12_3_H_Figs4b_6_8b):
"K is C&P coobservable _."
cover(VerifyCPCoobservability_)
"K is C&P coobservable."
Permissive local decisions:
 {(((1|->{})|->alpha)|->"unc"),(((1|->{})|->beta)|->"unc"),(((1|->{})|->gamma)|->"and"),
  (((2|->{})|->alpha)|->"unc"),(((2|->{})|->beta)|->"unc"),(((2|->{})|->gamma)|->"and")}
Permissive global decisions:
 {({}|->alpha),({}|->beta),({}|->gamma)}
cover(VerifyCPCoobservability)
"K is not D&A coobservable. _"
cover(VerifyDACoobservability_)
"K is not D&A coobservable."
cover(VerifyDACoobservability)
cover(cntrexCPCoobservability)
*/