// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL CASE2008_Ex3_5_2

SETS
 States = {s1, s2, s3, s4, s5, s6, s7,s8, s9};
 Events = {a1, b1, c1, d1, a2, b2, c2, d2, sigma, tau}

CONSTANTS
 A_d,         // Conjunctive or disjunctive decentralized architecture
 G_fb, H_lb,  // Automata for uncontrolled and desired behaviors
 bound        // Bound on the length of sequences

PROPERTIES
 //---------------------------------------------------------------------
 // A simple example for checking m_co-observability properties.
 //   H. Chakib and A. Khousmi,
 //     Multi-decision C&P_D&A architecture for the decentralized control
 //     of dicrete-event systems,
 //     IEEE Conference on Automation Science and Engineering (CASE),
 //     187-193, 2008.
 // Example 3.5.2, Fig. 3.5
 //---------------------------------------------------------------------
 bound = 5
 &
 G_fb = rec(       // Automaton modeling the uncontrolled behavior
            states: States,
            events: Events,
            tf: {(s1,d2,s2), (s1,c2,s3), (s1,a1,s4), (s1,b1,s5),
                 (s2,b1,s4), (s3,a1,s5),
                 (s4,a2,s6), (s4,b2,s7),
                 (s5,b2,s6), (s5,a2,s8),
                 (s6,c1,s7), (s6,d1,s8),
                 (s6,sigma,s9), (s7,sigma,s9), (s8,sigma,s9),
                 (s6,tau,s9), (s7,tau,s9), (s8,tau,s9) },
            x_0: s1,
            Xm: States,
            ctrlEvents: {sigma, tau},
            obsEvents: {a1, b1, c1, d1, a2, b2, c2, d2} )
 &
 H_lb = rec(       // Automaton modeling the desired behavior
            states: States,
            events: Events,
            tf: {(s1,d2,s2), (s1,c2,s3), (s1,a1,s4), (s1,b1,s5),
                 (s2,b1,s4), (s3,a1,s5),
                 (s4,a2,s6), (s4,b2,s7),
                 (s5,b2,s6), (s5,a2,s8),
                 (s6,c1,s7), (s6,d1,s8),
                 (s6,sigma,s9), (s7,sigma,s9),
                 (s8,tau,s9) },
            x_0: s1,
            Xm: States )
 &
 A_d = rec(        // Decentralized architecture
           Events_ce: {tau},
           Events_cd: {sigma},
           NoSupervisors: 2,
           Events_c: {(1,sigma), (1,tau), (2,sigma), (2,tau)},
           Events_o: {(1,a1), (1,b1), (1,c1), (1, d1),
                      (2,a2), (2,b2), (2,c2), (2, d2)} )
&
 A_d'NoSupervisors : NAT1
 &
 !i_.(i_ : 1 .. A_d'NoSupervisors => A_d'Events_c[{i_}] /= {})
 &
 !i_.(i_ : 1 .. A_d'NoSupervisors => A_d'Events_o[{i_}] /= {})
 &
 G_fb'ctrlEvents = UNION (i_).(i_ : 1 .. A_d'NoSupervisors | A_d'Events_c[{i_}])
 &
 G_fb'obsEvents =  UNION (i_).(i_ : 1 .. A_d'NoSupervisors | A_d'Events_o[{i_}])
END