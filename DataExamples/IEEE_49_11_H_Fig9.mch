// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL IEEE_49_11_H_Fig9

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
 // Theorem 2 with H of Fig. 9.
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
            states: {s1, s2, s3, s4, s5, s8, s9, s10},
            events: Events,
            tf: {(s1,b,s2), (s1,c,s3), (s1,a,s4),
                 (s2,ap,s5), (s5,c,s9),
                 (s4,bp,s8), (s8,c,s10) },
            x_0: s1,
            Xm: {s1, s2, s3, s4, s5, s8, s9, s10} )
 &
 A_d = rec(        // Decentralized architecture
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
"K is conditionally C&P coobservable_ ."
cover(VerifyGCCPCoobservability_)
"K is conditionally C&P coobservable."
Local decisions of the supervisors (enable):
 {((({(1|->a),(2|->bp)}|->2)|->{(1|->bp)})|->c),((({(1|->b),(2|->ap)}|->1)|->{(1|->ap)})|->c)}
Local decisions of the supervisor (enable if nobody disable) :
 {((({(1|->a),(2|->bp)}|->2)|->{(1|->bp)})|->c),((({(1|->b),(2|->ap)}|->1)|->{(1|->ap)})|->c)}
Local decisions of the supervisors (disable):
 {}
Local decisions of the supervisors (disable if nobody enable):
 {((({(1|->a)}|->1)|->{(1|->a)})|->c),
  ((({(1|->a),(2|->bp)}|->1)|->{(1|->a)})|->c), ((({(1|->a),(2|->bp)}|->2)|->{(1|->bp)})|->c),
  ((({(1|->b)}|->2)|->{(1|->b)})|->c), 
  ((({(1|->b),(2|->ap)}|->1)|->{(1|->ap)})|->c),((({(1|->b),(2|->ap)}|->2)|->{(1|->b)})|->c)}
cover(VerifyGCCPCoobservability)
"K is not conditionally D&A coobservable _."
cover(VerifyGCDACoobservability_)
"K is not conditionally D&A coobservable."
cover(VerifyGCDACoobservability)
cover(cntrexGCCPCoobservability)
*/