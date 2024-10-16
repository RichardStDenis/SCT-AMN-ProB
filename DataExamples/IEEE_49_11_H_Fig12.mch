// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL IEEE_49_11_H_Fig12

SETS
 States = {s1, s2, s3, s4, s5, s6, s7, s8, s9, s10,
           s11, s12, s13, s14, s15, s16, s17, s18, s19, s20 };
 Events = {a, ap, b, bp, c, d}

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
 // Theorem 4 with H of Fig. 12.
 //----------------------------------------------------------------------
 bound = 5
 &
 G_fb = rec(       // Automaton modeling the uncontrolled behavior
            states: States,
            events: Events,
            tf: {(s1,b,s2), (s1,c,s3), (s1,a,s4),
                 (s2,ap,s5), (s2,c,s6),
                 (s3,d,s11),
                 (s4,c,s7), (s4,bp,s8),
                 (s5,c,s9), (s8,c,s10),
                 (s11,b,s12), (s11,d,s13), (s11,a,s14),
                 (s12,ap,s15), (s12,d,s16),
                 (s14,d,s17), (s14,bp,s18),
                 (s15,d,s19), (s18,d,s20) },
            x_0: s1,
            Xm: States,
            ctrlEvents: {c, d},
            obsEvents: {a, ap, b, bp, c, d} )
 &
 H_lb = rec(       // Automaton modeling the desired behavior
            states: {s1, s2, s3, s4, s5, s8, s9, s10,
                     s11, s12, s14, s15, s16, s17, s18 },
            events: {a, ap, b, bp, c,d},
            tf: {(s1,b,s2), (s1,c,s3), (s1,a,s4),
                 (s2,ap,s5), (s3,d,s11), (s4,bp,s8),
                 (s5,c,s9), (s8,c,s10),
                 (s11,b,s12), (s11,a,s14),
                 (s12,ap,s15), (s12,d,s16),
                 (s14,d,s17), (s14,bp,s18) },
            x_0: s1,
            Xm: {s1, s2, s3, s4, s5, s8, s9, s10,
                 s11, s12, s14, s15, s16, s17, s18} )
 &
 A_d = rec(        // Decentralized architecture
           CtrlEvents: G_fb'ctrlEvents,
           Events_ce: //{c, d},             // test 1, 3
                      {c},                  // test 2
           Events_cd: //{c, d},             // test 1, 3
                      {d},                  // test 2
           NoSupervisors: 2,
           Events_c: {(1,c), (1,d),         // test 1,2
                      (2,c), (2,d) },       // test 1,2
                     //{(1,c), (1,d)},      // test 3
           Events_o: {(1,a), (1,ap), (1,c), (1,d),                      // test 1, 2
                      (2,b), (2,bp), (2,c), (2,d) },                    // test 1, 2
                     //{(2,a), (2, ap), (2,b), (2,bp), (2,c), (2,d)},   // test 3 
           Msks_o: {(1,(a,a)), (1,(ap,ap)), (1,(c,c)), (1,(d,d)),
                    (2,(b,b)), (2,(bp,bp)), (2,(c,c)), (2,(d,d)) } )
 &
 A_d'NoSupervisors : NAT1
 &    // ******** must be removed for test 3 ********
 !i_.(i_ : 1..A_d'NoSupervisors => A_d'Events_c[{i_}] /= {})
 &    // must be removed for test 3
 !i_.(i_ : 1..A_d'NoSupervisors => A_d'Events_o[{i_}] /= {})
 &
 G_fb'ctrlEvents = UNION (i_).(i_ : 1..A_d'NoSupervisors | A_d'Events_c[{i_}])
 &
 G_fb'obsEvents = UNION (i_).(i_ : 1..A_d'NoSupervisors | A_d'Events_o[{i_}]) END

/*
OUTPUT for the case of general architecture (test_section7_1_DEDS_12_3_H_Figs8b_11b):
"K is observable."
cover(VerifyObservability)
"K is not C&P coobservable -."
cover(VerifyGCPCoobservability_)
"K is not C&P coobservable."
cover(VerifyGCPCoobservability)
"K is not D&A coobservable _."
cover(VerifyGDACoobservability_)
"K is not D&A coobservable."
cover(VerifyGDACoobservability)
"K is not coobservable."

Diagnostic:

L_G: {{},{(1|->a)},{(1|->a),(2|->bp)},{(1|->a),(2|->c)},{(1|->b)},{(1|->b),(2|->ap)},
      {(1|->b),(2|->c)},{(1|->c)},{(1|->c),(2|->d)},[b,ap,c],[a,bp,c],[c,d,a],[c,d,b],
      [c,d,d],[c,d,a,bp],[c,d,a,d],[c,d,b,ap],[c,d,b,d],[c,d,a,bp,d],[c,d,b,ap,d] }

L_H: {[],[a],[a,bp],[b],[b,ap],
      [c],[c,d],[b,ap,c],[a,bp,c],[c,d,a],[c,d,b],
      [c,d,a,bp],[c,d,a,d],[c,d,b,ap],[c,d,b,d] }

L_G-L_H: {[a,c],[b,c],[c,d,d],[c,d,a,bp,d],[c,d,b,ap,d] }

E_(L_H,c): {[],[a,bp],[b,ap]}          E(L_H,d):  {[c],[c,d,a],[c,d,b]}
D_(L_H,L_G,c):  {[a],[b]}              D_(L_H,L_G,c): {[c,d],[c,d,a,bp],[c,d,b,ap]}

C&P coobservable:
s = [b], e = c, and [b,c] in L_G-L_H
i= 1:
 P_1PsR([b],A_d'Events_o[{1}],L_H) = {[],[b]}
 P_1PsR([b],A_d'Events_o[{2}],L_H) /\ E_(L_H,c)= {[]} /= {} => [c] in L_H
i =2:
 P_1PsR([b],A_d'Events_o[{2}],L_H) = {[b],[b,ap]}
 P_1PsR([b],A_d'Events_o[{2}],L_H) /\ E_(L_H,c)= {[b,ap]} /= {} => [b,ap,c in L_H]
=> L_H in not C&P coobservable (the same result can be achieved with s = [a]).

D&A coobservable:
s = [c,d,b], e = d, and [c,d,b,d] in L_H
i = 1:
 P_1PsR([c,d,b],A_d'Events_o[{1}],L_H) = {[c,d],[c,d,b]}
 P_1PsR([c,d,b],A_d'Events_o[{1}],L_H) /\ E_(L_H,c)= {[c,d]} => [c,d,d] in L_G-L_H
i = 2:
 P_1PsR([c,d,b],A_d'Events_o[{2}],L_H) = {[c,d,b],[c,d,b,ap]}
 P_1PsR([c,d,b],A_d'Events_o[{2}],L_H) /\ E_(L_H,c)= {[c,d,b,ap]} => [c,d,b,ap,d] in L_G-L_H
=> L_H in not D&A coobservable (the same result can be achieved with s = [c,d,a]).

----------------------------------------

OUTPUT for the first case (test_section7_1_IEEE_49_11_H_Fig12):
"K is observable."
cover(VerifyObservability1)
"K is not observable when all events are unobservable."
cover(VerifyObservability2)
"K is not conditionally coobservable _."
cover(VerifyGCCoobservability_)
### Warning: enumerating s2_ : seq (length) : inf ---> 3
### Warning: enumerating s2_ : seq (length) : inf ---> 3
### Warning: enumerating s2_ : seq (length) : inf ---> 3
### Warning: enumerating s2_ : seq (length) : inf ---> 3
"K is not conditionally coobservable __."
cover(VerifyGCCoobservability__)
"K is not conditionally C&P coobservable -."
cover(VerifyGCCPCoobservability_)
"K is not conditionally C&P coobservable."
cover(VerifyGCCPCoobservability)
"K is not conditionally D&A coobservable _."
cover(VerifyGCDACoobservability_)
"K is not conditionally D&A coobservable."
cover(VerifyGCDACoobservability)

----------------------------------------

OUTPUT for the second case (test_section7_1_IEEE_49_11_H_Fig12):
"K is observable."
cover(VerifyObservability1)
"K is not observable when all events are unobservable."
cover(VerifyObservability2)
"K is conditionally coobservable _."
cover(VerifyGCCoobservability_)
### Warning: enumerating s2_ : seq (length) : inf ---> 3
"K is conditionally coobservable __."
cover(VerifyGCCoobservability__)
"K is conditionally C&P coobservable _."
cover(VerifyGCCPCoobservability_)
"K is conditionally C&P coobservable."
cover(VerifyGCCPCoobservability)
"K is conditionally D&A coobservable _."
cover(VerifyGCDACoobservability_)
"K is conditionally D&A coobservable."
cover(VerifyGCDACoobservability)
"K is conditionally coobservable."
Local decisions (enabled unconditionally): 
 {((({(1|->a),(2|->bp)}|->2)|->{(1|->bp)})|->c),
  ((({(1|->b),(2|->ap)}|->1)|->{(1|->ap)})|->c),
  ((({(1|->c)}|->1)|->{(1|->c)})|->d),
  ((({(1|->c)}|->2)|->{(1|->c)})|->d) }
Local decisions (enabled conditionally): 
 {((({(1|->a),(2|->bp)}|->2)|->{(1|->bp)})|->c),      already enabled unconditionally
  ((({(1|->b),(2|->ap)}|->1)|->{(1|->ap)})|->c),      already enabled unconditionally
  ((({(1|->c)}|->1)|->{(1|->c)})|->d),                already enabled unconditionally
  ((({(1|->c)}|->2)|->{(1|->c)})|->d),                already enabled unconditionally
  ((([c,d,a]|->1)|->[c,d,a])|->d),                    OK
  ((([c,d,b]|->2)|->[c,d,b])|->d),                    OK
  ((([c,d,a,bp]|->1)|->[c,d,a])|->d),                 but disabled by supervisor 2
  ((([c,d,a,bp]|->2)|->[c,d,bp])|->d),                but disabled by supervisor 2
  ((([c,d,b,ap]|->1)|->[c,d,ap])|->d),                but disabled by supervisor 1
  ((([c,d,b,ap]|->2)|->[c,d,b])|->d) }                but disabled by supervisor 1
Local decisions (disabled unconditionally): 
 {((([c,d,a,bp]|->2)|->[c,d,bp])|->d),
  ((([c,d,b,ap]|->1)|->[c,d,ap])|->d) }
Local decisions (disabled conditionally): 
 {((({(1|->a)}|->1)|->{(1|->a)})|->c),                OK
  ((({(1|->a),(2|->bp)}|->1)|->{(1|->a)})|->c),       but enabled by supervisor 2
  ((({(1|->a),(2|->bp)}|->2)|->{(1|->bp)})|->c),      but enabled by supervisor 2
  ((({(1|->b)}|->2)|->{(1|->b)})|->c),                OK
  ((({(1|->b),(2|->ap)}|->1)|->{(1|->ap)})|->c),      but enabled by supervisor 1
  ((({(1|->b),(2|->ap)}|->2)|->{(1|->b)})|->c),       but enabled by supervisor 1
  ((({(1|->c)}|->1)|->{(1|->c)})|->d),
  ((({(1|->c)}|->2)|->{(1|->c)})|->d),
  ((([c,d,a,bp]|->2)|->[c,d,bp])|->d),                already disabled unconditionally
  ((([c,d,b,ap]|->1)|->[c,d,ap])|->d) }               already disabled unconditionally
Global decisions (enabled):            no decision:   c is enabled by default when []
 {({(1|->a),(2|->bp)}|->c),
  ({(1|->b),(2|->ap)}|->c),
  ({(1|->c)}|->d),
  ([c,d,a]|->d),
  ([c,d,b]|->d) }
Global decisions (disabled):           no decision:   d is disabled by default when [c,d]
 {({(1|->a)}|->c),
  ({(1|->b)}|->c),
  ([c,d,a,bp]|->d),
  ([c,d,b,ap]|->d) }
"The control is nonconflicting."
cover(VerifyGCCoobservability)

----------------------------------------

OUTPUT for the third case (test_section7_1_IEEE_49_11_H_Fig12):
"K is observable."
cover(VerifyObservability1)
"K is not observable when all events are unobservable."
cover(VerifyObservability2)
"K is not conditionally coobservable _."
cover(VerifyGCCoobservability_)
"K is not conditionally coobservable __."
cover(VerifyGCCoobservability__)
"K is not conditionally C&P coobservable -."
cover(VerifyGCCPCoobservability_)
"K is not conditionally C&P coobservable."
cover(VerifyGCCPCoobservability)
"K is not conditionally D&A coobservable _."
cover(VerifyGCDACoobservability_)
"K is not conditionally D&A coobservable."
cover(VerifyGCDACoobservability)
*/