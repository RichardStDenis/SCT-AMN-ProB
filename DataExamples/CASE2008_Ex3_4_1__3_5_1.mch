// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL CASE2008_Ex3_4_1__3_5_1

SETS
 States = {s1, s2, s3, s4, s5, s6, s7,s8, s9};
 Events = {a1, b1, c1, d1, a2, b2, c2, d2, sigma}

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
 // Example 3.4.1, Fig. 3.2 and example 3.5.1, Fig. 3.4
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
                 (s6,sigma,s9), (s7,sigma,s9), (s8,sigma,s9) },
            x_0: s1,
            Xm: States,
            ctrlEvents: {sigma},
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
                 //(s8,sigma,s9) },                     // Example 3.4.1, Fig 3.2
                 (s6,sigma,s9), (s7,sigma,s9) },    // Example 3.5.1, Fig 3.4
            x_0: s1,
            Xm: States )
 &
 A_d = rec(        // Decentralized architecture
           Events_ce: {sigma},
           Events_cd: {sigma},
           NoSupervisors: 2,
           Events_c: {(1,sigma), (2,sigma)},
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

/*
OUTPUT for Example 3.4.1 Fig 3.2 (test_section7_3_CASE2008_Ex3_4_1__3_5_1):
"K is C&P3-coobservable."
{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}
{[c2,a1,a2]}
{{(1|->b1),(2|->a2)}}
cover(FindPartitionForCP3Coobservability)
"K is D&A2-coobservable."
{{(1|->b1),(2|->b2)},[d2,b1,a2],[d2,b1,b2],[b1,b2,c1]}
{{(1|->a1),(2|->a2)},{(1|->a1),(2|->b2)},[c2,a1,b2],[c2,a1,b2,c1],[d2,b1,a2,c1],[a1,a2,c1]}
cover(FindPartitionForDA2Coobservability)
"K is D&A3-coobservable."
{{(1|->b1),(2|->b2)},[d2,b1,a2],[d2,b1,b2],[b1,b2,c1]}
{{(1|->a1),(2|->a2)},{(1|->a1),(2|->b2)},[c2,a1,b2],[c2,a1,b2,c1],[d2,b1,a2,c1],[a1,a2,c1]}
{}
cover(FindPartitionForDA3Coobservability)
"K is not C&P1-coobservable."
cover(VerifyCP1CoobservabilityEx3_4_1)
"K is C&P3-coobservable."
cover(VerifyCP3CoobservabilityEx3_4_1)
"K is D&A2-coobservable."
cover(VerifyDA2Coobservability1)
"K is not D&A3-coobservable with the same partition for verifying CP3CoobservabilityEx3_4_1."
cover(VerifyDA3CoobservabilityEx3_4_1)
"K is not C&P2-coobservable with the same partition for verifying DA2Coobservability1."
cover(VerifyCP2Coobservability1)
"K is not D&A-3coobservable."
cover(VerifyDA3CoobservabilityEx3_5_1)
"K is not C&P2-coobservable."
cover(VerifyCP2Coobservability2)
"K is C&P3-coobservable."
cover(VerifyCP3CoobservabilityEx3_5_1)
"K is D&A2-coobservable."
cover(VerifyDA2Coobservability2)


OUTPUT for Example 3.5.1, Fig 3.4 (test_section7_3_CASE2008_Ex3_4_1__3_5_1):
"K is C&P2-coobservable."
{{(1|->b1),(2|->b2)},[d2,b1,a2],[d2,b1,b2],[b1,b2,c1]}
{{(1|->a1),(2|->a2)},{(1|->a1),(2|->b2)},[c2,a1,b2],[c2,a1,b2,c1],[d2,b1,a2,c1],[a1,a2,c1]}
cover(FindPartitionForCP2Coobservability)
"K is C&P3-coobservable."
{{(1|->b1),(2|->b2)},[d2,b1,a2],[d2,b1,b2],[b1,b2,c1]}
{{(1|->a1),(2|->a2)},{(1|->a1),(2|->b2)},[c2,a1,b2],[c2,a1,b2,c1],[d2,b1,a2,c1],[a1,a2,c1]}
{}
cover(FindPartitionForCP3Coobservability)
"K is D&A3-coobservable."
{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}
{[c2,a1,a2]}
{{(1|->b1),(2|->a2)}}
cover(FindPartitionForDA3Coobservability)
"K is not C&P1-coobservable."
cover(VerifyCP1CoobservabilityEx3_4_1)
"K is not C&P3-coobservable."
cover(VerifyCP3CoobservabilityEx3_4_1)
"K is not D&A2-coobservable."
cover(VerifyDA2Coobservability1)
"K is D&A3-coobservable for ex. 3.5.1."
cover(VerifyDA3CoobservabilityEx3_4_1)
"K is C&P2-coobservable."
cover(VerifyCP2Coobservability1)
"K is D&A3-coobservable."
cover(VerifyDA3CoobservabilityEx3_5_1)
"K is C&P2-coobservable."
cover(VerifyCP2Coobservability2)
"K is not C&P3-coobservable with the same partition for verifying DA3CoobservabilityEx3_5_1."
cover(VerifyCP3CoobservabilityEx3_5_1)
"K is not D&A2-coobservable with the same partition for verifying CP2Coobservability."
cover(VerifyDA2Coobservability2)
*/