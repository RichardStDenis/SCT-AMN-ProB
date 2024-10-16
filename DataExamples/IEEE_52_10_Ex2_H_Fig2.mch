// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL IEEE_52_10_Ex2_H_Fig2

DEFINITIONS
 SET_PREF_MININT == 0;
 SET_PREF_MAXINT == 5

SETS
 States = {s0, s1, s2, s3, s4, s5, s6};
 Events = {a, b, c}

CONSTANTS
 A_d,              // N-inference decentralized architecture
 G_fb, H_lb,       // Automata for uncontrolled and desired behaviors
 bound             // Bound on the length of sequences

PROPERTIES
//---------------------------------------------------------------------
// A simple example for checking the N-inference observability property.
//   R. Kumar and S. Takai,
//     Inference-based ambiguity management in decentralized
//     decision-making: decentralized control of discrete event systems,
//     IEEE Transactions on Automatic Control,
//     52 (10), 1783-1794
// Example 2 with H of Fig. 2.
//---------------------------------------------------------------------
 bound = 3
 &
 G_fb = rec(       // Automaton modeling the uncontrolled behavior
            states: States,
            events: Events,
            tf: {(s0,a,s1), (s1,b,s2), (s2,c,s3),
                 (s0,b,s4), (s4,a,s5), (s5,c,s6) },
            x_0: s0,
            Xm: {s2, s3, s5, s6},
            ctrlEvents: {c},
                        // {a, b, c},                           // for the trivial test
            obsEvents: {a, b} )
 &
 H_lb = rec(       // Automaton modeling the desired behavior
            states: States - {s6},
                    // States,                                  // for the trivial test
            events: Events,
            tf: {(s0,a,s1), (s1,b,s2), (s2,c,s3),
                 (s0,b,s4), (s4,a,s5) }, // \/ {(s5,c,s6)},     // for the trivial test
            x_0: s0,
            Xm: {s2, s3, s5} )
                // {s2, s3, s5, s6} )                           // for the trivial test
 &
 A_d = rec(        // Decentralized architecture
           NoSupervisors: 2,
           Events_c: {(1,c), (2,c)},
                     // {(1,c), (2,c), (1,a), (2,b)},           // for the trivial test
           Msks_o: {(1,(a,a)),
                    (2,(b,b))} )
 &
 A_d'NoSupervisors : NAT1
 &
 !i_.(i_ : 1..A_d'NoSupervisors => A_d'Events_c[{i_}] /= {})
 &
 !i_.(i_ : 1..A_d'NoSupervisors => A_d'Msks_o[{i_}] /= {})
 &
 G_fb'ctrlEvents = UNION (i_).(i_ : 1..A_d'NoSupervisors | A_d'Events_c[{i_}])
 &
 G_fb'obsEvents = UNION (i_).(i_ : 1..A_d'NoSupervisors | dom(A_d'Msks_o[{i_}]))
END

/*
OUTPUT for the first case ((section7_2_IEEE_52_10_Ex1_2_H_Fig1_2):
"K is Lm(G)-closed."
cover(VerifyLmClosure)
"K is controllable."
cover(VerifyCtrl)
rec(D:{(0|->{(1|->b),(2|->a)}),
       (1|->{(1|->b),(2|->a)}),
       (2|->{(1|->b),(2|->a)}),
       (3|->{(1|->b),(2|->a)}),
       (4|->{(1|->b),(2|->a)})},
    E:{(0|->{(1|->a),(2|->b)}),
       (1|->{(1|->a),(2|->b)}),
       (2|->{(1|->a),(2|->b)}),
       (3|->{(1|->a),(2|->b)}),
       (4|->{(1|->a),(2|->b)})})
4
{{(1|->b),(2|->a)}}
{{(1|->a),(2|->b)}}
K is not 4-inference observable
cover(VerifyN_InferenceObservability)

----------------------------------------

OUTPUT for the second case (section7_2_IEEE_52_10_Ex1_2_H_Fig1_2):
"K is Lm(G)-closed."
cover(VerifyLmClosure)
"K is controllable."
cover(VerifyCtrl)
rec(D:({0} * {}),E:({0} * {{},{(1|->b)}}))
0
{}
{}
rec(D:({0} * {}),E:({0} * {{},{(1|->a)}}))
0
{}
{}
rec(D:{},E:{(0|->{(1|->a),(2|->b)}),(0|->{(1|->b),(2|->a)})})
0
{}
{}
K is 0-inference observable
cover(VerifyN_InferenceObservability)
rec(D:({0} * {}),E:({0} * {{},{(1|->b)}}))
rec(D:({0} * {}),E:({0} * {{},{(1|->a)}}))
rec(D:{},E:{(0|->{(1|->a),(2|->b)}),(0|->{(1|->b),(2|->a)})})
{(a|->rec(D:{},
          E:{(0|->{}),(0|->{(1|->b)})})),
 (b|->rec(D:{},
          E:{(0|->{}),(0|->{(1|->a)})})),
 (c|->rec(D:{},
          E:{(0|->{(1|->a),(2|->b)}),(0|->{(1|->b),(2|->a)})}))}
K is 0-inference observable
cover(VerifyN_InferenceObservability_)
"Dk and Ek for:"
a
rec(D:({0} * {}),E:({0} * {{},{(1|->b)}}))
{}
1
{}
| 1  | 0  | 1  | 0 |
global decision for {} and a = (0, enabled)
{(1|->a)}
1
{(1|->a)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->a)} and a = (0, unsure)
{(1|->a),(2|->b)}
1
{(1|->a)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->a),(2|->b)} and a = (0, unsure)
{(1|->b)}
1
{}
| 1  | 0  | 1  | 0 |
global decision for {(1|->b)} and a = (0, enabled)
{(1|->b),(2|->a)}
1
{(1|->a)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->b),(2|->a)} and a = (0, unsure)
[b,a,c]
1
{(1|->a)}
| 0  | 0  | -1  | 0 |
global decision for [b,a,c] and a = (0, unsure)
[a,b,c]
1
{(1|->a)}
| 0  | 0  | -1  | 0 |
global decision for [a,b,c] and a = (0, unsure)
"Dk and Ek for:"
b
rec(D:({0} * {}),E:({0} * {{},{(1|->a)}}))
{}
2
{}
| 1  | 0  | 1  | 0 |
global decision for {} and b = (0, enabled)
{(1|->a)}
2
{}
| 1  | 0  | 1  | 0 |
global decision for {(1|->a)} and b = (0, enabled)
{(1|->a),(2|->b)}
2
{(1|->b)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->a),(2|->b)} and b = (0, unsure)
{(1|->b)}
2
{(1|->b)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->b)} and b = (0, unsure)
{(1|->b),(2|->a)}
2
{(1|->b)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->b),(2|->a)} and b = (0, unsure)
[b,a,c]
2
{(1|->b)}
| 0  | 0  | -1  | 0 |
global decision for [b,a,c] and b = (0, unsure)
[a,b,c]
2
{(1|->b)}
| 0  | 0  | -1  | 0 |
global decision for [a,b,c] and b = (0, unsure)
"Dk and Ek for:"
c
rec(D:{},E:{(0|->{(1|->a),(2|->b)}),(0|->{(1|->b),(2|->a)})})
{}
1
{}
| 0  | 0  | -1  | 0 |
2
{}
| 0  | 0  | -1  | 0 |
global decision for {} and c = (0, unsure)
{(1|->a)}
1
{(1|->a)}
| 1  | 0  | 1  | 0 |
2
{}
| 0  | 0  | -1  | 0 |
global decision for {(1|->a)} and c = (0, unsure)
{(1|->a),(2|->b)}
1
{(1|->a)}
| 1  | 0  | 1  | 0 |
2
{(1|->b)}
| 1  | 0  | 1  | 0 |
global decision for {(1|->a),(2|->b)} and c = (0, enabled)
{(1|->b)}
1
{}
| 0  | 0  | -1  | 0 |
2
{(1|->b)}
| 1  | 0  | 1  | 0 |
global decision for {(1|->b)} and c = (0, unsure)
{(1|->b),(2|->a)}
1
{(1|->a)}
| 1  | 0  | 1  | 0 |
2
{(1|->b)}
| 1  | 0  | 1  | 0 |
global decision for {(1|->b),(2|->a)} and c = (0, enabled)
[b,a,c]
1
{(1|->a)}
| 1  | 0  | 1  | 0 |
2
{(1|->b)}
| 1  | 0  | 1  | 0 |
global decision for [b,a,c] and c = (0, enabled)
[a,b,c]
1
{(1|->a)}
| 1  | 0  | 1  | 0 |
2
{(1|->b)}
| 1  | 0  | 1  | 0 |
global decision for [a,b,c] and c = (0, enabled)
cover(GetDecentralizedSupervisor)
*/