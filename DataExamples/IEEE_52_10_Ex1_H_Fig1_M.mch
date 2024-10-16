// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL IEEE_52_10_Ex1_H_Fig1_M

DEFINITIONS
 SET_PREF_MININT == 0;
 SET_PREF_MAXINT == 5

SETS
 States = {s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10,
           s11, s12, s13, s14, s15, s16, s17, s18, s19 };
 Events = {a, b, c, d, ap, bp};
 LocObsSyms = {u, v, w, x, y, z}

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
// Example 1 with H of Fig. 1 with another observation mask.
//---------------------------------------------------------------------
 bound = 4
 &
 G_fb = rec(       // Automaton modeling the uncontrolled behavior
            states: States,
            events: Events,
            tf: {(s0,a,s3), (s0,b,s1), (s0,c,s2), (s0,d,s4),
                 (s1,c,s6), (s1,ap,s5),
                 (s3,c,s7), (s3,bp,s8),
                 (s4,a,s11), (s4,b,s9), (s4,c,s10),
                 (s5,c,s12),
                 (s8,c,s13),
                 (s9,c,s15), (s9,ap,s14),
                 (s11,c,s16), (s11,bp,s17),
                 (s14,c,s18),
                 (s17,c,s19) },
            x_0: s0,
            Xm: {s2, s5, s6, s7, s8, s10, s12, s13, s14, s15,
                 s16, s17, s18, s19 },
            ctrlEvents: {c},
            obsEvents: {a, b, d, ap, bp} )
 &
 H_lb = rec(       // Automaton modeling the desired behavior
            states: {s0, s1, s2, s3, s4, s5, s8, s9,
                     s11, s12, s13, s14, s15, s16, s17 },
            events: Events,
            tf: {(s0,a,s3), (s0,b,s1), (s0,c,s2), (s0,d,s4),
                 (s1,ap,s5),
                 (s3,bp,s8),
                 (s4,a,s11), (s4,b,s9),
                 (s5,c,s12),
                 (s8,c,s13),
                 (s9,c,s15), (s9,ap,s14),
                 (s11,c,s16), (s11,bp,s17) },
            x_0: s0,
            Xm: {s2, s5, s8, s12, s13, s14, s15, s16, s17} )
 &
 A_d = rec(        // Decentralized architecture
           NoSupervisors: 2,
           Events_c:  {(1,c), (2,c)},
           Msks_o: {(1,(a,u)), (1,(ap,v)), (1,(d,w)),
                    (2,(b,x)), (2,(bp,y)), (2,(d,z)) } )
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
OUTPUT (section7_2_IEEE_52_10_Ex1_2_H_Fig1_2):
"K is Lm(G)-closed."
cover(VerifyLmClosure)
"K is controllable."
cover(VerifyCtrl)
rec(D:{(0|->{(1|->a)}),(0|->{(1|->b)}),(0|->{(1|->d)}),(0|->[d,a,bp]),(0|->[d,b,ap]),
       (1|->{(1|->a)}),(1|->{(1|->b)}),(1|->{(1|->d)}),
       (2|->{(1|->d)})},
    E:{(0|->{}),(0|->{(1|->a),(2|->bp)}),(0|->{(1|->b),(2|->ap)}),(0|->{(1|->d),(2|->a)}),(0|->{(1|->d),(2|->b)}),
       (1|->{}),(1|->{(1|->d),(2|->a)}),(1|->{(1|->d),(2|->b)}),
       (2|->{})})
2
{}
{}
K is 2-inference observable
cover(VerifyN_InferenceObservability)
rec(D:{(0|->{(1|->a)}),(0|->{(1|->b)}),(0|->{(1|->d)}),(0|->[d,a,bp]),(0|->[d,b,ap]),
       (1|->{(1|->a)}),(1|->{(1|->b)}),(1|->{(1|->d)}),
       (2|->{(1|->d)})},
    E:{(0|->{}),(0|->{(1|->a),(2|->bp)}),(0|->{(1|->b),(2|->ap)}),(0|->{(1|->d),(2|->a)}),(0|->{(1|->d),(2|->b)}),
       (1|->{}),(1|->{(1|->d),(2|->a)}),(1|->{(1|->d),(2|->b)}),
       (2|->{})})
{(c|->rec(D:{(0|->{(1|->a)}),(0|->{(1|->b)}),(0|->{(1|->d)}),(0|->[d,a,bp]),(0|->[d,b,ap]),
             (1|->{(1|->a)}),(1|->{(1|->b)}),(1|->{(1|->d)}),
             (2|->{(1|->d)})},
          E:{(0|->{}),(0|->{(1|->a),(2|->bp)}),(0|->{(1|->b),(2|->ap)}),(0|->{(1|->d),(2|->a)}),(0|->{(1|->d),(2|->b)}),
             (1|->{}),(1|->{(1|->d),(2|->a)}),(1|->{(1|->d),(2|->b)}),
             (2|->{})}))}
K is 2-inference observable
cover(VerifyN_InferenceObservability_)
"Dk and Ek for:"
c
rec(D:{(0|->{(1|->a)}),(0|->{(1|->b)}),(0|->{(1|->d)}),(0|->[d,a,bp]),(0|->[d,b,ap]),
       (1|->{(1|->a)}),(1|->{(1|->b)}),(1|->{(1|->d)}),
       (2|->{(1|->d)})},
    E:{(0|->{}),(0|->{(1|->a),(2|->bp)}),(0|->{(1|->b),(2|->ap)}),(0|->{(1|->d),(2|->a)}),(0|->{(1|->d),(2|->b)}),
       (1|->{}),(1|->{(1|->d),(2|->a)}),(1|->{(1|->d),(2|->b)}),
       (2|->{})})
{}
1
{}
| 3  | 2  | 1  | 2 |
2
{}
| 3  | 2  | 1  | 2 |
global decision for {} and c = (2, enabled)
{(1|->a)}
1
{(1|->u)}
| 1  | 2  | 0  | 1 |
2
{}
| 3  | 2  | 1  | 2 |
global decision for {(1|->a)} and c = (1, disabled)
{(1|->a),(2|->c)}
1
{(1|->u)}
| 1  | 2  | 0  | 1 |
2
{}
| 3  | 2  | 1  | 2 |
global decision for {(1|->a),(2|->c)} and c = (1, disabled)
{(1|->a),(2|->bp)}
1
{(1|->u)}
| 1  | 2  | 0  | 1 |
2
{(1|->y)}
| 1  | 0  | 1  | 0 |
global decision for {(1|->a),(2|->bp)} and c = (0, enabled)
{(1|->b)}
1
{}
| 3  | 2  | 1  | 2 |
2
{(1|->x)}
| 1  | 2  | 0  | 1 |
global decision for {(1|->b)} and c = (1, disabled)
{(1|->b),(2|->c)}
1
{}
| 3  | 2  | 1  | 2 |
2
{(1|->x)}
| 1  | 2  | 0  | 1 |
global decision for {(1|->b),(2|->c)} and c = (1, disabled)
{(1|->b),(2|->ap)}
1
{(1|->v)}
| 1  | 0  | 1  | 0 |
2
{(1|->x)}
| 1  | 2  | 0  | 1 |
global decision for {(1|->b),(2|->ap)} and c = (0, enabled)
{(1|->c)}
1
{}
| 3  | 2  | 1  | 2 |
2
{}
| 3  | 2  | 1  | 2 |
global decision for {(1|->c)} and c = (2, enabled)
{(1|->d)}
1
{(1|->w)}
| 2  | 3  | 0  | 2 |
2
{(1|->z)}
| 2  | 3  | 0  | 2 |
global decision for {(1|->d)} and c = (2, disabled)
{(1|->d),(2|->a)}
1
{(1|->w),(2|->u)}
| 2  | 1  | 1  | 1 |
2
{(1|->z)}
| 2  | 3  | 0  | 2 |
global decision for {(1|->d),(2|->a)} and c = (1, enabled)
{(1|->d),(2|->b)}
1
{(1|->w)}
| 2  | 3  | 0  | 2 |
2
{(1|->z),(2|->x)}
| 2  | 1  | 1  | 1 |
global decision for {(1|->d),(2|->b)} and c = (1, enabled)
{(1|->d),(2|->c)}
1
{(1|->w)}
| 2  | 3  | 0  | 2 |
2
{(1|->z)}
| 2  | 3  | 0  | 2 |
global decision for {(1|->d),(2|->c)} and c = (2, disabled)
[d,a,c]
1
{(1|->w),(2|->u)}
| 2  | 1  | 1  | 1 |
2
{(1|->z)}
| 2  | 3  | 0  | 2 |
global decision for [d,a,c] and c = (1, enabled)
[d,a,bp]
1
{(1|->w),(2|->u)}
| 2  | 1  | 1  | 1 |
2
{(1|->z),(2|->y)}
| 0  | 1  | 0  | 0 |
global decision for [d,a,bp] and c = (0, disabled)
[d,a,bp,c]
1
{(1|->w),(2|->u)}
| 2  | 1  | 1  | 1 |
2
{(1|->z),(2|->y)}
| 0  | 1  | 0  | 0 |
global decision for [d,a,bp,c] and c = (0, disabled)
[d,b,c]
1
{(1|->w)}
| 2  | 3  | 0  | 2 |
2
{(1|->z),(2|->x)}
| 2  | 1  | 1  | 1 |
global decision for [d,b,c] and c = (1, enabled)
[d,b,ap]
1
{(1|->w),(2|->v)}
| 0  | 1  | 0  | 0 |
2
{(1|->z),(2|->x)}
| 2  | 1  | 1  | 1 |
global decision for [d,b,ap] and c = (0, disabled)
[d,b,ap,c]
1
{(1|->w),(2|->v)}
| 0  | 1  | 0  | 0 |
2
{(1|->z),(2|->x)}
| 2  | 1  | 1  | 1 |
global decision for [d,b,ap,c] and c = (0, disabled)
[b,ap,c]
1
{(1|->v)}
| 1  | 0  | 1  | 0 |
2
{(1|->x)}
| 1  | 2  | 0  | 1 |
global decision for [b,ap,c] and c = (0, enabled)
[a,bp,c]
1
{(1|->u)}
| 1  | 2  | 0  | 1 |
2
{(1|->y)}
| 1  | 0  | 1  | 0 |
global decision for [a,bp,c] and c = (0, enabled)
cover(GetDecentralizedSupervisor)
*/