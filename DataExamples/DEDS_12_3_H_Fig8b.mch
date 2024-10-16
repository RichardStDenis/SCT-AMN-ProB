// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL DEDS_12_3_H_Fig8b

SETS 
 States = {s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11};
 Events = {alpha, beta, gamma, delta}

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
 // Proposition 4 with H of Fig. 8b.
 //---------------------------------------------------------------------
 bound = 4
 &
 G_fb = rec(       // Automaton modeling the uncontrolled behavior
            states: States,
            events: Events,
            tf: {(s1,gamma,s2), (s1,beta,s3), (s1,alpha,s4),
                 (s3,gamma,s5), (s4,gamma,s6),
                 (s5,delta,s7), (s5,beta,s8), (s5,alpha,s9),
                 (s8,delta,s10), (s9,delta,s11) },
            x_0: s1,
            Xm: States,
            ctrlEvents: {gamma, delta},
            obsEvents: {alpha, beta, gamma, delta} )
 &
 H_lb = rec(       // Automaton modeling the desired behavior
            states: {s1, s3, s4, s5, s6, s7, s8, s9},
            events: Events,
            tf: {(s1,beta,s3), (s1,alpha,s4),
                 (s3,gamma,s5), (s4,gamma,s6),
                 (s5,delta,s7), (s5,beta,s8), (s5,alpha,s9) },
            x_0: s1,
            Xm: {s1, s3, s4, s5, s6, s7, s8, s9} )
 &
 A_d = rec(        // Decentralized architecture
           CtrlEvents: G_fb'ctrlEvents,
           Events_ce: {delta},
           Events_cd: {gamma},
           NoSupervisors: 2,
           Events_c: {(1,gamma), (1,delta), (2,gamma), (2,delta)},
           Events_o: {(1,alpha), (1,gamma), (1,delta),
                      (2,beta), (2,gamma), (2,delta) },
           Msks_o: {(1,(alpha,alpha)), (1,(gamma,gamma)), (1,(delta,delta)),
                    (2,(beta,beta)), (2,(gamma,gamma)), (2,(delta,delta)) } )
 &
 A_d'NoSupervisors : NAT1
 &
 !i_.(i_ : 1..A_d'NoSupervisors => A_d'Events_c[{i_}] /= {})
 &
 !i_.(i_ : 1..A_d'NoSupervisors => A_d'Events_o[{i_}] /= {})
 &
 G_fb'ctrlEvents = UNION (i_).(i_ : 1..A_d'NoSupervisors | A_d'Events_c[{i_}])
 &
 G_fb'obsEvents = UNION (i_).(i_ : 1..A_d'NoSupervisors | A_d'Events_o[{i_}])
 &
 G_fb'obsEvents = UNION (i_).(i_ : 1..A_d'NoSupervisors | dom(A_d'Msks_o[{i_}])) END

/*
OUTPUT for the first case (test_section7_1_DEDS_12_3_H_Figs4b_6_8b):
"K is not C&P coobservable _."
cover(VerifyCPCoobservability_)
"K is not C&P coobservable."
cover(VerifyCPCoobservability)
"K is not D&A coobservable. _"
cover(VerifyDACoobservability_)
"K is not D&A coobservable."
cover(VerifyDACoobservability)

----------------------------------------

OUTPUT for the second case (test_section7_1_DEDS_12_3_H_Figs8b_11b):
"K is observable."
cover(VerifyObservability)
"K is C&P coobservable _."
cover(VerifyGCPCoobservability_)
"K is C&P coobservable."
cover(VerifyGCPCoobservability)
"K is D&A coobservable _."
cover(VerifyGDACoobservability_)
"K is D&A coobservable."
cover(VerifyGDACoobservability)
"K is coobservable."
Local decisions for all supervisors: 
 {(((1|->{})|->alpha)|->"unc"),(((1|->{})|->beta)|->"unc"),
  (((1|->{(1|->alpha)})|->gamma)|->"or"),
  (((1|->{(1|->gamma)})|->alpha)|->"unc"),(((1|->{(1|->gamma)})|->beta)|->"unc"),
  (((1|->{(1|->gamma)})|->delta)|->"and"),
  (((2|->{})|->alpha)|->"unc"),(((2|->{})|->beta)|->"unc"),
  (((2|->{(1|->beta)})|->gamma)|->"or"),
  (((2|->{(1|->beta),(2|->gamma)})|->alpha)|->"unc"),
  (((2|->{(1|->beta),(2|->gamma)})|->beta)|->"unc"),
  (((2|->{(1|->beta),(2|->gamma)})|->delta)|->"and")}
Global decisions: 
 {({}|->alpha),({}|->beta),
  ({(1|->alpha)}|->gamma),({(1|->beta)}|->gamma),
  ({(1|->beta),(2|->gamma)}|->alpha),({(1|->beta),(2|->gamma)}|->beta),
  ({(1|->beta),(2|->gamma)}|->delta)}
cover(VerifyCoobservability)
cover(cntrexGCPCoobservability)
cover(cntrexGDACoobservability)

----------------------------------------

OUTPUT for the third case (section7_1_DEDS_12_3_H_Figs8b_11bPartition):
"K is coobservable."
{delta}
{gamma}
cover(FindPartitionCoobservability)
"K is Lm(G)-closed."
"K is controllable."
"K is coobservable."
{delta}
{gamma}
Local decisions for all supervisors: 
 {(((1|->{})|->alpha)|->"unc"),(((1|->{})|->beta)|->"unc"),
  (((1|->{(1|->alpha)})|->gamma)|->"or"),
  (((1|->{(1|->gamma)})|->alpha)|->"unc"),(((1|->{(1|->gamma)})|->beta)|->"unc"),
  (((1|->{(1|->gamma)})|->delta)|->"and"),
  (((2|->{})|->alpha)|->"unc"),(((2|->{})|->beta)|->"unc"),
  (((2|->{(1|->beta)})|->gamma)|->"or"),
  (((2|->{(1|->beta),(2|->gamma)})|->alpha)|->"unc"),
  (((2|->{(1|->beta),(2|->gamma)})|->beta)|->"unc"),
  (((2|->{(1|->beta),(2|->gamma)})|->delta)|->"and")}
Global decisions: 
 {({}|->alpha),({}|->beta),
  ({(1|->alpha)}|->gamma),({(1|->beta)}|->gamma),
  ({(1|->beta),(2|->gamma)}|->alpha),({(1|->beta),(2|->gamma)}|->beta),
  ({(1|->beta),(2|->gamma)}|->delta)}
cover(FindPartitionCoobservability_)

----------------------------------------

OUTPUT for the fourth case (section7_2_IEEE_52_10_Ex1_2_H_Fig1_2):
"K is Lm(G)-closed."
cover(VerifyLmClosure)
"K is controllable."
cover(VerifyCtrl)
rec(D:({0} * {{}}),E:({0} * {{(1|->alpha)},{(1|->beta)}}))
0
{}
{}
rec(D:{(0|->[beta,gamma,alpha]),(0|->[beta,gamma,beta])},E:{(0|->{(1|->beta),(2|->gamma)})})
0
{}
{}
K is 0-inference observable
cover(VerifyN_InferenceObservability)
rec(D:({0} * {{}}),E:({0} * {{(1|->alpha)},{(1|->beta)}}))
rec(D:{(0|->[beta,gamma,alpha]),(0|->[beta,gamma,beta])},
    E:{(0|->{(1|->beta),(2|->gamma)})})
{(gamma|->rec(D:{(0|->{})},
              E:{(0|->{(1|->alpha)}),(0|->{(1|->beta)})})),
 (delta|->rec(D:{(0|->[beta,gamma,alpha]),(0|->[beta,gamma,beta])},
              E:{(0|->{(1|->beta),(2|->gamma)})}))}
K is 0-inference observable
cover(VerifyN_InferenceObservability_)
"Dk and Ek for:"
gamma
rec(D:({0} * {{}}),E:({0} * {{(1|->alpha)},{(1|->beta)}}))
{}
1
{}
| 0  | 1  | 0  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for {} and gamma = (0, disabled)
{(1|->alpha)}
1
{(1|->alpha)}
| 1  | 0  | 1  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for {(1|->alpha)} and gamma = (0, enabled)
{(1|->alpha),(2|->gamma)}
1
{(1|->alpha),(2|->gamma)}
| 0  | 0  | -1  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for {(1|->alpha),(2|->gamma)} and gamma = (0, unsure)
{(1|->beta)}
1
{(1|->beta)}
| 1  | 0  | 1  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for {(1|->beta)} and gamma = (0, enabled)
{(1|->beta),(2|->gamma)}
1
{(1|->beta),(2|->gamma)}
| 0  | 0  | -1  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for {(1|->beta),(2|->gamma)} and gamma = (0, unsure)
{(1|->gamma)}
1
{(1|->gamma)}
| 0  | 0  | -1  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for {(1|->gamma)} and gamma = (0, unsure)
[beta,gamma,alpha]
1
[beta,gamma,alpha]
| 0  | 0  | -1  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for [beta,gamma,alpha] and gamma = (0, unsure)
[beta,gamma,beta]
1
[beta,gamma,beta]
| 0  | 0  | -1  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for [beta,gamma,beta] and gamma = (0, unsure)
[beta,gamma,delta]
1
[beta,gamma,delta]
| 0  | 0  | -1  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for [beta,gamma,delta] and gamma = (0, unsure)
[beta,gamma,alpha,delta]
1
[beta,gamma,alpha,delta]
| 0  | 0  | -1  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for [beta,gamma,alpha,delta] and gamma = (0, unsure)
[beta,gamma,beta,delta]
1
[beta,gamma,beta,delta]
| 0  | 0  | -1  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for [beta,gamma,beta,delta] and gamma = (0, unsure)
"Dk and Ek for:"
delta
rec(D:{(0|->[beta,gamma,alpha]),(0|->[beta,gamma,beta])},E:{(0|->{(1|->beta),(2|->gamma)})})
{}
1
{}
| 0  | 0  | -1  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for {} and delta = (0, unsure)
{(1|->alpha)}
1
{(1|->alpha)}
| 0  | 0  | -1  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for {(1|->alpha)} and delta = (0, unsure)
{(1|->alpha),(2|->gamma)}
1
{(1|->alpha),(2|->gamma)}
| 0  | 0  | -1  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for {(1|->alpha),(2|->gamma)} and delta = (0, unsure)
{(1|->beta)}
1
{(1|->beta)}
| 0  | 0  | -1  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for {(1|->beta)} and delta = (0, unsure)
{(1|->beta),(2|->gamma)}
1
{(1|->beta),(2|->gamma)}
| 1  | 0  | 1  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for {(1|->beta),(2|->gamma)} and delta = (0, enabled)
{(1|->gamma)}
1
{(1|->gamma)}
| 0  | 0  | -1  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for {(1|->gamma)} and delta = (0, unsure)
[beta,gamma,alpha]
1
[beta,gamma,alpha]
| 0  | 1  | 0  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for [beta,gamma,alpha] and delta = (0, disabled)
[beta,gamma,beta]
1
[beta,gamma,beta]
| 0  | 1  | 0  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for [beta,gamma,beta] and delta = (0, disabled)
[beta,gamma,delta]
1
[beta,gamma,delta]
| 0  | 0  | -1  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for [beta,gamma,delta] and delta = (0, unsure)
[beta,gamma,alpha,delta]
1
[beta,gamma,alpha,delta]
| 0  | 0  | -1  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for [beta,gamma,alpha,delta] and delta = (0, unsure)
[beta,gamma,beta,delta]
1
[beta,gamma,beta,delta]
| 0  | 0  | -1  | 0 |
2
{}
| 1  | 1  | -1  | 1 |
global decision for [beta,gamma,beta,delta] and delta = (0, unsure)
cover(GetDecentralizedSupervisor)
*/