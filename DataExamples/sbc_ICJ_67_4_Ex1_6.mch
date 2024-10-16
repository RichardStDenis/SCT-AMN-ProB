// copyright Richard St-Denis, Universite de Sherbrooke, 2022.
MODEL sbc_ICJ_67_4_Ex1_6

SETS
 States = {x0, x1, x2, x3, x4};
 ObservationSpace = {y};
 Events = {s1, s2}

CONSTANTS
 G1, G2,              // Automaton for the uncontrolled behavior
 mask,                // Observation mask
 Q0, Q1, Q2, Q3, Q4   // Predicate representing the specification to be fulfilled



PROPERTIES
//---------------------------------------------------------------------
// Examples 1 to 6 from:
//   S Takai and S. Kodama,
//     M-controllable subpredicates arising in state feedback control of discrete
//     event systems,
//     International Journal of Control, 67 (2): 553-566, 1997.
// Figure 1.
//---------------------------------------------------------------------
 G1 = rec(              // Automaton modeling the free behavior (Fig. 1)
          states: States,
          events: Events,
          tf: {(x0, s1, x2), (x0, s2, x1),
               (x1, s1, x3), (x2, s2, x3),
               (x3, s1, x4), (x3, s2, x4) },
          x_0: x0,
          Xm: {x0, x1, x2, x4},
          ctrlEvents: Events )
 &
 G2 = rec(              // Automaton modeling the free behavior (Fig. 2)
          states: States,
          events: Events,
          tf: {(x0, s1, x1), (x0, s2, x2),
               (x1, s2, x3), (x2, s2, x3),
               (x3, s1, x4) },
          x_0: x0,
          Xm: {x0, x1, x2, x4},
          ctrlEvents: {s2} )
 &
 mask = {(x0, y), (x1, y), (x2, y), (x3, y), (x4, y)}
 &
 Q0 = {x0, x1, x2}
 &
 Q1 = {x0, x1}
 &
 Q2 = {x0, x2}
 &
 Q3 = Q1
 &
 Q4 = {x0, x1, x2, x3} END

/*
OUTPUT:
{y}
cover(Use_M_)
{x0,x1,x2,x3,x4}
cover(USE_M_1M)
{{x0,x1,x2,x3,x4}}
cover(USE_ObservabilityClasses)
"Satisfied."
cover(Verify_wp_true)
"Satisfied."
cover(Verify_wp_false)
"Satisfied."
cover(Verify_wlp_true)
"Satisfied."
cover(Verify_wlp_false)
{x0,x1,x2}
cover(USE_CtrlField)
"Idempotent verified"
cover(Test_Idempotent)
"Q0 is weakly controllable."
cover(Verify_WeaklyControllable)
{s1,s2}
cover(USE_A_hat_0)
{s1,s2}
cover(USE_A_hat_1)
{s1,s2}
cover(USE_A_hat_2)
{s1,s2}
cover(USE_A_hat_3)
{s1,s2}
cover(USE_A_hat_4)
{s1,s2}
cover(USE_A_breve_0)
{s1}
cover(USE_A_breve_1)
{s2}
cover(USE_A_breve_2)
{s1}
cover(USE_A_breve_3)
{s1,s2}
cover(USE_A_breve_4)
"Definitions are consistent."
cover(Verify_inclusion_1)
"Definitions are consistent."
cover(Verify_inclusion_2)
{x0}
cover(USE_R_hat_0)
{x0}
cover(USE_R_hat_1)
{x0}
cover(USE_R_hat_2)
{x0}
cover(USE_R_hat_4)
{x0}
cover(USE_R_breve_0)
{x0,x1}
cover(USE_R_breve_1)
{x0,x2}
cover(USE_R_breve_2)
{x0}
cover(USE_R_breve_4)
"Definitions are consistent."
cover(Verify_inclusion_3)
"Definitions are consistent."
cover(Verify_inclusion_4)
"Condition 1 is satisfied."
cover(Verify_C1)
"Condition 2 is satisfied."
cover(Verify_C2)
"The assertion is satisfied."
cover(Verify_assertion)
"Q0 is not strong M-controllable."
cover(Verify_StrongMcontrollable)
"{x0,x1} is not strong M-controllable."
cover(Verify_StrongMcontrollable_1)
"{x0,x2} is not strong M-controllable."
cover(Verify_StrongMcontrollable_2)
"{x0,x1,x2,x3} is not strong M-controllable."
cover(Verify_StrongMcontrollable_4)
"{x0,x1,x2} is not strong M-controllable."
cover(Verify_StrongMcontrollable_2_0)
"{x0,x1} is strong M-controllable."
cover(Verify_StrongMcontrollable_2_1)
{s2}
cover(USE_A_hat6)
"Q0 is not M-controllable."
cover(Verify_Mcontrollable)
"{x0,x1} is M-controllable."
cover(Verify_Mcontrollable_1)
"{x0,x2} is M-controllable."
cover(Verify_Mcontrollable_2)
{x0}
cover(USE_Approx)
{x0}
{(x0|->{s1,s2})}
{(x0|->{s1,s2})}
{(x0|->{})}
{(x0|->{})}
{(x0|->{}),(x1|->{}),(x2|->{}),(x3|->{}),(x4|->{})}
cover(USE_GetSupSMCP_1)
{x0,x1}
{(x0|->{s2}),(x1|->{s2})}
{(x0|->{s1}),(x1|->{})}
{(x0|->{s1}),(x1|->{}),(x2|->{}),(x3|->{s1}),(x4|->{})}
cover(USE_GETSupSMCP_2)
{x0,x1}
cover(ComputesupSMCP)
{x0}
{(x0|->{s1,s2})}
{(x0|->{s1,s2})}
{(x0|->{})}
{(x0|->{})}
{(x0|->{}),(x1|->{}),(x2|->{}),(x3|->{}),(x4|->{})}
cover(USE_GetmaxMCP_0)
{x0,x1}
{(x0|->{s1}),(x1|->{s1})}
{(x0|->{s1}),(x1|->{s1})}
{(x0|->{s2}),(x1|->{})}
{(x0|->{s2}),(x1|->{s2})}
{(x0|->{s2}),(x1|->{}),(x2|->{s2}),(x3|->{s2}),(x4|->{})}
cover(USE_GetmaxMCP_1)
{x0,x2}
{(x0|->{s2}),(x2|->{s2})}
{(x0|->{s2}),(x2|->{s2})}
{(x0|->{s1}),(x2|->{})}
{(x0|->{s1}),(x2|->{s1})}
{(x0|->{s1}),(x1|->{s1}),(x2|->{}),(x3|->{s1}),(x4|->{})}
cover(USE_GetmaxMCP_2)
{{},{x0},{x0,x1},{x0,x2},{x0,x1,x2}}
cover(USE_C)
{{},{x0}}
cover(USE_SMC)
{{},{x0},{x0,x1},{x0,x2}}
cover(USE_MC)

ALL OPERATIONS COVERED
*/