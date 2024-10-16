// Author: Richard St-Denis, Universite de Sherbrooke, 2024.

MODEL SCP_94_2_Fig13

SETS
 States = {x0,x1, x2, x3, x4, x5, x6, x7, x8, x9};
 Events = {e0, e1, e2, e3, e4, e5, e6, e7, e8, e9}

CONSTANTS
 G_fig13,     // Automata for the uncontrolled behavior
 Q_fig13      // Predicate representing the specification

PROPERTIES
 //---------------------------------------------------------------------
 // A simple example for checking co-observability properties.
 //   B. Fraikin, M. Frappier and R. St-Denis,
 //     Supervisory Control Theory with Alloy,
 //     Science of Computer Programming,
 //     94 (2), 217-237, 2014.
 // Fig. 13.
 //---------------------------------------------------------------------

 G_fig13 = rec(states: States,    // Automaton modeling the uncontrolled behavior
               events: Events,
               tf: {(x0, e1, x1), (x0, e2, x2), (x0, e6, x6), (x1, e0, x0),
                    (x2, e3, x3), (x3, e4, x4), (x4, e5, x5),
                    (x6, e7, x7), (x8, e9, x9), (x9, e8, x8) },
               x_0: x0,
               Xm: {x1, x4, x6, x9},
               ctrlEvents: {e2, e4, e7, e8, e9} )
 &
 Q_fig13 = States - {x5, x7} END

/*
OUTPUT:
{x0,x1,x6,x8,x9}
{x0,x1,x6}
cover(DisplaySup)
{x0,x1,x6}
cover(ComputeSupCCP)
{(x0|->{e2}),(x1|->{}),(x2|->{}),(x3|->{e4}),(x4|->{}),(x5|->{}),(x6|->{e7}),(x7|->{}),(x8|->{e9}),(x9|->{e8})}
{(x0|->{e2}),(x1|->{}),(x6|->{e7})}
{(x0|->{e2}),(x1|->{}),(x6|->{e7})}
{(x0|->{e0,e1,e3,e4,e5,e6,e7,e8,e9}),(x1|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),(x2|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),
 (x3|->{e0,e1,e2,e3,e5,e6,e7,e8,e9}),(x4|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),(x5|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),
 (x6|->{e0,e1,e2,e3,e4,e5,e6,e8,e9}),(x7|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),(x8|->{e0,e1,e2,e3,e4,e5,e6,e7,e8}),
 (x9|->{e0,e1,e2,e3,e4,e5,e6,e7,e9})}
{(x0|->{e0,e1,e3,e4,e5,e6,e7,e8,e9}),(x1|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),(x6|->{e0,e1,e2,e3,e4,e5,e6,e8,e9})}
{(x0|->{e1,e6}),(x1|->{e0}),(x6|->{})}
"e1 from x0"
"not e2 from x0"
"e6 from x0"
{((x0|->e1)|->x1),((x0|->e6)|->x6),((x1|->e0)|->x0)}
"The closed-loop is reachable."
"The closed-loop is coreachable."
"The closed-loop is nonblocking."
cover(Display_SFBC_d)
{e0,e1,e3,e5,e6}
cover(Display_unctrlEvents)
{(x0|->x1),(x0|->x2),(x0|->x6),(x1|->x0),(x2|->x3),(x3|->x4),(x8|->x9),(x9|->x8)}
cover(Display_unlblR_1)
{(x0|->x1),(x0|->x2),(x0|->x6),(x1|->x0),(x2|->x3),(x3|->x4),(x8|->x9),(x9|->x8)}
cover(Display_unlblTR_Q)
{(x0|->x1),(x0|->x6),(x1|->x0),(x2|->x3),(x4|->x5)}
cover(Display_unlblR_2)
{(x0|->x1),(x0|->x6),(x1|->x0),(x2|->x3),(x4|->x5)}
cover(Display_unlblTR_E)
"Unsatisfied."
cover(Verify_wp_false_1)
"Unsatisfied."
cover(Verify_wp_false_2)
"Satisfied."
cover(Verify_wp_true)
"Unsatisfied."
cover(Verify_wlp_false)
"Satisfied."
cover(Verify_wlp_true1)
"Satisfied."
cover(Verify_wlp_true2)
{x0,x1,x2,x3,x6,x8,x9}
cover(DisplayCtrlField)
{x0,x1,x2,x3,x4,x6,x8,x9}
cover(Display_DualCtrlField)
{x0,x1,x2,x3,x4,x6,x8,x9}
cover(Display_DualCtrlField_)
{x4,x5,x7}
cover(Display_DualCtrlFieldNotQ)
{x0,x1,x2,x3,x6,x8,x9}
cover(Display_NotDualCtrlFieldNotQ)
"Satisfied."
cover(Verify_Idempotent)
"Satisfied."
cover(Verify_Idempotent_Dual)
"Satisfied."
cover(Verify_Dual)
{x0,x1,x2,x3,x4,x6}
cover(Display_R)
{x0,x1,x2,x3,x4,x6}
cover(Display_RorCR_1)
{x0,x1,x2,x3,x4,x6,x8,x9}
cover(Display_CR)
{x0,x1,x2,x3,x4,x6,x8,x9}
cover(Display_RorCR_2)
"Q is not reachable."
cover(Verify_Reachable)
"Q is coreachable."
cover(Verify_Coreachable)
"Q is nonblocking."
cover(Verify_Nonblocking_true)
"{x0} is blocking."
cover(Verify_Nonblocking_false)
"Q is not weakly controllable."
cover(Verify_WeaklyControllable_1)
"[Q] is weakly controllable."
cover(Verify_WeaklyControllabl_2)
"{} is weakly controllable."
cover(Verify_WeaklyControllabl_3)
"[Q] is weakly controllable."
cover(Verify_WeaklyControllable_)
"[Q] is weakly controllable."
cover(Verify_WeaklyControllable__)
"Q is not controllable."
cover(Verify_Controllable_1)
"[Q] is not controllable."
cover(Verify_Controllable_2)
"The property is satisfied."
cover(Verify_Property_1)
"The property is satisfied."
cover(Verify_Property_2)
{x0,x1,x6,x8,x9}
cover(SearchSupWCCP)
{x0,x1,x6}
cover(SearchSupCCP)
*/