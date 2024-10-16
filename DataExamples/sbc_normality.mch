// copyright Richard St-Denis, Universite de Sherbrooke, 2022.
MODEL sbc_normality

SETS
 States = {x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12};
 ObservationSpace = {y0, y1, y2, y3, y4, y5, y6, y7, y8};
 Events = {e1, e2, e3, e4, e5, e6, e7, e8, e9, e10}

CONSTANTS
 G0,     // Automaton for the uncontrolled behavior
 mask,   // Observation mask
 Q0      // Predicate representing the specification to be fulfilled

PROPERTIES
 G0 = rec(              // Automaton modeling the free behavior
         states: States,
         events: Events,
         tf: {(x0, e1, x1), (x0, e2, x2),
              (x0, e3, x3), (x0, e4, x4),
              (x0, e5, x5), (x0, e6, x6),
              (x0, e7, x7), (x7, e8, x8),
              (x1, e9, x9), (x2, e9, x10),
              (x3, e3, x11), (x4, e4, x12),
              (x5, e5, x11), (x6, e6, x12) },
         x_0: x0,
         Xm: States,
         ctrlEvents: {e2, e3, e4, e5, e6, e9, e10} )
 &
 mask = {(x0, y0), (x1, y1), (x2, y1), (x3, y2), (x4, y2), (x5, y3), (x6, y3),
         (x7, y4), (x8, y5), (x9, y6), (x10, y6), (x11, y7), (x12, y8) }
 &
 Q0 = {x0, x1, x2, x4, x7, x8, x9, x11} END

/*
OUTPUT:
{y0,y1,y2,y4,y5,y6,y7}
cover(Use_M_)
{x0,x1,x2,x3,x4,x7,x8,x9,x10,x11}
cover(USE_M_1M)
{{x0},{x1,x2},{x3,x4},{x5,x6},{x7},{x8},{x9,x10},{x11},{x12}}
cover(USE_ObservabilityClasses)
{x0,x1,x2,x4,x7,x8,x9,x11}
cover(USE_CtrlField)
"Idempotent verified"
cover(Test_Idempotent)
"Q0 is weakly controllable."
cover(Verify_WeaklyControllable)
"Q0 is not controllable."
cover(Verify_Controllable)
"Q0 is not normal."
cover(Verify_Normal)
{x0,x1,x2,x7,x8,x11}
cover(Use_Normality)
"Q0 is not strong M-controllable."
cover(Verify_StrongMcontrollable)
"Q0 is not M-controllable."
cover(Verify_Mcontrollable)
{x0,x1,x2,x4,x7,x8}
cover(USE_Approx)
{x0,x1,x2,x4,x7,x8,x9,x11}
{(x0|->{e3,e5,e6}),(x1|->{}),(x2|->{e9}),(x4|->{e4}),(x7|->{}),(x8|->{}),(x9|->{}),(x11|->{})}
{(x0|->{e1,e2,e4,e7}),(x1|->{e9}),(x2|->{}),(x4|->{}),(x7|->{e8}),(x8|->{}),(x9|->{}),(x11|->{})}
cover(USE_GetSupWCCP)
{x0,x1,x2,x4,x7,x8,x9}
{(x0|->{e3,e5,e6}),(x1|->{}),(x2|->{e9}),(x4|->{e4}),(x7|->{}),(x8|->{}),(x9|->{})}
{(x0|->{e1,e2,e4,e7}),(x1|->{e9}),(x2|->{}),(x4|->{}),(x7|->{e8}),(x8|->{}),(x9|->{})}
cover(USE_GetSupCCP)
{x0,x1,x2,x7,x8}
{(x0|->{e3,e4,e5,e6}),(x1|->{e9}),(x2|->{e9}),(x7|->{}),(x8|->{})}
{(x0|->{e1,e2,e7}),(x1|->{}),(x2|->{}),(x7|->{e8}),(x8|->{})}
cover(USE_GetSupNCCP)
{x0,x1,x2,x4,x7,x8}
{(x0|->{e3,e5,e6}),(x1|->{e9}),(x2|->{e9}),(x4|->{e3,e4}),(x7|->{}),(x8|->{})}
{(x0|->{e1,e2,e4,e7}),(x1|->{}),(x2|->{}),(x4|->{}),(x7|->{e8}),(x8|->{})}
cover(USE_GetSupSMCP)
{x0,x1,x2,x4,x7,x8}
{(x0|->{e3,e5,e6}),(x1|->{e9}),(x2|->{e9}),(x4|->{e4}),(x7|->{}),(x8|->{})}
{(x0|->{e1,e2,e4,e7}),(x1|->{}),(x2|->{}),(x4|->{}),(x7|->{e8}),(x8|->{})}
cover(USE_GetmaxMCP)
{{},{x0,x1,x2,x7,x8}}
cover(USE_NC)
*/