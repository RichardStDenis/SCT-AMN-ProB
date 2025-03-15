// copyright  Richard St-Denis, Universite de Sherbrooke, 2022.
MODEL section5_2

SETS
 States = {x0,x1, x2, x3, x4, x5, x6, x7, x8, x9};
 Events = {e0, e1, e2, e3, e4, e5, e6, e7, e8, e9}

ABSTRACT_CONSTANTS GetsupCCP

CONSTANTS
 Gsys,        // Automata for the uncontrolled behavior
 Qspec        // Predicate representing the specification

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 1000000;
 SET_PREF_SMT == FALSE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../Theories/SBCDES.def";
 "LibraryIO.def"

PROPERTIES
Gsys = rec(states: States,    // Automaton modeling the uncontrolled behavior
           events: Events,
           tf: {(x0, e1, x1), (x0, e2, x2), (x0, e6, x6), (x1, e0, x0),
                (x2, e3, x3), (x3, e4, x4), (x4, e5, x5),
                (x6, e7, x7), (x8, e9, x9), (x9, e8, x8) },
           x_0: x0,
           Xm: {x1, x4, x6, x9},
           ctrlEvents: {e2, e4, e7, e8, e9} ) &
 Qspec = States - {x5, x7} &

 Gsys : Generator_C(Gsys'states,Gsys'events) &
 GetsupCCP = lambdaGetsupCCP(Gsys,GetsupCCP)

OPERATIONS
 SearchSupWCCP = ANY sup_ WHERE
  IsASupWCCP(Gsys,CtrlField(Gsys,Qspec),sup_) THEN PRINT(sup_) END;

 Display_SFBC = LET sup BE sup = GetsupCCP(Qspec) IN
  PRINT(A(Gsys,sup,x0));
  PRINT(A(Gsys,sup,x1));
  PRINT(A(Gsys,sup,x2));
  PRINT(A(Gsys,sup,x3));
  PRINT(A(Gsys,sup,x4));
  PRINT(A(Gsys,sup,x5));
  PRINT(A(Gsys,sup,x6));
  PRINT(A(Gsys,sup,x7));
  PRINT(A(Gsys,sup,x8));
  PRINT(A(Gsys,sup,x9));
  IF f_e(Gsys,sup,e1,x0) THEN PRINT("e1 allowed to occur at x0") END;
  IF f_e(Gsys,sup,e2,x0) THEN PRINT("e2 allowed to occur at x0") END;
  IF f_e(Gsys,sup,e6,x0) THEN PRINT("e6 allowed to occur at x0") END;
  IF f_e(Gsys,sup,e0,x1) THEN PRINT("e0 allowed to occur at x1") END;
  IF f_e(Gsys,sup,e3,x2) THEN PRINT("e3 allowed to occur at x2") END;
  IF f_e(Gsys,sup,e4,x3) THEN PRINT("e4 allowed to occur at x3") END;
  IF f_e(Gsys,sup,e5,x4) THEN PRINT("e5 allowed to occur at x4") END;
  IF f_e(Gsys,sup,e7,x6) THEN PRINT("e7 allowed to occur at x6") END;
  IF f_e(Gsys,sup,e9,x8) THEN PRINT("e9 allowed to occur at x8") END;
  IF f_e(Gsys,sup,e8,x9) THEN PRINT("e8 allowed to occur at x9") END;
  ANY sfbc WHERE sfbc : SFBCf(Gsys'states,Gsys'events) &
   sfbc = f_a(Gsys,sup) THEN PRINT(sfbc) END END END

/*
OUTPUT:
{x0,x1,x6,x8,x9}
cover(SearchSupWCCP)
{e2}
{}
{}
{e4}
{}
{}
{e7}
{}
{e9}
{e8}
"e1 allowed to occur at x0"
"e6 allowed to occur at x0"
"e0 allowed to occur at x1"
"e3 allowed to occur at x2"
"e5 allowed to occur at x4"
{(x0|->{e0,e1,e3,e4,e5,e6,e7,e8,e9}),
 (x1|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),
 (x2|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),
 (x3|->{e0,e1,e2,e3,e5,e6,e7,e8,e9}),
 (x4|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),
 (x5|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),
 (x6|->{e0,e1,e2,e3,e4,e5,e6,e8,e9}),
 (x7|->{e0,e1,e2,e3,e4,e5,e6,e7,e8,e9}),
 (x8|->{e0,e1,e2,e3,e4,e5,e6,e7,e8}),
 (x9|->{e0,e1,e2,e3,e4,e5,e6,e7,e9})}
cover(Display_SFBC)
*/