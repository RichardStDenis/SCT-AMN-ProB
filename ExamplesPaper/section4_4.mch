// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL section4_4

SETS
 States = {s0, s1, s2, s3};
 Events = {a,b,c}

CONSTANTS
 Gsys,        // A generator
 L_G,         // A prefix-closed language
 Fl,
 CS_in

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 5000000;
 SET_PREF_SMT == FALSE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../Theories/SCT_core.def";
 "LibraryIO.def";

PROPERTIES
// Example in page 251 of:
//   K. C. Wong and W. M. Wohnam,
//     Hierarchical Control of Discrete-event systems
//     Discrete Event Dynamic Systems: Theory and Applications,
//     6 (), 241-273, 1996.

 Gsys = rec(states: States,
            events: Events,
            tf: {(s0,a,s1), (s1,b,s2), (s1,c,s3) },
            x_0: s0,
            Xm: States,
            ctrlEvents: Events ) &

 L_G = GetStrings(Gsys,4) &
 Fl = F_L(L_G) &

 CS_in : ControlStructure(Fl,L_G) &
 CS_in = {({}|->{{}}),
          ({[]}|->{{},{[]}}),
          ({[],[a]}|->{{},{[]},{[a]},{[],[a]}}),
          ({[],[a],[a,b]}|->{{},{[]},{[a,b]},{[],[a,b]},{[a],[a,b]},{[],[a],[a,b]}}),
          ({[],[a],[a,c]}|->{{},{[]},{[a,c]},{[],[a,c]},{[a],[a,c]},{[],[a],[a,c]}}),
          ({[],[a],[a,b],[a,c]}|->{{},{[]},{[a,b],[a,c]},{[],[a,b],[a,c]},{[a],[a,b],[a,c]},{[],[a],[a,b],[a,c]}}) }

VARIABLES
 code

INITIALISATION
 code := 0

INVARIANT
 code >= 0 & code <= 2

OPERATIONS
 DisplayResults = PRE code = 0 THEN
  PRINTF("L: ~w~n", [L_G]);
  PRINTF("FL: ~w~n", [Fl]);
  code := 1
END;

 FindControlStructure = PRE code = 1 THEN
  ANY Cs WHERE Cs : ControlStructure(Fl,L_G) & A1(Cs,Fl) & A2(Cs,Fl) & A3(Cs,Fl) & A4(Cs,Fl) &
  // TO HAVE a control structure that corresponds to the set of controllable events {a}
  !H__.(H__ : Fl  =>  Cs(H__) = UNION(K_).(K_ : POW(H__) | C(K_,H__,{b,c}) ) )
 THEN PRINTF("Control structure: ~w~n", [Cs]);
      IF Cs = CS_in THEN PRINT("The result is good") ELSE
                         PRINT("The result is bad") END;
      code :=2 END END END

/*
OUTPUT:
L: {{},{(1|->a)},{(1|->a),(2|->b)},{(1|->a),(2|->c)}}
FL: {{},{{}},{{},{(1|->a)}},{{},{(1|->a)},{(1|->a),(2|->b)}},
    {{},{(1|->a)},{(1|->a),(2|->c)}},{{},{(1|->a)},{(1|->a),(2|->b)},{(1|->a),(2|->c)}}}
cover(DisplayResults)
Control structure: 
{({}|->{{}}),
 ({{}}|->{{},{{}}}),
 ({{},{(1|->a)}}|->{{},{{}},{{},{(1|->a)}},{{(1|->a)}}}),
 ({{},{(1|->a)},{(1|->a),(2|->b)}}|->{{},{{}},{{},{(1|->a),(2|->b)}},{{},{(1|->a)},{(1|->a),(2|->b)}},{{(1|->a)},{(1|->a),(2|->b)}},{{(1|->a),(2|->b)}}}),
 ({{},{(1|->a)},{(1|->a),(2|->c)}}|->{{},{{}},{{},{(1|->a),(2|->c)}},{{},{(1|->a)},{(1|->a),(2|->c)}},{{(1|->a)},{(1|->a),(2|->c)}},{{(1|->a),(2|->c)}}}),
 ({{},{(1|->a)},{(1|->a),(2|->b)},{(1|->a),(2|->c)}}|->{{},{{}},{{},{(1|->a)},{(1|->a),(2|->b)},{(1|->a),(2|->c)}},{{},{(1|->a),(2|->b)},{(1|->a),(2|->c)}},{{(1|->a)},{(1|->a),(2|->b)},{(1|->a),(2|->c)}},{{(1|->a),(2|->b)},{(1|->a),(2|->c)}}})}
"The result is good"
cover(FindControlStructure)
*/