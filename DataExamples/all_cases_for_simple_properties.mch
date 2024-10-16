// Richard St-Denis, Universite de Sherbrooke, 2022.
MACHINE all_cases_for_simple_properties

SETS
  States = {s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10};
  Events = {e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10}

CONSTANTS
  G, H                  // Automata for free and legal behaviors

//---------------------------------------------------------------------
PROPERTIES
  G = rec(              // Automaton modeling the free behavior
          states: States,
          events: Events,
          tf: {(s0,e1,s1), (s0,e3,s3), (s0,e7,s7),
                (s1,e2,s2), (s2,e10,s10),
                (s3,e4,s4), (s4,e5,s5), (s4,e6,s6),
                (s5,e10,s10), (s6,e10,s10),
                (s7,e8,s8), (s8,e9,s9), (s9,e10,s10)},
          x_0: s0,
          Xm: {s4, s10},
          ctrlEvents: {e2, e4, e8, e9},
          obsEvents: Events - {e5, e6} )
  &
  H = rec(              // Automaton modeling the legal behavior
          states: States - {s2} /* uncontrollable */ - {s6},
          events: Events,
          tf: {(s0,e1,s1), (s0,e3,s3), (s0,e7,s7),
               (s1,e2,s2), (s2,e10,s10),
               (s3,e4,s4), (s4,e5,s5), (s4,e6,s6),
               (s5,e10,s10), (s6,e10,s10),
               (s7,e8,s8), (s8,e9,s9), (s9,e10,s10)} -
                        {(s1,e2,s2)} /* uncontrollable */
                        - {(s2,e10,s10)}
                        - {(s4,e6,s6)} - {(s6,e10,s10)},
          x_0: s0,
          Xm: {s10} /* LmG-closed */ \/ {s4} )

// prefix-closed: "Xm: States".
// not Lm(G)-closed: remove "\/ {s4}" in marked states of H.
// controllable: remove "- {s6}" in states of H and
//               remove "- {(s4,e6,s6)} - {(s6,e10,s10)}" in
//               transitions of H.
// normality: remove "s5" or "s6" in observable events of G.
// not observable: remove "- {s6}" in states of H and
//                 remove "- {(s4,e6,s6)}" in
//                 transitions of H.
END