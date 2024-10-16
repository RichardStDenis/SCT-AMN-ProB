// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL ProcIEEE_77_1_Fig3

SETS
  States = {I1_I2, R1_I2, U1_I2,
            I1_R2, R1_R2, U1_R2,
            I1_U2, R1_U2, U1_U2, R12, R21 };
  Events = {a1, g1, f1, a2, g2, f2}  // ask, grant, free

CONSTANTS
  G, H                  // Automata for free and legal behaviors

PROPERTIES
//---------------------------------------------------------------------
// Two users of a resource FIFO and mutual exclusion.
//   P. J. G. Ramadge and W. M. Wonham,
//     The control of discrete event systems,
//     Proceedings of the IEEE, 77 (1): 81-98, 1989.
// Figures 3.
//---------------------------------------------------------------------
  G = rec(              // Automaton modeling the free behavior
          states: States - {R12, R21},
          events: Events,
          tf: {(I1_I2, a1, R1_I2), (R1_I2, g1, U1_I2), (U1_I2, f1, I1_I2),
               (I1_R2, a1, R1_R2), (R1_R2, g1, U1_R2), (U1_R2, f1, I1_R2),
               (I1_U2, a1, R1_U2), (R1_U2, g1, U1_U2), (U1_U2, f1, I1_U2),
               (I1_I2, a2, I1_R2), (I1_R2, g2, I1_U2), (I1_U2, f2, I1_I2),
               (R1_I2, a2, R1_R2), (R1_R2, g2, R1_U2), (R1_U2, f2, R1_I2),
               (U1_I2, a2, U1_R2), (U1_R2, g2, U1_U2), (U1_U2, f2, U1_I2) },
          x_0: I1_I2,
          Xm: {I1_I2},
          ctrlEvents: {g1, g2} )
  &
  H = rec(              // Automaton modeling the legal behavior
          states: States - {U1_U2},
          events: Events,
          tf: {(I1_I2, a1, R1_I2), (R1_I2, g1, U1_I2), (U1_I2, f1, I1_I2),
               (I1_I2, a2, I1_R2), (I1_R2, g2, I1_U2), (I1_U2, f2, I1_I2),
               (I1_R2, a1, R21), (R21, g2, R1_U2), (R1_U2, f2, R1_I2), 
               (R1_I2, a2, R12), (R12, g1, U1_R2), (U1_R2, f1, I1_R2), 
               (U1_I2, a2, U1_R2),
               (I1_U2, a1, R1_U2) },
          x_0: I1_I2,
          Xm: {I1_I2} )
END