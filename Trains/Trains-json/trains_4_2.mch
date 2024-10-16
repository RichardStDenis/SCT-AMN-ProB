// copyright Richard St_Denis, Universite de Sherbrooke, 2023.
MODEL SBtrains_4_2

SETS
 States = {s1_s1, s2_s1, s1_s2,
           s3_s1, s2_s2, s1_s3,
           s4_s1, s3_s2, s2_s3,
           s1_s4, s4_s2, s3_s3,
           s2_s4, s4_s3, s3_s4,
           s4_s4 };
 Events = {t1_1t2, t1_2t3, t1_3t4, t1_4t1, t2_1t2, t2_2t3, t2_3t4, t2_4t1};
 LocalStates = {s1, s2, s3, s4}

CONSTANTS
 G_FB,
 t41, t42,
 Qpred, BadStates

PROPERTIES
 G_FB = rec(       // Automaton modeling the free behavior
            states: States,
            events: Events,
            tf: {(s1_s1,t1_1t2,s2_s1), (s1_s1,t2_1t2,s1_s2), (s2_s1,t1_2t3,s3_s1),
                 (s2_s1,t2_1t2,s2_s2), (s1_s2,t1_1t2,s2_s2), (s1_s2,t2_2t3,s1_s3),
                 (s3_s1,t1_3t4,s4_s1), (s3_s1,t2_1t2,s3_s2), (s2_s2,t1_2t3,s3_s2),
                 (s2_s2,t2_2t3,s2_s3), (s1_s3,t1_1t2,s2_s3), (s1_s3,t2_3t4,s1_s4),
                 (s4_s1,t1_4t1,s1_s1), (s4_s1,t2_1t2,s4_s2), (s3_s2,t1_3t4,s4_s2),
                 (s3_s2,t2_2t3,s3_s3), (s2_s3,t1_2t3,s3_s3), (s2_s3,t2_3t4,s2_s4),
                 (s1_s4,t1_1t2,s2_s4), (s1_s4,t2_4t1,s1_s1), (s4_s2,t1_4t1,s1_s2),
                 (s4_s2,t2_2t3,s4_s3), (s3_s3,t1_3t4,s4_s3), (s3_s3,t2_3t4,s3_s4),
                 (s2_s4,t1_2t3,s3_s4), (s2_s4,t2_4t1,s2_s1), (s4_s3,t1_4t1,s1_s3),
                 (s4_s3,t2_3t4,s4_s4), (s3_s4,t1_3t4,s4_s4), (s3_s4,t2_4t1,s3_s1),
                 (s4_s4,t1_4t1,s1_s4), (s4_s4,t2_4t1,s4_s1) },
            x_0: s1_s1,
            Xm: {s1_s1},
            ctrlEvents: {t1_2t3, t1_4t1, t2_2t3, t2_4t1} )
 &
 t41 = {
      (s1_s1,s1),(s2_s1,s2),(s1_s2,s1),
      (s3_s1,s3),(s2_s2,s2),(s1_s3,s1),
      (s4_s1,s4),(s3_s2,s3),(s2_s3,s2),
      (s1_s4,s1),(s4_s2,s4),(s3_s3,s3),
      (s2_s4,s2),(s4_s3,s4),(s3_s4,s3),
      (s4_s4,s4) }
 &
 t42 = {
      (s1_s1,s1),(s2_s1,s1),(s1_s2,s2),
      (s3_s1,s1),(s2_s2,s2),(s1_s3,s3),
      (s4_s1,s1),(s3_s2,s2),(s2_s3,s3),
      (s1_s4,s4),(s4_s2,s2),(s3_s3,s3),
      (s2_s4,s4),(s4_s3,s3),(s3_s4,s4),
      (s4_s4,s4) }
 &
 BadStates =  States    // to be completed
 &
 Qpred = States - BadStates
END