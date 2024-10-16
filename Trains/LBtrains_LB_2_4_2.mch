// copyright Richard St_Denis, Universite de Sherbrooke, 2023.
MODEL LBtrains_LB_2_4_2

INCLUDES
//LBtrains_S_4_2 /*@file "./LBtrains_S_4_2.mch" */   // to be used with test_trains_gen_LB
  LBtrains_FB_4_2 /*@file "./LBtrains_FB_4_2.mch" */ // to be used with test_trains_LB

CONSTANTS
 G_LB,
 bound_G_LB

PROPERTIES
 G_LB : struct(states: POW(G_FB'states), events: POW(G_FB'events),
               tf: (G_FB'states*G_FB'events)+->G_FB'states,
               x_0: G_FB'states, Xm: POW(G_FB'states) )    // because the transition function is empty
 &
 G_LB = rec(       // Automaton modeling the legal behavior
            events:{t1_1t2,t1_2t3,t1_3t4,t1_4t1,
                    t2_1t2,t2_2t3,t2_3t4,t2_4t1},
            x_0:s2_s4,
            Xm:{s2_s4},
            states:{s3_s1,s1_s3,s4_s2,s2_s4},
            tf:{} ) &
 bound_G_LB = 8
END