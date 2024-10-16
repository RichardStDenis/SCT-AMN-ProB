// copyright Richard St_Denis, Universite de Sherbrooke, 2023.
MODEL LBtrains_LB_1_6_2

INCLUDES
//LBtrains_S_6_2 /*@file "./LBtrains_S_6_2.mch" */   // to be used with test_trains_gen_LB
  LBtrains_FB_6_2 /*@file "./LBtrains_FB_6_2.mch" */ // to be used with test_trains_LB

CONSTANTS
 G_LB,
 bound_G_LB

PROPERTIES
 G_LB = rec(       // Automaton modeling the legal behavior
            events:{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t1,
                    t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t1 },
            x_0:s2_s4,
            Xm:{s2_s4},
            states:{s2_s1,s1_s2,s3_s1,s1_s3,s4_s1,s3_s2,s2_s3,s1_s4,s5_s1,s4_s2,
                    s2_s4,s1_s5,s6_s1,s5_s2,s4_s3,s3_s4,s2_s5,s1_s6,s6_s2,s5_s3,
                    s3_s5,s2_s6,s6_s3,s5_s4,s4_s5,s3_s6,s6_s4,s4_s6,s6_s5,s5_s6 },
            tf:{((s2_s1|->t1_2t3)|->s3_s1),((s1_s2|->t2_2t3)|->s1_s3),
                ((s3_s1|->t1_3t4)|->s4_s1),((s3_s1|->t2_1t2)|->s3_s2),
                ((s1_s3|->t1_1t2)|->s2_s3),((s1_s3|->t2_3t4)|->s1_s4),
                ((s4_s1|->t1_4t5)|->s5_s1),((s4_s1|->t2_1t2)|->s4_s2),
                ((s3_s2|->t1_3t4)|->s4_s2),((s2_s3|->t2_3t4)|->s2_s4),
                ((s1_s4|->t1_1t2)|->s2_s4),((s1_s4|->t2_4t5)|->s1_s5),
                ((s5_s1|->t1_5t6)|->s6_s1),((s5_s1|->t2_1t2)|->s5_s2),
                ((s4_s2|->t1_4t5)|->s5_s2),((s4_s2|->t2_2t3)|->s4_s3),
                ((s2_s4|->t1_2t3)|->s3_s4),((s2_s4|->t2_4t5)|->s2_s5),
                ((s1_s5|->t1_1t2)|->s2_s5),((s1_s5|->t2_5t6)|->s1_s6),
                ((s6_s1|->t2_1t2)|->s6_s2),((s5_s2|->t1_5t6)|->s6_s2),
                ((s5_s2|->t2_2t3)|->s5_s3),((s4_s3|->t1_4t5)|->s5_s3),
                ((s3_s4|->t2_4t5)|->s3_s5),((s2_s5|->t1_2t3)|->s3_s5),
                ((s2_s5|->t2_5t6)|->s2_s6),((s1_s6|->t1_1t2)|->s2_s6),
                ((s6_s2|->t1_6t1)|->s1_s2),((s6_s2|->t2_2t3)|->s6_s3),
                ((s5_s3|->t1_5t6)|->s6_s3),((s5_s3|->t2_3t4)|->s5_s4),
                ((s3_s5|->t1_3t4)|->s4_s5),((s3_s5|->t2_5t6)|->s3_s6),
                ((s2_s6|->t1_2t3)|->s3_s6),((s2_s6|->t2_6t1)|->s2_s1),
                ((s6_s3|->t1_6t1)|->s1_s3),((s6_s3|->t2_3t4)|->s6_s4),
                ((s5_s4|->t1_5t6)|->s6_s4),((s4_s5|->t2_5t6)|->s4_s6),
                ((s3_s6|->t1_3t4)|->s4_s6),((s3_s6|->t2_6t1)|->s3_s1),
                ((s6_s4|->t1_6t1)|->s1_s4),((s6_s4|->t2_4t5)|->s6_s5),
                ((s4_s6|->t1_4t5)|->s5_s6),((s4_s6|->t2_6t1)|->s4_s1),
                ((s6_s5|->t1_6t1)|->s1_s5),((s5_s6|->t2_6t1)|->s5_s1) } ) &
 bound_G_LB = 12
END