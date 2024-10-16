// copyright Richard St_Denis, Universite de Sherbrooke, 2023.
MODEL LBtrains_LB_2_8_2

INCLUDES
//LBtrains_S_8_2 /*@file "./LBtrains_S_8_2.mch" */   // to be used with test_trains_gen_LB
  LBtrains_FB_8_2 /*@file "./LBtrains_FB_8_2.mch" */ // to be used with test_trains_LB

CONSTANTS
 G_LB,
 bound_G_LB

PROPERTIES
 G_LB = rec(       // Automaton modeling the legal behavior
            events:{t1_1t2,t1_2t3,t1_3t4,t1_4t5,t1_5t6,t1_6t7,t1_7t8,t1_8t1,
                    t2_1t2,t2_2t3,t2_3t4,t2_4t5,t2_5t6,t2_6t7,t2_7t8,t2_8t1 },
            x_0:s2_s4,
            Xm:{s2_s4},
            states:{s3_s1,s1_s3,s4_s1,s1_s4,s5_s1,s4_s2,s2_s4,s1_s5,s6_s1,
                    s5_s2,s2_s5,s1_s6,s7_s1,s6_s2,s5_s3,s3_s5,s2_s6,s1_s7,
                    s7_s2,s6_s3,s3_s6,s2_s7,s8_s2,s7_s3,s6_s4,s4_s6,s3_s7,
                    s2_s8,s8_s3,s7_s4,s4_s7,s3_s8,s8_s4,s7_s5,s5_s7,s4_s8,
                    s8_s5,s5_s8,s8_s6,s6_s8 },
            tf:{((s3_s1|->t1_3t4)|->s4_s1),((s1_s3|->t2_3t4)|->s1_s4),
                ((s4_s1|->t1_4t5)|->s5_s1),((s4_s1|->t2_1t2)|->s4_s2),
                ((s1_s4|->t1_1t2)|->s2_s4),((s1_s4|->t2_4t5)|->s1_s5),
                ((s5_s1|->t1_5t6)|->s6_s1),((s5_s1|->t2_1t2)|->s5_s2),
                ((s4_s2|->t1_4t5)|->s5_s2),((s2_s4|->t2_4t5)|->s2_s5),
                ((s1_s5|->t1_1t2)|->s2_s5),((s1_s5|->t2_5t6)|->s1_s6),
                ((s6_s1|->t1_6t7)|->s7_s1),((s6_s1|->t2_1t2)|->s6_s2),
                ((s5_s2|->t1_5t6)|->s6_s2),((s5_s2|->t2_2t3)|->s5_s3),
                ((s2_s5|->t1_2t3)|->s3_s5),((s2_s5|->t2_5t6)|->s2_s6),
                ((s1_s6|->t1_1t2)|->s2_s6),((s1_s6|->t2_6t7)|->s1_s7),
                ((s7_s1|->t2_1t2)|->s7_s2),((s6_s2|->t1_6t7)|->s7_s2),
                ((s6_s2|->t2_2t3)|->s6_s3),((s5_s3|->t1_5t6)|->s6_s3),
                ((s3_s5|->t2_5t6)|->s3_s6),((s2_s6|->t1_2t3)|->s3_s6),
                ((s2_s6|->t2_6t7)|->s2_s7),((s1_s7|->t1_1t2)|->s2_s7),
                ((s7_s2|->t1_7t8)|->s8_s2),((s7_s2|->t2_2t3)|->s7_s3),
                ((s6_s3|->t1_6t7)|->s7_s3),((s6_s3|->t2_3t4)|->s6_s4),
                ((s3_s6|->t1_3t4)|->s4_s6),((s3_s6|->t2_6t7)|->s3_s7),
                ((s2_s7|->t1_2t3)|->s3_s7),((s2_s7|->t2_7t8)|->s2_s8),
                ((s8_s2|->t2_2t3)|->s8_s3),((s7_s3|->t1_7t8)|->s8_s3),
                ((s7_s3|->t2_3t4)|->s7_s4),((s6_s4|->t1_6t7)|->s7_s4),
                ((s4_s6|->t2_6t7)|->s4_s7),((s3_s7|->t1_3t4)|->s4_s7),
                ((s3_s7|->t2_7t8)|->s3_s8),((s2_s8|->t1_2t3)|->s3_s8),
                ((s8_s3|->t1_8t1)|->s1_s3),((s8_s3|->t2_3t4)|->s8_s4),
                ((s7_s4|->t1_7t8)|->s8_s4),((s7_s4|->t2_4t5)|->s7_s5),
                ((s4_s7|->t1_4t5)|->s5_s7),((s4_s7|->t2_7t8)|->s4_s8),
                ((s3_s8|->t1_3t4)|->s4_s8),((s3_s8|->t2_8t1)|->s3_s1),
                ((s8_s4|->t1_8t1)|->s1_s4),((s8_s4|->t2_4t5)|->s8_s5),
                ((s7_s5|->t1_7t8)|->s8_s5),((s5_s7|->t2_7t8)|->s5_s8),
                ((s4_s8|->t1_4t5)|->s5_s8),((s4_s8|->t2_8t1)|->s4_s1),
                ((s8_s5|->t1_8t1)|->s1_s5),((s8_s5|->t2_5t6)|->s8_s6),
                ((s5_s8|->t1_5t6)|->s6_s8),((s5_s8|->t2_8t1)|->s5_s1),
                ((s8_s6|->t1_8t1)|->s1_s6),((s6_s8|->t2_8t1)|->s6_s1) } ) &
 bound_G_LB = 16
END