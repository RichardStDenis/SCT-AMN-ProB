// copyright Richard St_Denis, Universite de Sherbrooke, 2023.
MODEL LBtrains_S_6_2

SETS
 States = {s1_s1, s2_s1, s1_s2,
           s3_s1, s2_s2, s1_s3,
           s4_s1, s3_s2, s2_s3,
           s1_s4, s5_s1, s4_s2,
           s3_s3, s2_s4, s1_s5,
           s6_s1, s5_s2, s4_s3,
           s3_s4, s2_s5, s1_s6,
           s6_s2, s5_s3, s4_s4,
           s3_s5, s2_s6, s6_s3,
           s5_s4, s4_s5, s3_s6,
           s6_s4, s5_s5, s4_s6,
           s6_s5, s5_s6, s6_s6 };
 Events = {t1_1t2, t1_2t3, t1_3t4, t1_4t5, t1_5t6, t1_6t1,
           t2_1t2, t2_2t3, t2_3t4, t2_4t5, t2_5t6, t2_6t1 }

CONSTANTS
 nbr_trains, nbr_sections

PROPERTIES
 nbr_trains = 2 & nbr_sections = 6
END