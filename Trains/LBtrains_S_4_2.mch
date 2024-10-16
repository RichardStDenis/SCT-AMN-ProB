// copyright Richard St_Denis, Universite de Sherbrooke, 2023.
MODEL LBtrains_S_4_2

SETS
 States = {s1_s1, s2_s1, s1_s2,
           s3_s1, s2_s2, s1_s3,
           s4_s1, s3_s2, s2_s3,
           s1_s4, s4_s2, s3_s3,
           s2_s4, s4_s3, s3_s4,
           s4_s4 };
 Events = {t1_1t2, t1_2t3, t1_3t4, t1_4t1,
           t2_1t2, t2_2t3, t2_3t4, t2_4t1 }

CONSTANTS
 nbr_trains, nbr_sections

PROPERTIES
 nbr_trains = 2 & nbr_sections = 4
END