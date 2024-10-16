// copyright Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL tunnels_6_2

INCLUDES
 SBtrains_6_2  /*@file "./SBtrains_6_2.mch" */

SETS
 ObservationSpace = {t1t1, t1t2, t1t3,
                     t2t1, t2t2, t2t3,
                     t3t1, t3t2, t3t3 }

CONSTANTS
 tunnel            // Observation mask

PROPERTIES
 tunnel = {(s1_s1, t1t1), (s1_s2, t1t1), (s1_s3, t1t2),
           (s1_s4, t1t2), (s1_s5, t1t3), (s1_s6, t1t3),
           (s2_s1, t1t1), (s2_s2, t1t1), (s2_s3, t1t2),
           (s2_s4, t1t2), (s2_s5, t1t3), (s2_s6, t1t3),
           (s3_s1, t2t1), (s3_s2, t2t1), (s3_s3, t2t2),
           (s3_s4, t2t2), (s3_s5, t2t3), (s3_s6, t2t3),
           (s4_s1, t2t1), (s4_s2, t2t1), (s4_s3, t2t2),
           (s4_s4, t2t2), (s4_s5, t2t3), (s4_s6, t2t3),
           (s5_s1, t3t1), (s5_s2, t3t1), (s5_s3, t3t2),
           (s5_s4, t3t2), (s5_s5, t3t3), (s5_s6, t3t3),
           (s6_s1, t3t1), (s6_s2, t3t1), (s6_s3, t3t2),
           (s6_s4, t3t2), (s6_s5, t3t3), (s6_s6, t3t3) }
END