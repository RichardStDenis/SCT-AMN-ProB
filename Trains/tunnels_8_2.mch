// copyright Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL tunnels_8_2

INCLUDES
 SBtrains_8_2  /*@file "./SBtrains_8_2.mch" */

SETS
 ObservationSpace = {s1s1, s1t1, s1s4, s1s5, s1t2, s1s8,
                     t1s1, t1t1, t1s4, t1s5, t1t2, t1s8,
                     s4s1, s4t1, s4s4, s4s5, s4t2, s4s8,
                     s5s1, s5t1, s5s4, s5s5, s5t2, s5s8,
                     t2s1, t2t1, t2s4, t2s5, t2t2, t2s8,
                     s8s1, s8t1, s8s4, s8s5, s8t2, s8s8 }

CONSTANTS
 tunnel            // Observation mask

PROPERTIES
 tunnel = {(s1_s1, s1s1), (s1_s2, s1t1), (s1_s3, s1t1), (s1_s4, s1s4), (s1_s5, s1s5), (s1_s6, s1t2), (s1_s7, s1t2), (s1_s8, s1s8),
           (s2_s1, t1s1), (s2_s2, t1t1), (s2_s3, t1t1), (s2_s4, t1s4), (s2_s5, t1s5), (s2_s6, s1t2), (s2_s7, t1t2), (s2_s8, t1s8),
           (s3_s1, t1s1), (s3_s2, t1t1), (s3_s3, t1t1), (s3_s4, t1s4), (s3_s5, t1s5), (s3_s6, t1t2), (s3_s7, t1t2), (s3_s8, t1s8),
           (s4_s1, s4s1), (s4_s2, s4t1), (s4_s3, s4t1), (s4_s4, s4s4), (s4_s5, s4s5), (s4_s6, s4t2), (s4_s7, s4t2), (s4_s8, s4s8),
           (s5_s1, s5s1), (s5_s2, s5t1), (s5_s3, s5t1), (s5_s4, s5s4), (s5_s5, s5s5), (s5_s6, s5t2), (s5_s7, s5t2), (s5_s8, s5s8),
           (s6_s1, t2s1), (s6_s2, t2t1), (s6_s3, t2t1), (s6_s4, t2s4), (s6_s5, t2s5), (s6_s6, t2t2), (s6_s7, t2t2), (s6_s8, t2s8),
           (s7_s1, t2s1), (s7_s2, t2t1), (s7_s3, t2t1), (s7_s4, t2s4), (s7_s5, t2s5), (s7_s6, t2t2), (s7_s7, t2t2), (s7_s8, t2s8),
           (s8_s1, s8s1), (s8_s2, s8t1), (s8_s3, s8t1), (s8_s4, s8s4), (s8_s5, s8s5), (s8_s6, s8t2), (s8_s7, s8t2), (s8_s8, s8s8) }
END