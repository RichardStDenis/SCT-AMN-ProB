// copyright Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL tunnel_8_2

INCLUDES
 SBtrains_8_2  /*@file "./SBtrains_8_2.mch" */

SETS
 ObservationSpace = {s1s1, s1s2, s1tn, s1s6, s1s7, s1s8,
                     s2s1, s2s2, s2tn, s2s6, s2s7, s2s8,
                     tns1, tns2, tntn, tns6, tns7, tns8,
                     s6s1, s6s2, s6tn, s6s6, s6s7, s6s8,
                     s7s1, s7s2, s7tn, s7s6, s7s7, s7s8,
                     s8s1, s8s2, s8tn, s8s6, s8s7, s8s8 }

CONSTANTS
 tunnel            // Observation mask

PROPERTIES
 tunnel = {(s1_s1, s1s1), (s1_s2, s1s2), (s1_s3, s1tn), (s1_s4, s1tn), (s1_s5, s1tn), (s1_s6, s1s6), (s1_s7, s1s7), (s1_s8, s1s8),
           (s2_s1, s2s1), (s2_s2, s2s2), (s2_s3, s2tn), (s2_s4, s2tn), (s2_s5, s2tn), (s2_s6, s2s6), (s2_s7, s2s7), (s2_s8, s2s8),
           (s3_s1, tns1), (s3_s2, tns2), (s3_s3, tntn), (s3_s4, tntn), (s3_s5, tntn), (s3_s6, tns6), (s3_s7, tns7), (s3_s8, tns8),
           (s4_s1, tns1), (s4_s2, tns2), (s4_s3, tntn), (s4_s4, tntn), (s4_s5, tntn), (s4_s6, tns6), (s4_s7, tns7), (s4_s8, tns8),
           (s5_s1, tns1), (s5_s2, tns2), (s5_s3, tntn), (s5_s4, tntn), (s5_s5, tntn), (s5_s6, tns6), (s5_s7, tns7), (s5_s8, tns8),
           (s6_s1, s6s1), (s6_s2, s6s2), (s6_s3, s6tn), (s6_s4, s6tn), (s6_s5, s6tn), (s6_s6, s6s6), (s6_s7, s6s7), (s6_s8, s6s8),
           (s7_s1, s7s1), (s7_s2, s7s2), (s7_s3, s7tn), (s7_s4, s7tn), (s7_s5, s7tn), (s7_s6, s7s6), (s7_s7, s7s7), (s7_s8, s7s8),
           (s8_s1, s8s1), (s8_s2, s8s2), (s8_s3, s8tn), (s8_s4, s8tn), (s8_s5, s8tn), (s8_s6, s8s6), (s8_s7, s8s7), (s8_s8, s8s8) }
END