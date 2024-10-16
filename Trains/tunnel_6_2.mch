// copyright Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL tunnel_6_2

INCLUDES
 SBtrains_6_2  /*@file "./SBtrains_6_2.mch" */

SETS
 ObservationSpace = {s1s1, s1s2, s1tn, s1s6,
                     s2s1, s2s2, s2tn, s2s6,
                     tns1, tns2, tntn, tns6,
                     s6s1, s6s2, s6tn, s6s6 }

CONSTANTS
 tunnel            // Observation mask

PROPERTIES
 tunnel = {(s1_s1, s1s1), (s1_s2, s1s2), (s1_s3, s1tn),
           (s1_s4, s1tn), (s1_s5, s1tn), (s1_s6, s1s6),
           (s2_s1, s2s1), (s2_s2, s2s2), (s2_s3, s2tn),
           (s2_s4, s2tn), (s2_s5, s2tn), (s2_s6, s2s6),
           (s3_s1, tns1), (s3_s2, tns2), (s3_s3, tntn),
           (s3_s4, tntn), (s3_s5, tntn), (s3_s6, tns6),
           (s4_s1, tns1), (s4_s2, tns2), (s4_s3, tntn),
           (s4_s4, tntn), (s4_s5, tntn), (s4_s6, tns6),
           (s5_s1, tns1), (s5_s2, tns2), (s5_s3, tntn),
           (s5_s4, tntn), (s5_s5, tntn), (s5_s6, tns6),
           (s6_s1, s6s1), (s6_s2, s6s2), (s6_s3, s6tn),
           (s6_s4, s6tn), (s6_s5, s6tn), (s6_s6, s6s6) }
END