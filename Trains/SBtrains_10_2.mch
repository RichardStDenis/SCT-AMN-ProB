// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBtrains_10_2

INCLUDES
 LBtrains_FB_10_2 /*@file "./LBtrains_FB_10_2.mch" */

SETS
 LocalStates = {s1, s2, s3, s4, s5, s6, s7, s8, s9, s10}

DEFINITIONS
 OnSameSection(x) ==
  t101(x) = t102(x);
 OnAdjacentSections(x) ==
  nextSection(t101(x)) = t102(x) or prevSection(t101(x)) = t102(x);

CONSTANTS
 t101, t102,
 nextSection, prevSection,
  Qpred1
//Qpred2

PROPERTIES
 t101 = {
      (s1_s1,s1),(s2_s1,s2),(s1_s2,s1),
      (s3_s1,s3),(s2_s2,s2),(s1_s3,s1),
      (s4_s1,s4),(s3_s2,s3),(s2_s3,s2),
      (s1_s4,s1),(s5_s1,s5),(s4_s2,s4),
      (s3_s3,s3),(s2_s4,s2),(s1_s5,s1),
      (s6_s1,s6),(s5_s2,s5),(s4_s3,s4),
      (s3_s4,s3),(s2_s5,s2),(s1_s6,s1),
      (s7_s1,s7),(s6_s2,s6),(s5_s3,s5),
      (s4_s4,s4),(s3_s5,s3),(s2_s6,s2),
      (s1_s7,s1),(s8_s1,s8),(s7_s2,s7),
      (s6_s3,s6),(s5_s4,s5),(s4_s5,s4),
      (s3_s6,s3),(s2_s7,s2),(s1_s8,s1),
      (s9_s1,s9),(s8_s2,s8),(s7_s3,s7),
      (s6_s4,s6),(s5_s5,s5),(s4_s6,s4),
      (s3_s7,s3),(s2_s8,s2),(s1_s9,s1),
      (s10_s1,s10),(s9_s2,s9),(s8_s3,s8),
      (s7_s4,s7),(s6_s5,s6),(s5_s6,s5),
      (s4_s7,s4),(s3_s8,s3),(s2_s9,s2),
      (s1_s10,s1),(s10_s2,s10),(s9_s3,s9),
      (s8_s4,s8),(s7_s5,s7),(s6_s6,s6),
      (s5_s7,s5),(s4_s8,s4),(s3_s9,s3),
      (s2_s10,s2),(s10_s3,s10),(s9_s4,s9),
      (s8_s5,s8),(s7_s6,s7),(s6_s7,s6),
      (s5_s8,s5),(s4_s9,s4),(s3_s10,s3),
      (s10_s4,s10),(s9_s5,s9),(s8_s6,s8),
      (s7_s7,s7),(s6_s8,s6),(s5_s9,s5),
      (s4_s10,s4),(s10_s5,s10),(s9_s6,s9),
      (s8_s7,s8),(s7_s8,s7),(s6_s9,s6),
      (s5_s10,s5),(s10_s6,s10),(s9_s7,s9),
      (s8_s8,s8),(s7_s9,s7),(s6_s10,s6),
      (s10_s7,s10),(s9_s8,s9),(s8_s9,s8),
      (s7_s10,s7),(s10_s8,s10),(s9_s9,s9),
      (s8_s10,s8),(s10_s9,s10),(s9_s10,s9),
      (s10_s10,s10) }
 &
 t102 = {
      (s1_s1,s1),(s2_s1,s1),(s1_s2,s2),
      (s3_s1,s1),(s2_s2,s2),(s1_s3,s3),
      (s4_s1,s1),(s3_s2,s2),(s2_s3,s3),
      (s1_s4,s4),(s5_s1,s1),(s4_s2,s2),
      (s3_s3,s3),(s2_s4,s4),(s1_s5,s5),
      (s6_s1,s1),(s5_s2,s2),(s4_s3,s3),
      (s3_s4,s4),(s2_s5,s5),(s1_s6,s6),
      (s7_s1,s1),(s6_s2,s2),(s5_s3,s3),
      (s4_s4,s4),(s3_s5,s5),(s2_s6,s6),
      (s1_s7,s7),(s8_s1,s1),(s7_s2,s2),
      (s6_s3,s3),(s5_s4,s4),(s4_s5,s5),
      (s3_s6,s6),(s2_s7,s7),(s1_s8,s8),
      (s9_s1,s1),(s8_s2,s2),(s7_s3,s3),
      (s6_s4,s4),(s5_s5,s5),(s4_s6,s6),
      (s3_s7,s7),(s2_s8,s8),(s1_s9,s9),
      (s10_s1,s1),(s9_s2,s2),(s8_s3,s3),
      (s7_s4,s4),(s6_s5,s5),(s5_s6,s6),
      (s4_s7,s7),(s3_s8,s8),(s2_s9,s9),
      (s1_s10,s10),(s10_s2,s2),(s9_s3,s3),
      (s8_s4,s4),(s7_s5,s5),(s6_s6,s6),
      (s5_s7,s7),(s4_s8,s8),(s3_s9,s9),
      (s2_s10,s10),(s10_s3,s3),(s9_s4,s4),
      (s8_s5,s5),(s7_s6,s6),(s6_s7,s7),
      (s5_s8,s8),(s4_s9,s9),(s3_s10,s10),
      (s10_s4,s4),(s9_s5,s5),(s8_s6,s6),
      (s7_s7,s7),(s6_s8,s8),(s5_s9,s9),
      (s4_s10,s10),(s10_s5,s5),(s9_s6,s6),
      (s8_s7,s7),(s7_s8,s8),(s6_s9,s9),
      (s5_s10,s10),(s10_s6,s6),(s9_s7,s7),
      (s8_s8,s8),(s7_s9,s9),(s6_s10,s10),
      (s10_s7,s7),(s9_s8,s8),(s8_s9,s9),
      (s7_s10,s10),(s10_s8,s8),(s9_s9,s9),
      (s8_s10,s10),(s10_s9,s9),(s9_s10,s10),
      (s10_s10,s10) }
 &
 nextSection = {(s1,s2), (s2,s3), (s3,s4), (s4,s5), (s5,s6),
                (s6,s7), (s7,s8), (s8,s9), (s9,s10), (s10,s1) }
 &
 prevSection =  nextSection~
 &
  Qpred1 = {x | not(OnSameSection(x))}
//Qpred2 = {x | not(OnSameSection(x)) & not(OnAdjacentSections(x))}
END