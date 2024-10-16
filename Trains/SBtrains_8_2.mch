// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBtrains_8_2

INCLUDES
 LBtrains_FB_8_2 /*@file "./LBtrains_FB_8_2.mch" */

SETS
 LocalStates = {s1, s2, s3, s4, s5, s6, s7, s8}

DEFINITIONS
 OnSameSection(x) ==
  t81(x) = t82(x);
 OnAdjacentSections(x) ==
  nextSection(t81(x)) = t82(x) or prevSection(t81(x)) = t82(x);

CONSTANTS
 t81, t82,
 nextSection, prevSection,
  Qpred1
//Qpred2

PROPERTIES
 t81 = {
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
      (s8_s2,s8),(s7_s3,s7),(s6_s4,s6),
      (s5_s5,s5),(s4_s6,s4),(s3_s7,s3),
      (s2_s8,s2),(s8_s3,s8),(s7_s4,s7),
      (s6_s5,s6),(s5_s6,s5),(s4_s7,s4),
      (s3_s8,s3),(s8_s4,s8),(s7_s5,s7),
      (s6_s6,s6),(s5_s7,s5),(s4_s8,s4),
      (s8_s5,s8),(s7_s6,s7),(s6_s7,s6),
      (s5_s8,s5),(s8_s6,s8),(s7_s7,s7),
      (s6_s8,s6),(s8_s7,s8),(s7_s8,s7),
      (s8_s8,s8) }
 &
 t82 = {
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
      (s8_s2,s2),(s7_s3,s3),(s6_s4,s4),
      (s5_s5,s5),(s4_s6,s6),(s3_s7,s7),
      (s2_s8,s8),(s8_s3,s3),(s7_s4,s4),
      (s6_s5,s5),(s5_s6,s6),(s4_s7,s7),
      (s3_s8,s8),(s8_s4,s4),(s7_s5,s5),
      (s6_s6,s6),(s5_s7,s7),(s4_s8,s8),
      (s8_s5,s5),(s7_s6,s6),(s6_s7,s7),
      (s5_s8,s8),(s8_s6,s6),(s7_s7,s7),
      (s6_s8,s8),(s8_s7,s7),(s7_s8,s8),
      (s8_s8,s8) }
 &
 nextSection = {(s1,s2), (s2,s3), (s3,s4), (s4,s5), (s5,s6), (s6,s7), (s7,s8), (s8,s1)}
 &
 prevSection =  nextSection~
 &
  Qpred1 = {x | not(OnSameSection(x))}
//Qpred2 = {x | not(OnSameSection(x)) & not(OnAdjacentSections(x))}
END