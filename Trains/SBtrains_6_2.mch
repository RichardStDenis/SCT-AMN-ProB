// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBtrains_6_2

INCLUDES
 LBtrains_FB_6_2 /*@file "./LBtrains_FB_6_2.mch" */

SETS
 LocalStates = {s1, s2, s3, s4, s5, s6}

DEFINITIONS
 OnSameSection(x) ==
  t61(x) = t62(x);
 OnAdjacentSections(x) ==
  nextSection(t61(x)) = t62(x) or prevSection(t61(x)) = t62(x);

CONSTANTS
 t61, t62,
 nextSection, prevSection,
  Qpred1
//Qpred2

PROPERTIES
 t61 = {
      (s1_s1,s1),(s2_s1,s2),(s1_s2,s1),
      (s3_s1,s3),(s2_s2,s2),(s1_s3,s1),
      (s4_s1,s4),(s3_s2,s3),(s2_s3,s2),
      (s1_s4,s1),(s5_s1,s5),(s4_s2,s4),
      (s3_s3,s3),(s2_s4,s2),(s1_s5,s1),
      (s6_s1,s6),(s5_s2,s5),(s4_s3,s4),
      (s3_s4,s3),(s2_s5,s2),(s1_s6,s1),
      (s6_s2,s6),(s5_s3,s5),(s4_s4,s4),
      (s3_s5,s3),(s2_s6,s2),(s6_s3,s6),
      (s5_s4,s5),(s4_s5,s4),(s3_s6,s3),
      (s6_s4,s6),(s5_s5,s5),(s4_s6,s4),
      (s6_s5,s6),(s5_s6,s5),(s6_s6,s6) }
 &
 t62 = {
      (s1_s1,s1),(s2_s1,s1),(s1_s2,s2),
      (s3_s1,s1),(s2_s2,s2),(s1_s3,s3),
      (s4_s1,s1),(s3_s2,s2),(s2_s3,s3),
      (s1_s4,s4),(s5_s1,s1),(s4_s2,s2),
      (s3_s3,s3),(s2_s4,s4),(s1_s5,s5),
      (s6_s1,s1),(s5_s2,s2),(s4_s3,s3),
      (s3_s4,s4),(s2_s5,s5),(s1_s6,s6),
      (s6_s2,s2),(s5_s3,s3),(s4_s4,s4),
      (s3_s5,s5),(s2_s6,s6),(s6_s3,s3),
      (s5_s4,s4),(s4_s5,s5),(s3_s6,s6),
      (s6_s4,s4),(s5_s5,s5),(s4_s6,s6),
      (s6_s5,s5),(s5_s6,s6),(s6_s6,s6) }
 &
 nextSection = {(s1,s2), (s2,s3), (s3,s4), (s4,s5), (s5,s6), (s6,s1)}
 &
 prevSection =  nextSection~
 &
  Qpred1 = {x | not(OnSameSection(x))}
//Qpred2 = {x | not(OnSameSection(x)) & not(OnAdjacentSections(x))}
END