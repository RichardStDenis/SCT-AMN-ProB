// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBtrains_4_2

INCLUDES
 LBtrains_FB_4_2 /*@file "./LBtrains_FB_4_2.mch" */

SETS
 LocalStates = {s1, s2, s3, s4}

DEFINITIONS
 OnSameSection(x) ==
  t41(x) = t42(x);
 OnAdjacentSections(x) ==
  nextSection(t41(x)) = t42(x) or prevSection(t41(x)) = t42(x);

CONSTANTS
 t41, t42,
 nextSection, prevSection,
  Qpred1
//Qpred2

PROPERTIES
 t41 = {
      (s1_s1,s1),(s2_s1,s2),(s1_s2,s1),
      (s3_s1,s3),(s2_s2,s2),(s1_s3,s1),
      (s4_s1,s4),(s3_s2,s3),(s2_s3,s2),
      (s1_s4,s1),(s4_s2,s4),(s3_s3,s3),
      (s2_s4,s2),(s4_s3,s4),(s3_s4,s3),
      (s4_s4,s4) }
 &
 t42 = {
      (s1_s1,s1),(s2_s1,s1),(s1_s2,s2),
      (s3_s1,s1),(s2_s2,s2),(s1_s3,s3),
      (s4_s1,s1),(s3_s2,s2),(s2_s3,s3),
      (s1_s4,s4),(s4_s2,s2),(s3_s3,s3),
      (s2_s4,s4),(s4_s3,s3),(s3_s4,s4),
      (s4_s4,s4) }
 &
 nextSection = {(s1,s2), (s2,s3), (s3,s4), (s4,s1)}
 &
 prevSection =  nextSection~
 &
  Qpred1 = {x | not(OnSameSection(x))}
//Qpred2 = {x | not(OnSameSection(x)) & not(OnAdjacentSections(x))}
END