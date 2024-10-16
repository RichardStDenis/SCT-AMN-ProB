// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBtrains_12_2

INCLUDES
 LBtrains_FB_12_2 /*@file "./LBtrains_FB_12_2.mch" */

SETS
 LocalStates = {s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12}

DEFINITIONS
 OnSameSection(x) ==
  t121(x) = t122(x);
 OnAdjacentSections(x) ==
  nextSection(t121(x)) = t122(x) or prevSection(t121(x)) = t122(x);

CONSTANTS
 t121, t122,
 nextSection, prevSection,
  Qpred1
//Qpred2

PROPERTIES
 t121 = {
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
      (s1_s10,s1),(s11_s1,s11),(s10_s2,s10),
      (s9_s3,s9),(s8_s4,s8),(s7_s5,s7),
      (s6_s6,s6),(s5_s7,s5),(s4_s8,s4),
      (s3_s9,s3),(s2_s10,s2),(s1_s11,s1),
      (s12_s1,s12),(s11_s2,s11),(s10_s3,s10),
      (s9_s4,s9),(s8_s5,s8),(s7_s6,s7),
      (s6_s7,s6),(s5_s8,s5),(s4_s9,s4),
      (s3_s10,s3),(s2_s11,s2),(s1_s12,s1),
      (s12_s2,s12),(s11_s3,s11),(s10_s4,s10),
      (s9_s5,s9),(s8_s6,s8),(s7_s7,s7),
      (s6_s8,s6),(s5_s9,s5),(s4_s10,s4),
      (s3_s11,s3),(s2_s12,s2),(s12_s3,s12),
      (s11_s4,s11),(s10_s5,s10),(s9_s6,s9),
      (s8_s7,s8),(s7_s8,s7),(s6_s9,s6),
      (s5_s10,s5),(s4_s11,s4),(s3_s12,s3),
      (s12_s4,s12),(s11_s5,s11),(s10_s6,s10),
      (s9_s7,s9),(s8_s8,s8),(s7_s9,s7),
      (s6_s10,s6),(s5_s11,s5),(s4_s12,s4),
      (s12_s5,s12),(s11_s6,s11),(s10_s7,s10),
      (s9_s8,s9),(s8_s9,s8),(s7_s10,s7),
      (s6_s11,s6),(s5_s12,s5),(s12_s6,s12),
      (s11_s7,s11),(s10_s8,s10),(s9_s9,s9),
      (s8_s10,s8),(s7_s11,s7),(s6_s12,s6),
      (s12_s7,s12),(s11_s8,s11),(s10_s9,s10),
      (s9_s10,s9),(s8_s11,s8),(s7_s12,s7),
      (s12_s8,s12),(s11_s9,s11),(s10_s10,s10),
      (s9_s11,s9),(s8_s12,s8),(s12_s9,s12),
      (s11_s10,s11),(s10_s11,s10),(s9_s12,s9),
      (s12_s10,s12),(s11_s11,s11),(s10_s12,s10),
      (s12_s11,s12),(s11_s12,s11),(s12_s12,s12) }
 &
 t122 = {
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
      (s1_s10,s10),(s11_s1,s1),(s10_s2,s2),
      (s9_s3,s3),(s8_s4,s4),(s7_s5,s5),
      (s6_s6,s6),(s5_s7,s7),(s4_s8,s8),
      (s3_s9,s9),(s2_s10,s10),(s1_s11,s11),
      (s12_s1,s1),(s11_s2,s2),(s10_s3,s3),
      (s9_s4,s4),(s8_s5,s5),(s7_s6,s6),
      (s6_s7,s7),(s5_s8,s8),(s4_s9,s9),
      (s3_s10,s10),(s2_s11,s11),(s1_s12,s12),
      (s12_s2,s2),(s11_s3,s3),(s10_s4,s4),
      (s9_s5,s5),(s8_s6,s6),(s7_s7,s7),
      (s6_s8,s8),(s5_s9,s9),(s4_s10,s10),
      (s3_s11,s11),(s2_s12,s12),(s12_s3,s3),
      (s11_s4,s4),(s10_s5,s5),(s9_s6,s6),
      (s8_s7,s7),(s7_s8,s8),(s6_s9,s9),
      (s5_s10,s10),(s4_s11,s11),(s3_s12,s12),
      (s12_s4,s4),(s11_s5,s5),(s10_s6,s6),
      (s9_s7,s7),(s8_s8,s8),(s7_s9,s9),
      (s6_s10,s10),(s5_s11,s11),(s4_s12,s12),
      (s12_s5,s5),(s11_s6,s6),(s10_s7,s7),
      (s9_s8,s8),(s8_s9,s9),(s7_s10,s10),
      (s6_s11,s11),(s5_s12,s12),(s12_s6,s6),
      (s11_s7,s7),(s10_s8,s8),(s9_s9,s9),
      (s8_s10,s10),(s7_s11,s11),(s6_s12,s12),
      (s12_s7,s7),(s11_s8,s8),(s10_s9,s9),
      (s9_s10,s10),(s8_s11,s11),(s7_s12,s12),
      (s12_s8,s8),(s11_s9,s9),(s10_s10,s10),
      (s9_s11,s11),(s8_s12,s12),(s12_s9,s9),
      (s11_s10,s10),(s10_s11,s11),(s9_s12,s12),
      (s12_s10,s10),(s11_s11,s11),(s10_s12,s12),
      (s12_s11,s11),(s11_s12,s12),(s12_s12,s12) }
 &
 nextSection = {(s1,s2), (s2,s3), (s3,s4), (s4,s5), (s5,s6), (s6,s7),
                (s7,s8), (s8,s9), (s9,s10), (s10,s11), (s11,s12), (s12,s1) }
 &
 prevSection =  nextSection~
 &
  Qpred1 = {x | not(OnSameSection(x))}
//Qpred2 = {x | not(OnSameSection(x)) & not(OnAdjacentSections(x))}
END