// copyright Richard St_Denis, Universite de Sherbrooke, 2023.
MODEL LBtrains_S_12_3

SETS
 States = {s1_s1_s1, s2_s1_s1, s1_s1_s2,
           s1_s2_s1, s3_s1_s1, s2_s1_s2,
           s2_s2_s1, s1_s1_s3, s1_s2_s2,
           s1_s3_s1, s4_s1_s1, s3_s1_s2,
           s3_s2_s1, s2_s1_s3, s2_s2_s2,
           s2_s3_s1, s1_s1_s4, s1_s2_s3,
           s1_s3_s2, s1_s4_s1, s5_s1_s1,
           s4_s1_s2, s4_s2_s1, s3_s1_s3,
           s3_s2_s2, s3_s3_s1, s2_s1_s4,
           s2_s2_s3, s2_s3_s2, s2_s4_s1,
           s1_s1_s5, s1_s2_s4, s1_s3_s3,
           s1_s4_s2, s1_s5_s1, s6_s1_s1,
           s5_s1_s2, s5_s2_s1, s4_s1_s3,
           s4_s2_s2, s4_s3_s1, s3_s1_s4,
           s3_s2_s3, s3_s3_s2, s3_s4_s1,
           s2_s1_s5, s2_s2_s4, s2_s3_s3,
           s2_s4_s2, s2_s5_s1, s1_s1_s6,
           s1_s2_s5, s1_s3_s4, s1_s4_s3,
           s1_s5_s2, s1_s6_s1, s7_s1_s1,
           s6_s1_s2, s6_s2_s1, s5_s1_s3,
           s5_s2_s2, s5_s3_s1, s4_s1_s4,
           s4_s2_s3, s4_s3_s2, s4_s4_s1,
           s3_s1_s5, s3_s2_s4, s3_s3_s3,
           s3_s4_s2, s3_s5_s1, s2_s1_s6,
           s2_s2_s5, s2_s3_s4, s2_s4_s3,
           s2_s5_s2, s2_s6_s1, s1_s1_s7,
           s1_s2_s6, s1_s3_s5, s1_s4_s4,
           s1_s5_s3, s1_s6_s2, s1_s7_s1,
           s8_s1_s1, s7_s1_s2, s7_s2_s1,
           s6_s1_s3, s6_s2_s2, s6_s3_s1,
           s5_s1_s4, s5_s2_s3, s5_s3_s2,
           s5_s4_s1, s4_s1_s5, s4_s2_s4,
           s4_s3_s3, s4_s4_s2, s4_s5_s1,
           s3_s1_s6, s3_s2_s5, s3_s3_s4,
           s3_s4_s3, s3_s5_s2, s3_s6_s1,
           s2_s1_s7, s2_s2_s6, s2_s3_s5,
           s2_s4_s4, s2_s5_s3, s2_s6_s2,
           s2_s7_s1, s1_s1_s8, s1_s2_s7,
           s1_s3_s6, s1_s4_s5, s1_s5_s4,
           s1_s6_s3, s1_s7_s2, s1_s8_s1,
           s9_s1_s1, s8_s1_s2, s8_s2_s1,
           s7_s1_s3, s7_s2_s2, s7_s3_s1,
           s6_s1_s4, s6_s2_s3, s6_s3_s2,
           s6_s4_s1, s5_s1_s5, s5_s2_s4,
           s5_s3_s3, s5_s4_s2, s5_s5_s1,
           s4_s1_s6, s4_s2_s5, s4_s3_s4,
           s4_s4_s3, s4_s5_s2, s4_s6_s1,
           s3_s1_s7, s3_s2_s6, s3_s3_s5,
           s3_s4_s4, s3_s5_s3, s3_s6_s2,
           s3_s7_s1, s2_s1_s8, s2_s2_s7,
           s2_s3_s6, s2_s4_s5, s2_s5_s4,
           s2_s6_s3, s2_s7_s2, s2_s8_s1,
           s1_s1_s9, s1_s2_s8, s1_s3_s7,
           s1_s4_s6, s1_s5_s5, s1_s6_s4,
           s1_s7_s3, s1_s8_s2, s1_s9_s1,
           s10_s1_s1, s9_s1_s2, s9_s2_s1,
           s8_s1_s3, s8_s2_s2, s8_s3_s1,
           s7_s1_s4, s7_s2_s3, s7_s3_s2,
           s7_s4_s1, s6_s1_s5, s6_s2_s4,
           s6_s3_s3, s6_s4_s2, s6_s5_s1,
           s5_s1_s6, s5_s2_s5, s5_s3_s4,
           s5_s4_s3, s5_s5_s2, s5_s6_s1,
           s4_s1_s7, s4_s2_s6, s4_s3_s5,
           s4_s4_s4, s4_s5_s3, s4_s6_s2,
           s4_s7_s1, s3_s1_s8, s3_s2_s7,
           s3_s3_s6, s3_s4_s5, s3_s5_s4,
           s3_s6_s3, s3_s7_s2, s3_s8_s1,
           s2_s1_s9, s2_s2_s8, s2_s3_s7,
           s2_s4_s6, s2_s5_s5, s2_s6_s4,
           s2_s7_s3, s2_s8_s2, s2_s9_s1,
           s1_s1_s10, s1_s2_s9, s1_s3_s8,
           s1_s4_s7, s1_s5_s6, s1_s6_s5,
           s1_s7_s4, s1_s8_s3, s1_s9_s2,
           s1_s10_s1, s11_s1_s1, s10_s1_s2,
           s10_s2_s1, s9_s1_s3, s9_s2_s2,
           s9_s3_s1, s8_s1_s4, s8_s2_s3,
           s8_s3_s2, s8_s4_s1, s7_s1_s5,
           s7_s2_s4, s7_s3_s3, s7_s4_s2,
           s7_s5_s1, s6_s1_s6, s6_s2_s5,
           s6_s3_s4, s6_s4_s3, s6_s5_s2,
           s6_s6_s1, s5_s1_s7, s5_s2_s6,
           s5_s3_s5, s5_s4_s4, s5_s5_s3,
           s5_s6_s2, s5_s7_s1, s4_s1_s8,
           s4_s2_s7, s4_s3_s6, s4_s4_s5,
           s4_s5_s4, s4_s6_s3, s4_s7_s2,
           s4_s8_s1, s3_s1_s9, s3_s2_s8,
           s3_s3_s7, s3_s4_s6, s3_s5_s5,
           s3_s6_s4, s3_s7_s3, s3_s8_s2,
           s3_s9_s1, s2_s1_s10, s2_s2_s9,
           s2_s3_s8, s2_s4_s7, s2_s5_s6,
           s2_s6_s5, s2_s7_s4, s2_s8_s3,
           s2_s9_s2, s2_s10_s1, s1_s1_s11,
           s1_s2_s10, s1_s3_s9, s1_s4_s8,
           s1_s5_s7, s1_s6_s6, s1_s7_s5,
           s1_s8_s4, s1_s9_s3, s1_s10_s2,
           s1_s11_s1, s12_s1_s1, s11_s1_s2,
           s11_s2_s1, s10_s1_s3, s10_s2_s2,
           s10_s3_s1, s9_s1_s4, s9_s2_s3,
           s9_s3_s2, s9_s4_s1, s8_s1_s5,
           s8_s2_s4, s8_s3_s3, s8_s4_s2,
           s8_s5_s1, s7_s1_s6, s7_s2_s5,
           s7_s3_s4, s7_s4_s3, s7_s5_s2,
           s7_s6_s1, s6_s1_s7, s6_s2_s6,
           s6_s3_s5, s6_s4_s4, s6_s5_s3,
           s6_s6_s2, s6_s7_s1, s5_s1_s8,
           s5_s2_s7, s5_s3_s6, s5_s4_s5,
           s5_s5_s4, s5_s6_s3, s5_s7_s2,
           s5_s8_s1, s4_s1_s9, s4_s2_s8,
           s4_s3_s7, s4_s4_s6, s4_s5_s5,
           s4_s6_s4, s4_s7_s3, s4_s8_s2,
           s4_s9_s1, s3_s1_s10, s3_s2_s9,
           s3_s3_s8, s3_s4_s7, s3_s5_s6,
           s3_s6_s5, s3_s7_s4, s3_s8_s3,
           s3_s9_s2, s3_s10_s1, s2_s1_s11,
           s2_s2_s10, s2_s3_s9, s2_s4_s8,
           s2_s5_s7, s2_s6_s6, s2_s7_s5,
           s2_s8_s4, s2_s9_s3, s2_s10_s2,
           s2_s11_s1, s1_s1_s12, s1_s2_s11,
           s1_s3_s10, s1_s4_s9, s1_s5_s8,
           s1_s6_s7, s1_s7_s6, s1_s8_s5,
           s1_s9_s4, s1_s10_s3, s1_s11_s2,
           s1_s12_s1, s12_s1_s2, s12_s2_s1,
           s11_s1_s3, s11_s2_s2, s11_s3_s1,
           s10_s1_s4, s10_s2_s3, s10_s3_s2,
           s10_s4_s1, s9_s1_s5, s9_s2_s4,
           s9_s3_s3, s9_s4_s2, s9_s5_s1,
           s8_s1_s6, s8_s2_s5, s8_s3_s4,
           s8_s4_s3, s8_s5_s2, s8_s6_s1,
           s7_s1_s7, s7_s2_s6, s7_s3_s5,
           s7_s4_s4, s7_s5_s3, s7_s6_s2,
           s7_s7_s1, s6_s1_s8, s6_s2_s7,
           s6_s3_s6, s6_s4_s5, s6_s5_s4,
           s6_s6_s3, s6_s7_s2, s6_s8_s1,
           s5_s1_s9, s5_s2_s8, s5_s3_s7,
           s5_s4_s6, s5_s5_s5, s5_s6_s4,
           s5_s7_s3, s5_s8_s2, s5_s9_s1,
           s4_s1_s10, s4_s2_s9, s4_s3_s8,
           s4_s4_s7, s4_s5_s6, s4_s6_s5,
           s4_s7_s4, s4_s8_s3, s4_s9_s2,
           s4_s10_s1, s3_s1_s11, s3_s2_s10,
           s3_s3_s9, s3_s4_s8, s3_s5_s7,
           s3_s6_s6, s3_s7_s5, s3_s8_s4,
           s3_s9_s3, s3_s10_s2, s3_s11_s1,
           s2_s1_s12, s2_s2_s11, s2_s3_s10,
           s2_s4_s9, s2_s5_s8, s2_s6_s7,
           s2_s7_s6, s2_s8_s5, s2_s9_s4,
           s2_s10_s3, s2_s11_s2, s2_s12_s1,
           s1_s2_s12, s1_s3_s11, s1_s4_s10,
           s1_s5_s9, s1_s6_s8, s1_s7_s7,
           s1_s8_s6, s1_s9_s5, s1_s10_s4,
           s1_s11_s3, s1_s12_s2, s12_s1_s3,
           s12_s2_s2, s12_s3_s1, s11_s1_s4,
           s11_s2_s3, s11_s3_s2, s11_s4_s1,
           s10_s1_s5, s10_s2_s4, s10_s3_s3,
           s10_s4_s2, s10_s5_s1, s9_s1_s6,
           s9_s2_s5, s9_s3_s4, s9_s4_s3,
           s9_s5_s2, s9_s6_s1, s8_s1_s7,
           s8_s2_s6, s8_s3_s5, s8_s4_s4,
           s8_s5_s3, s8_s6_s2, s8_s7_s1,
           s7_s1_s8, s7_s2_s7, s7_s3_s6,
           s7_s4_s5, s7_s5_s4, s7_s6_s3,
           s7_s7_s2, s7_s8_s1, s6_s1_s9,
           s6_s2_s8, s6_s3_s7, s6_s4_s6,
           s6_s5_s5, s6_s6_s4, s6_s7_s3,
           s6_s8_s2, s6_s9_s1, s5_s1_s10,
           s5_s2_s9, s5_s3_s8, s5_s4_s7,
           s5_s5_s6, s5_s6_s5, s5_s7_s4,
           s5_s8_s3, s5_s9_s2, s5_s10_s1,
           s4_s1_s11, s4_s2_s10, s4_s3_s9,
           s4_s4_s8, s4_s5_s7, s4_s6_s6,
           s4_s7_s5, s4_s8_s4, s4_s9_s3,
           s4_s10_s2, s4_s11_s1, s3_s1_s12,
           s3_s2_s11, s3_s3_s10, s3_s4_s9,
           s3_s5_s8, s3_s6_s7, s3_s7_s6,
           s3_s8_s5, s3_s9_s4, s3_s10_s3,
           s3_s11_s2, s3_s12_s1, s2_s2_s12,
           s2_s3_s11, s2_s4_s10, s2_s5_s9,
           s2_s6_s8, s2_s7_s7, s2_s8_s6,
           s2_s9_s5, s2_s10_s4, s2_s11_s3,
           s2_s12_s2, s1_s3_s12, s1_s4_s11,
           s1_s5_s10, s1_s6_s9, s1_s7_s8,
           s1_s8_s7, s1_s9_s6, s1_s10_s5,
           s1_s11_s4, s1_s12_s3, s12_s1_s4,
           s12_s2_s3, s12_s3_s2, s12_s4_s1,
           s11_s1_s5, s11_s2_s4, s11_s3_s3,
           s11_s4_s2, s11_s5_s1, s10_s1_s6,
           s10_s2_s5, s10_s3_s4, s10_s4_s3,
           s10_s5_s2, s10_s6_s1, s9_s1_s7,
           s9_s2_s6, s9_s3_s5, s9_s4_s4,
           s9_s5_s3, s9_s6_s2, s9_s7_s1,
           s8_s1_s8, s8_s2_s7, s8_s3_s6,
           s8_s4_s5, s8_s5_s4, s8_s6_s3,
           s8_s7_s2, s8_s8_s1, s7_s1_s9,
           s7_s2_s8, s7_s3_s7, s7_s4_s6,
           s7_s5_s5, s7_s6_s4, s7_s7_s3,
           s7_s8_s2, s7_s9_s1, s6_s1_s10,
           s6_s2_s9, s6_s3_s8, s6_s4_s7,
           s6_s5_s6, s6_s6_s5, s6_s7_s4,
           s6_s8_s3, s6_s9_s2, s6_s10_s1,
           s5_s1_s11, s5_s2_s10, s5_s3_s9,
           s5_s4_s8, s5_s5_s7, s5_s6_s6,
           s5_s7_s5, s5_s8_s4, s5_s9_s3,
           s5_s10_s2, s5_s11_s1, s4_s1_s12,
           s4_s2_s11, s4_s3_s10, s4_s4_s9,
           s4_s5_s8, s4_s6_s7, s4_s7_s6,
           s4_s8_s5, s4_s9_s4, s4_s10_s3,
           s4_s11_s2, s4_s12_s1, s3_s2_s12,
           s3_s3_s11, s3_s4_s10, s3_s5_s9,
           s3_s6_s8, s3_s7_s7, s3_s8_s6,
           s3_s9_s5, s3_s10_s4, s3_s11_s3,
           s3_s12_s2, s2_s3_s12, s2_s4_s11,
           s2_s5_s10, s2_s6_s9, s2_s7_s8,
           s2_s8_s7, s2_s9_s6, s2_s10_s5,
           s2_s11_s4, s2_s12_s3, s1_s4_s12,
           s1_s5_s11, s1_s6_s10, s1_s7_s9,
           s1_s8_s8, s1_s9_s7, s1_s10_s6,
           s1_s11_s5, s1_s12_s4, s12_s1_s5,
           s12_s2_s4, s12_s3_s3, s12_s4_s2,
           s12_s5_s1, s11_s1_s6, s11_s2_s5,
           s11_s3_s4, s11_s4_s3, s11_s5_s2,
           s11_s6_s1, s10_s1_s7, s10_s2_s6,
           s10_s3_s5, s10_s4_s4, s10_s5_s3,
           s10_s6_s2, s10_s7_s1, s9_s1_s8,
           s9_s2_s7, s9_s3_s6, s9_s4_s5,
           s9_s5_s4, s9_s6_s3, s9_s7_s2,
           s9_s8_s1, s8_s1_s9, s8_s2_s8,
           s8_s3_s7, s8_s4_s6, s8_s5_s5,
           s8_s6_s4, s8_s7_s3, s8_s8_s2,
           s8_s9_s1, s7_s1_s10, s7_s2_s9,
           s7_s3_s8, s7_s4_s7, s7_s5_s6,
           s7_s6_s5, s7_s7_s4, s7_s8_s3,
           s7_s9_s2, s7_s10_s1, s6_s1_s11,
           s6_s2_s10, s6_s3_s9, s6_s4_s8,
           s6_s5_s7, s6_s6_s6, s6_s7_s5,
           s6_s8_s4, s6_s9_s3, s6_s10_s2,
           s6_s11_s1, s5_s1_s12, s5_s2_s11,
           s5_s3_s10, s5_s4_s9, s5_s5_s8,
           s5_s6_s7, s5_s7_s6, s5_s8_s5,
           s5_s9_s4, s5_s10_s3, s5_s11_s2,
           s5_s12_s1, s4_s2_s12, s4_s3_s11,
           s4_s4_s10, s4_s5_s9, s4_s6_s8,
           s4_s7_s7, s4_s8_s6, s4_s9_s5,
           s4_s10_s4, s4_s11_s3, s4_s12_s2,
           s3_s3_s12, s3_s4_s11, s3_s5_s10,
           s3_s6_s9, s3_s7_s8, s3_s8_s7,
           s3_s9_s6, s3_s10_s5, s3_s11_s4,
           s3_s12_s3, s2_s4_s12, s2_s5_s11,
           s2_s6_s10, s2_s7_s9, s2_s8_s8,
           s2_s9_s7, s2_s10_s6, s2_s11_s5,
           s2_s12_s4, s1_s5_s12, s1_s6_s11,
           s1_s7_s10, s1_s8_s9, s1_s9_s8,
           s1_s10_s7, s1_s11_s6, s1_s12_s5,
           s12_s1_s6, s12_s2_s5, s12_s3_s4,
           s12_s4_s3, s12_s5_s2, s12_s6_s1,
           s11_s1_s7, s11_s2_s6, s11_s3_s5,
           s11_s4_s4, s11_s5_s3, s11_s6_s2,
           s11_s7_s1, s10_s1_s8, s10_s2_s7,
           s10_s3_s6, s10_s4_s5, s10_s5_s4,
           s10_s6_s3, s10_s7_s2, s10_s8_s1,
           s9_s1_s9, s9_s2_s8, s9_s3_s7,
           s9_s4_s6, s9_s5_s5, s9_s6_s4,
           s9_s7_s3, s9_s8_s2, s9_s9_s1,
           s8_s1_s10, s8_s2_s9, s8_s3_s8,
           s8_s4_s7, s8_s5_s6, s8_s6_s5,
           s8_s7_s4, s8_s8_s3, s8_s9_s2,
           s8_s10_s1, s7_s1_s11, s7_s2_s10,
           s7_s3_s9, s7_s4_s8, s7_s5_s7,
           s7_s6_s6, s7_s7_s5, s7_s8_s4,
           s7_s9_s3, s7_s10_s2, s7_s11_s1,
           s6_s1_s12, s6_s2_s11, s6_s3_s10,
           s6_s4_s9, s6_s5_s8, s6_s6_s7,
           s6_s7_s6, s6_s8_s5, s6_s9_s4,
           s6_s10_s3, s6_s11_s2, s6_s12_s1,
           s5_s2_s12, s5_s3_s11, s5_s4_s10,
           s5_s5_s9, s5_s6_s8, s5_s7_s7,
           s5_s8_s6, s5_s9_s5, s5_s10_s4,
           s5_s11_s3, s5_s12_s2, s4_s3_s12,
           s4_s4_s11, s4_s5_s10, s4_s6_s9,
           s4_s7_s8, s4_s8_s7, s4_s9_s6,
           s4_s10_s5, s4_s11_s4, s4_s12_s3,
           s3_s4_s12, s3_s5_s11, s3_s6_s10,
           s3_s7_s9, s3_s8_s8, s3_s9_s7,
           s3_s10_s6, s3_s11_s5, s3_s12_s4,
           s2_s5_s12, s2_s6_s11, s2_s7_s10,
           s2_s8_s9, s2_s9_s8, s2_s10_s7,
           s2_s11_s6, s2_s12_s5, s1_s6_s12,
           s1_s7_s11, s1_s8_s10, s1_s9_s9,
           s1_s10_s8, s1_s11_s7, s1_s12_s6,
           s12_s1_s7, s12_s2_s6, s12_s3_s5,
           s12_s4_s4, s12_s5_s3, s12_s6_s2,
           s12_s7_s1, s11_s1_s8, s11_s2_s7,
           s11_s3_s6, s11_s4_s5, s11_s5_s4,
           s11_s6_s3, s11_s7_s2, s11_s8_s1,
           s10_s1_s9, s10_s2_s8, s10_s3_s7,
           s10_s4_s6, s10_s5_s5, s10_s6_s4,
           s10_s7_s3, s10_s8_s2, s10_s9_s1,
           s9_s1_s10, s9_s2_s9, s9_s3_s8,
           s9_s4_s7, s9_s5_s6, s9_s6_s5,
           s9_s7_s4, s9_s8_s3, s9_s9_s2,
           s9_s10_s1, s8_s1_s11, s8_s2_s10,
           s8_s3_s9, s8_s4_s8, s8_s5_s7,
           s8_s6_s6, s8_s7_s5, s8_s8_s4,
           s8_s9_s3, s8_s10_s2, s8_s11_s1,
           s7_s1_s12, s7_s2_s11, s7_s3_s10,
           s7_s4_s9, s7_s5_s8, s7_s6_s7,
           s7_s7_s6, s7_s8_s5, s7_s9_s4,
           s7_s10_s3, s7_s11_s2, s7_s12_s1,
           s6_s2_s12, s6_s3_s11, s6_s4_s10,
           s6_s5_s9, s6_s6_s8, s6_s7_s7,
           s6_s8_s6, s6_s9_s5, s6_s10_s4,
           s6_s11_s3, s6_s12_s2, s5_s3_s12,
           s5_s4_s11, s5_s5_s10, s5_s6_s9,
           s5_s7_s8, s5_s8_s7, s5_s9_s6,
           s5_s10_s5, s5_s11_s4, s5_s12_s3,
           s4_s4_s12, s4_s5_s11, s4_s6_s10,
           s4_s7_s9, s4_s8_s8, s4_s9_s7,
           s4_s10_s6, s4_s11_s5, s4_s12_s4,
           s3_s5_s12, s3_s6_s11, s3_s7_s10,
           s3_s8_s9, s3_s9_s8, s3_s10_s7,
           s3_s11_s6, s3_s12_s5, s2_s6_s12,
           s2_s7_s11, s2_s8_s10, s2_s9_s9,
           s2_s10_s8, s2_s11_s7, s2_s12_s6,
           s1_s7_s12, s1_s8_s11, s1_s9_s10,
           s1_s10_s9, s1_s11_s8, s1_s12_s7,
           s12_s1_s8, s12_s2_s7, s12_s3_s6,
           s12_s4_s5, s12_s5_s4, s12_s6_s3,
           s12_s7_s2, s12_s8_s1, s11_s1_s9,
           s11_s2_s8, s11_s3_s7, s11_s4_s6,
           s11_s5_s5, s11_s6_s4, s11_s7_s3,
           s11_s8_s2, s11_s9_s1, s10_s1_s10,
           s10_s2_s9, s10_s3_s8, s10_s4_s7,
           s10_s5_s6, s10_s6_s5, s10_s7_s4,
           s10_s8_s3, s10_s9_s2, s10_s10_s1,
           s9_s1_s11, s9_s2_s10, s9_s3_s9,
           s9_s4_s8, s9_s5_s7, s9_s6_s6,
           s9_s7_s5, s9_s8_s4, s9_s9_s3,
           s9_s10_s2, s9_s11_s1, s8_s1_s12,
           s8_s2_s11, s8_s3_s10, s8_s4_s9,
           s8_s5_s8, s8_s6_s7, s8_s7_s6,
           s8_s8_s5, s8_s9_s4, s8_s10_s3,
           s8_s11_s2, s8_s12_s1, s7_s2_s12,
           s7_s3_s11, s7_s4_s10, s7_s5_s9,
           s7_s6_s8, s7_s7_s7, s7_s8_s6,
           s7_s9_s5, s7_s10_s4, s7_s11_s3,
           s7_s12_s2, s6_s3_s12, s6_s4_s11,
           s6_s5_s10, s6_s6_s9, s6_s7_s8,
           s6_s8_s7, s6_s9_s6, s6_s10_s5,
           s6_s11_s4, s6_s12_s3, s5_s4_s12,
           s5_s5_s11, s5_s6_s10, s5_s7_s9,
           s5_s8_s8, s5_s9_s7, s5_s10_s6,
           s5_s11_s5, s5_s12_s4, s4_s5_s12,
           s4_s6_s11, s4_s7_s10, s4_s8_s9,
           s4_s9_s8, s4_s10_s7, s4_s11_s6,
           s4_s12_s5, s3_s6_s12, s3_s7_s11,
           s3_s8_s10, s3_s9_s9, s3_s10_s8,
           s3_s11_s7, s3_s12_s6, s2_s7_s12,
           s2_s8_s11, s2_s9_s10, s2_s10_s9,
           s2_s11_s8, s2_s12_s7, s1_s8_s12,
           s1_s9_s11, s1_s10_s10, s1_s11_s9,
           s1_s12_s8, s12_s1_s9, s12_s2_s8,
           s12_s3_s7, s12_s4_s6, s12_s5_s5,
           s12_s6_s4, s12_s7_s3, s12_s8_s2,
           s12_s9_s1, s11_s1_s10, s11_s2_s9,
           s11_s3_s8, s11_s4_s7, s11_s5_s6,
           s11_s6_s5, s11_s7_s4, s11_s8_s3,
           s11_s9_s2, s11_s10_s1, s10_s1_s11,
           s10_s2_s10, s10_s3_s9, s10_s4_s8,
           s10_s5_s7, s10_s6_s6, s10_s7_s5,
           s10_s8_s4, s10_s9_s3, s10_s10_s2,
           s10_s11_s1, s9_s1_s12, s9_s2_s11,
           s9_s3_s10, s9_s4_s9, s9_s5_s8,
           s9_s6_s7, s9_s7_s6, s9_s8_s5,
           s9_s9_s4, s9_s10_s3, s9_s11_s2,
           s9_s12_s1, s8_s2_s12, s8_s3_s11,
           s8_s4_s10, s8_s5_s9, s8_s6_s8,
           s8_s7_s7, s8_s8_s6, s8_s9_s5,
           s8_s10_s4, s8_s11_s3, s8_s12_s2,
           s7_s3_s12, s7_s4_s11, s7_s5_s10,
           s7_s6_s9, s7_s7_s8, s7_s8_s7,
           s7_s9_s6, s7_s10_s5, s7_s11_s4,
           s7_s12_s3, s6_s4_s12, s6_s5_s11,
           s6_s6_s10, s6_s7_s9, s6_s8_s8,
           s6_s9_s7, s6_s10_s6, s6_s11_s5,
           s6_s12_s4, s5_s5_s12, s5_s6_s11,
           s5_s7_s10, s5_s8_s9, s5_s9_s8,
           s5_s10_s7, s5_s11_s6, s5_s12_s5,
           s4_s6_s12, s4_s7_s11, s4_s8_s10,
           s4_s9_s9, s4_s10_s8, s4_s11_s7,
           s4_s12_s6, s3_s7_s12, s3_s8_s11,
           s3_s9_s10, s3_s10_s9, s3_s11_s8,
           s3_s12_s7, s2_s8_s12, s2_s9_s11,
           s2_s10_s10, s2_s11_s9, s2_s12_s8,
           s1_s9_s12, s1_s10_s11, s1_s11_s10,
           s1_s12_s9, s12_s1_s10, s12_s2_s9,
           s12_s3_s8, s12_s4_s7, s12_s5_s6,
           s12_s6_s5, s12_s7_s4, s12_s8_s3,
           s12_s9_s2, s12_s10_s1, s11_s1_s11,
           s11_s2_s10, s11_s3_s9, s11_s4_s8,
           s11_s5_s7, s11_s6_s6, s11_s7_s5,
           s11_s8_s4, s11_s9_s3, s11_s10_s2,
           s11_s11_s1, s10_s1_s12, s10_s2_s11,
           s10_s3_s10, s10_s4_s9, s10_s5_s8,
           s10_s6_s7, s10_s7_s6, s10_s8_s5,
           s10_s9_s4, s10_s10_s3, s10_s11_s2,
           s10_s12_s1, s9_s2_s12, s9_s3_s11,
           s9_s4_s10, s9_s5_s9, s9_s6_s8,
           s9_s7_s7, s9_s8_s6, s9_s9_s5,
           s9_s10_s4, s9_s11_s3, s9_s12_s2,
           s8_s3_s12, s8_s4_s11, s8_s5_s10,
           s8_s6_s9, s8_s7_s8, s8_s8_s7,
           s8_s9_s6, s8_s10_s5, s8_s11_s4,
           s8_s12_s3, s7_s4_s12, s7_s5_s11,
           s7_s6_s10, s7_s7_s9, s7_s8_s8,
           s7_s9_s7, s7_s10_s6, s7_s11_s5,
           s7_s12_s4, s6_s5_s12, s6_s6_s11,
           s6_s7_s10, s6_s8_s9, s6_s9_s8,
           s6_s10_s7, s6_s11_s6, s6_s12_s5,
           s5_s6_s12, s5_s7_s11, s5_s8_s10,
           s5_s9_s9, s5_s10_s8, s5_s11_s7,
           s5_s12_s6, s4_s7_s12, s4_s8_s11,
           s4_s9_s10, s4_s10_s9, s4_s11_s8,
           s4_s12_s7, s3_s8_s12, s3_s9_s11,
           s3_s10_s10, s3_s11_s9, s3_s12_s8,
           s2_s9_s12, s2_s10_s11, s2_s11_s10,
           s2_s12_s9, s1_s10_s12, s1_s11_s11,
           s1_s12_s10, s12_s1_s11, s12_s2_s10,
           s12_s3_s9, s12_s4_s8, s12_s5_s7,
           s12_s6_s6, s12_s7_s5, s12_s8_s4,
           s12_s9_s3, s12_s10_s2, s12_s11_s1,
           s11_s1_s12, s11_s2_s11, s11_s3_s10,
           s11_s4_s9, s11_s5_s8, s11_s6_s7,
           s11_s7_s6, s11_s8_s5, s11_s9_s4,
           s11_s10_s3, s11_s11_s2, s11_s12_s1,
           s10_s2_s12, s10_s3_s11, s10_s4_s10,
           s10_s5_s9, s10_s6_s8, s10_s7_s7,
           s10_s8_s6, s10_s9_s5, s10_s10_s4,
           s10_s11_s3, s10_s12_s2, s9_s3_s12,
           s9_s4_s11, s9_s5_s10, s9_s6_s9,
           s9_s7_s8, s9_s8_s7, s9_s9_s6,
           s9_s10_s5, s9_s11_s4, s9_s12_s3,
           s8_s4_s12, s8_s5_s11, s8_s6_s10,
           s8_s7_s9, s8_s8_s8, s8_s9_s7,
           s8_s10_s6, s8_s11_s5, s8_s12_s4,
           s7_s5_s12, s7_s6_s11, s7_s7_s10,
           s7_s8_s9, s7_s9_s8, s7_s10_s7,
           s7_s11_s6, s7_s12_s5, s6_s6_s12,
           s6_s7_s11, s6_s8_s10, s6_s9_s9,
           s6_s10_s8, s6_s11_s7, s6_s12_s6,
           s5_s7_s12, s5_s8_s11, s5_s9_s10,
           s5_s10_s9, s5_s11_s8, s5_s12_s7,
           s4_s8_s12, s4_s9_s11, s4_s10_s10,
           s4_s11_s9, s4_s12_s8, s3_s9_s12,
           s3_s10_s11, s3_s11_s10, s3_s12_s9,
           s2_s10_s12, s2_s11_s11, s2_s12_s10,
           s1_s11_s12, s1_s12_s11, s12_s1_s12,
           s12_s2_s11, s12_s3_s10, s12_s4_s9,
           s12_s5_s8, s12_s6_s7, s12_s7_s6,
           s12_s8_s5, s12_s9_s4, s12_s10_s3,
           s12_s11_s2, s12_s12_s1, s11_s2_s12,
           s11_s3_s11, s11_s4_s10, s11_s5_s9,
           s11_s6_s8, s11_s7_s7, s11_s8_s6,
           s11_s9_s5, s11_s10_s4, s11_s11_s3,
           s11_s12_s2, s10_s3_s12, s10_s4_s11,
           s10_s5_s10, s10_s6_s9, s10_s7_s8,
           s10_s8_s7, s10_s9_s6, s10_s10_s5,
           s10_s11_s4, s10_s12_s3, s9_s4_s12,
           s9_s5_s11, s9_s6_s10, s9_s7_s9,
           s9_s8_s8, s9_s9_s7, s9_s10_s6,
           s9_s11_s5, s9_s12_s4, s8_s5_s12,
           s8_s6_s11, s8_s7_s10, s8_s8_s9,
           s8_s9_s8, s8_s10_s7, s8_s11_s6,
           s8_s12_s5, s7_s6_s12, s7_s7_s11,
           s7_s8_s10, s7_s9_s9, s7_s10_s8,
           s7_s11_s7, s7_s12_s6, s6_s7_s12,
           s6_s8_s11, s6_s9_s10, s6_s10_s9,
           s6_s11_s8, s6_s12_s7, s5_s8_s12,
           s5_s9_s11, s5_s10_s10, s5_s11_s9,
           s5_s12_s8, s4_s9_s12, s4_s10_s11,
           s4_s11_s10, s4_s12_s9, s3_s10_s12,
           s3_s11_s11, s3_s12_s10, s2_s11_s12,
           s2_s12_s11, s1_s12_s12, s12_s2_s12,
           s12_s3_s11, s12_s4_s10, s12_s5_s9,
           s12_s6_s8, s12_s7_s7, s12_s8_s6,
           s12_s9_s5, s12_s10_s4, s12_s11_s3,
           s12_s12_s2, s11_s3_s12, s11_s4_s11,
           s11_s5_s10, s11_s6_s9, s11_s7_s8,
           s11_s8_s7, s11_s9_s6, s11_s10_s5,
           s11_s11_s4, s11_s12_s3, s10_s4_s12,
           s10_s5_s11, s10_s6_s10, s10_s7_s9,
           s10_s8_s8, s10_s9_s7, s10_s10_s6,
           s10_s11_s5, s10_s12_s4, s9_s5_s12,
           s9_s6_s11, s9_s7_s10, s9_s8_s9,
           s9_s9_s8, s9_s10_s7, s9_s11_s6,
           s9_s12_s5, s8_s6_s12, s8_s7_s11,
           s8_s8_s10, s8_s9_s9, s8_s10_s8,
           s8_s11_s7, s8_s12_s6, s7_s7_s12,
           s7_s8_s11, s7_s9_s10, s7_s10_s9,
           s7_s11_s8, s7_s12_s7, s6_s8_s12,
           s6_s9_s11, s6_s10_s10, s6_s11_s9,
           s6_s12_s8, s5_s9_s12, s5_s10_s11,
           s5_s11_s10, s5_s12_s9, s4_s10_s12,
           s4_s11_s11, s4_s12_s10, s3_s11_s12,
           s3_s12_s11, s2_s12_s12, s12_s3_s12,
           s12_s4_s11, s12_s5_s10, s12_s6_s9,
           s12_s7_s8, s12_s8_s7, s12_s9_s6,
           s12_s10_s5, s12_s11_s4, s12_s12_s3,
           s11_s4_s12, s11_s5_s11, s11_s6_s10,
           s11_s7_s9, s11_s8_s8, s11_s9_s7,
           s11_s10_s6, s11_s11_s5, s11_s12_s4,
           s10_s5_s12, s10_s6_s11, s10_s7_s10,
           s10_s8_s9, s10_s9_s8, s10_s10_s7,
           s10_s11_s6, s10_s12_s5, s9_s6_s12,
           s9_s7_s11, s9_s8_s10, s9_s9_s9,
           s9_s10_s8, s9_s11_s7, s9_s12_s6,
           s8_s7_s12, s8_s8_s11, s8_s9_s10,
           s8_s10_s9, s8_s11_s8, s8_s12_s7,
           s7_s8_s12, s7_s9_s11, s7_s10_s10,
           s7_s11_s9, s7_s12_s8, s6_s9_s12,
           s6_s10_s11, s6_s11_s10, s6_s12_s9,
           s5_s10_s12, s5_s11_s11, s5_s12_s10,
           s4_s11_s12, s4_s12_s11, s3_s12_s12,
           s12_s4_s12, s12_s5_s11, s12_s6_s10,
           s12_s7_s9, s12_s8_s8, s12_s9_s7,
           s12_s10_s6, s12_s11_s5, s12_s12_s4,
           s11_s5_s12, s11_s6_s11, s11_s7_s10,
           s11_s8_s9, s11_s9_s8, s11_s10_s7,
           s11_s11_s6, s11_s12_s5, s10_s6_s12,
           s10_s7_s11, s10_s8_s10, s10_s9_s9,
           s10_s10_s8, s10_s11_s7, s10_s12_s6,
           s9_s7_s12, s9_s8_s11, s9_s9_s10,
           s9_s10_s9, s9_s11_s8, s9_s12_s7,
           s8_s8_s12, s8_s9_s11, s8_s10_s10,
           s8_s11_s9, s8_s12_s8, s7_s9_s12,
           s7_s10_s11, s7_s11_s10, s7_s12_s9,
           s6_s10_s12, s6_s11_s11, s6_s12_s10,
           s5_s11_s12, s5_s12_s11, s4_s12_s12,
           s12_s5_s12, s12_s6_s11, s12_s7_s10,
           s12_s8_s9, s12_s9_s8, s12_s10_s7,
           s12_s11_s6, s12_s12_s5, s11_s6_s12,
           s11_s7_s11, s11_s8_s10, s11_s9_s9,
           s11_s10_s8, s11_s11_s7, s11_s12_s6,
           s10_s7_s12, s10_s8_s11, s10_s9_s10,
           s10_s10_s9, s10_s11_s8, s10_s12_s7,
           s9_s8_s12, s9_s9_s11, s9_s10_s10,
           s9_s11_s9, s9_s12_s8, s8_s9_s12,
           s8_s10_s11, s8_s11_s10, s8_s12_s9,
           s7_s10_s12, s7_s11_s11, s7_s12_s10,
           s6_s11_s12, s6_s12_s11, s5_s12_s12,
           s12_s6_s12, s12_s7_s11, s12_s8_s10,
           s12_s9_s9, s12_s10_s8, s12_s11_s7,
           s12_s12_s6, s11_s7_s12, s11_s8_s11,
           s11_s9_s10, s11_s10_s9, s11_s11_s8,
           s11_s12_s7, s10_s8_s12, s10_s9_s11,
           s10_s10_s10, s10_s11_s9, s10_s12_s8,
           s9_s9_s12, s9_s10_s11, s9_s11_s10,
           s9_s12_s9, s8_s10_s12, s8_s11_s11,
           s8_s12_s10, s7_s11_s12, s7_s12_s11,
           s6_s12_s12, s12_s7_s12, s12_s8_s11,
           s12_s9_s10, s12_s10_s9, s12_s11_s8,
           s12_s12_s7, s11_s8_s12, s11_s9_s11,
           s11_s10_s10, s11_s11_s9, s11_s12_s8,
           s10_s9_s12, s10_s10_s11, s10_s11_s10,
           s10_s12_s9, s9_s10_s12, s9_s11_s11,
           s9_s12_s10, s8_s11_s12, s8_s12_s11,
           s7_s12_s12, s12_s8_s12, s12_s9_s11,
           s12_s10_s10, s12_s11_s9, s12_s12_s8,
           s11_s9_s12, s11_s10_s11, s11_s11_s10,
           s11_s12_s9, s10_s10_s12, s10_s11_s11,
           s10_s12_s10, s9_s11_s12, s9_s12_s11,
           s8_s12_s12, s12_s9_s12, s12_s10_s11,
           s12_s11_s10, s12_s12_s9, s11_s10_s12,
           s11_s11_s11, s11_s12_s10, s10_s11_s12,
           s10_s12_s11, s9_s12_s12, s12_s10_s12,
           s12_s11_s11, s12_s12_s10, s11_s11_s12,
           s11_s12_s11, s10_s12_s12, s12_s11_s12,
           s12_s12_s11, s11_s12_s12, s12_s12_s12 };
 Events = {t1_1t2, t1_2t3, t1_3t4, t1_4t5, t1_5t6, t1_6t7, t1_7t8, t1_8t9, t1_9t10, t1_10t11, t1_11t12, t1_12t1,
           t2_1t2, t2_2t3, t2_3t4, t2_4t5, t2_5t6, t2_6t7, t2_7t8, t2_8t9, t2_9t10, t2_10t11, t2_11t12, t2_12t1,
           t3_1t2, t3_2t3, t3_3t4, t3_4t5, t3_5t6, t3_6t7, t3_7t8, t3_8t9, t3_9t10, t3_10t11, t3_11t12, t3_12t1 }

CONSTANTS
 nbr_trains, nbr_sections

PROPERTIES
 nbr_trains = 3 & nbr_sections = 12
END