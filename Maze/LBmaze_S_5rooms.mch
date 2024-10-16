// copyright Richard St_Denis, Universite de Sherbrooke, 2023.
MODEL LBmaze_S_5rooms

SETS
 States = {r2_r4, r0_r4, r2_r3,
           r1_r4, r0_r3, r3_r4,
           r2_r0, r1_r3, r0_r0,
           r3_r3, r4_r4, r2_r2,
           r1_r0, r0_r2, r3_r0,
           r4_r3, r2_r1, r1_r2,
           r0_r1, r3_r2, r4_r0,
           r1_r1, r3_r1, r4_r2,
           r4_r1 };
 Events = {c0to1, c1to2, c2to0, c0to3, c3to4, c4to0, c1to3, c3to1,
           m0to2, m2to1, m1to0, m0to4, m4to3, m3to0 }

CONSTANTS
 nbr_rooms

PROPERTIES
 nbr_rooms = 5
END