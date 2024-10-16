// copyright Richard St_Denis, Universite de Sherbrooke, 2023.
MODEL LBmaze_LB_5rooms

INCLUDES
 LBmaze_FB_5rooms /*@file "./LBmaze_FB_5rooms.mch" */

CONSTANTS
 G_LB,
 bound_G_LB

PROPERTIES
 G_LB = rec(       // Automaton modeling the legal behavior
            events:{c0to1,c1to2,c2to0,c0to3,c3to4,c4to0,c1to3,c3to1,
                    m0to2,m2to1,m1to0,m0to4,m4to3,m3to0 },
            x_0:r2_r4,
            Xm:{r2_r4},
            states:{r2_r4,r0_r4,r2_r3,r1_r4,r0_r3,r3_r4,r2_r0,r1_r3,r1_r0,r0_r2,
                    r3_r0,r4_r3,r2_r1,r1_r2,r0_r1,r3_r2,r4_r0,r3_r1,r4_r2,r4_r1 },
            tf:{(r2_r4,c2to0,r0_r4),(r2_r4,m4to3,r2_r3),
                (r0_r4,c0to1,r1_r4),(r0_r4,c0to3,r3_r4),
                (r0_r4,m4to3,r0_r3),(r2_r3,c2to0,r0_r3),
                (r2_r3,m3to0,r2_r0),(r1_r4,c1to2,r2_r4),
                (r1_r4,c1to3,r3_r4),(r1_r4,m4to3,r1_r3),
                (r0_r3,c0to1,r1_r3),(r3_r4,c3to1,r1_r4),
                (r2_r0,m0to4,r2_r4),(r1_r3,c1to2,r2_r3),
                (r1_r3,m3to0,r1_r0),(r1_r0,c1to2,r2_r0),
                (r1_r0,c1to3,r3_r0),(r1_r0,m0to2,r1_r2),
                (r1_r0,m0to4,r1_r4),(r0_r2,c0to1,r1_r2),
                (r0_r2,c0to3,r3_r2),(r0_r2,m2to1,r0_r1),
                (r3_r0,c3to4,r4_r0),(r3_r0,c3to1,r1_r0),
                (r3_r0,m0to2,r3_r2),(r3_r0,m0to4,r3_r4),
                (r4_r3,c4to0,r0_r3),(r4_r3,m3to0,r4_r0),
                (r2_r1,c2to0,r0_r1),(r2_r1,m1to0,r2_r0),
                (r1_r2,c1to3,r3_r2),(r0_r1,c0to3,r3_r1),
                (r3_r2,c3to4,r4_r2),(r3_r2,c3to1,r1_r2),
                (r3_r2,m2to1,r3_r1),(r4_r0,m0to2,r4_r2),
                (r3_r1,c3to4,r4_r1),(r3_r1,m1to0,r3_r0),
                (r4_r2,c4to0,r0_r2),(r4_r2,m2to1,r4_r1),
                (r4_r1,c4to0,r0_r1),(r4_r1,m1to0,r4_r0) } ) &
 bound_G_LB = 7
END