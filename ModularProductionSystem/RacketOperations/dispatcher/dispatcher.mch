// copyright Richard St_Denis, Universite de Sherbrooke, 2023.
MODEL SBdispatcher

SETS
 States = {g1og2o_G1r_G2r, g1og2o_G1ie_G2r, g1og2o_G1r_G2ie,
           g1cg2o_G1e_G2r, g1og2o_G1ie_G2ie, g2cg1o_G1r_G2e,
           g1cg2o_G1ir_G2r, g1cg2o_G1e_G2ie, g2cg1o_G1ie_G2e,
           g2cg1o_G1r_G2ir, g1cg2o_G1ir_G2ie, g1cg2c_G1e_G2e,
           g2cg1o_G1ie_G2ir, g2cg1c_G1e_G2e, g1cg2c_G1ir_G2e,
           g1cg2c_G1e_G2ir, g2cg1c_G1e_G2ir, g2cg1c_G1ir_G2e,
           g1cg2c_G1ir_G2ir, g1cg2c_G1e_G2r, g2cg1c_G1ir_G2ir,
           g2cg1c_G1r_G2e, g1cg2c_G1ir_G2r, g1cg2c_G1e_G2ie,
           g2cg1c_G1r_G2ir, g2cg1c_G1ie_G2e, g2cg1o_G1r_G2r,
           g1cg2c_G1ir_G2ie, g1cg2o_G1r_G2r, g2cg1c_G1ie_G2ir,
           g2cg1o_G1r_G2ie, g2cg1o_G1ie_G2r, g1cg2o_G1ie_G2r,
           g1cg2o_G1r_G2ie, g2cg1o_G1ie_G2ie, g2cg1c_G1e_G2r,
           g1cg2o_G1ie_G2ie, g1cg2c_G1r_G2e, g2cg1c_G1e_G2ie,
           g2cg1c_G1ir_G2r, g1cg2c_G1ie_G2e, g1cg2c_G1r_G2ir,
           g2cg1c_G1ir_G2ie, g2cg1c_G1r_G2r, g1cg2c_G1ie_G2ir,
           g1cg2c_G1r_G2r, g2cg1c_G1r_G2ie, g2cg1c_G1ie_G2r,
           g1cg2c_G1ie_G2r, g1cg2c_G1r_G2ie, g2cg1c_G1ie_G2ie,
           g1cg2c_G1ie_G2ie };
 Events = {G1_eoc, G1_eoo, G2_eoc, G2_eoo, G1_close, G1_open, G2_close, G2_open};
 LocalStates = {g1og2o, g1cg2o, g2cg1o, g1cg2c, g2cg1c, G1e, G1ie, G1ir, G1r, G2e, G2ie, G2ir, G2r}

CONSTANTS
 G_FB,
 seqD, gate1, gate2,
 Qpred, BadStates

PROPERTIES
 G_FB = rec(       // Automaton modeling the free behavior
            states: States,
            events: Events,
            tf: {(g1og2o_G1r_G2r,G1_close,g1og2o_G1ie_G2r), (g1og2o_G1r_G2r,G2_close,g1og2o_G1r_G2ie), (g1og2o_G1ie_G2r,G1_eoc,g1cg2o_G1e_G2r),
                 (g1og2o_G1ie_G2r,G2_close,g1og2o_G1ie_G2ie), (g1og2o_G1r_G2ie,G1_close,g1og2o_G1ie_G2ie), (g1og2o_G1r_G2ie,G2_eoc,g2cg1o_G1r_G2e),
                 (g1cg2o_G1e_G2r,G1_open,g1cg2o_G1ir_G2r), (g1cg2o_G1e_G2r,G2_close,g1cg2o_G1e_G2ie), (g1og2o_G1ie_G2ie,G1_eoc,g1cg2o_G1e_G2ie),
                 (g1og2o_G1ie_G2ie,G2_eoc,g2cg1o_G1ie_G2e), (g2cg1o_G1r_G2e,G2_open,g2cg1o_G1r_G2ir), (g2cg1o_G1r_G2e,G1_close,g2cg1o_G1ie_G2e),
                 (g1cg2o_G1ir_G2r,G1_eoo,g1og2o_G1r_G2r), (g1cg2o_G1ir_G2r,G2_close,g1cg2o_G1ir_G2ie), (g1cg2o_G1e_G2ie,G1_open,g1cg2o_G1ir_G2ie),
                 (g1cg2o_G1e_G2ie,G2_eoc,g1cg2c_G1e_G2e), (g2cg1o_G1ie_G2e,G2_open,g2cg1o_G1ie_G2ir), (g2cg1o_G1ie_G2e,G1_eoc,g2cg1c_G1e_G2e),
                 (g2cg1o_G1r_G2ir,G2_eoo,g1og2o_G1r_G2r), (g2cg1o_G1r_G2ir,G1_close,g2cg1o_G1ie_G2ir), (g1cg2o_G1ir_G2ie,G1_eoo,g1og2o_G1r_G2ie),
                 (g1cg2o_G1ir_G2ie,G2_eoc,g1cg2c_G1ir_G2e), (g1cg2c_G1e_G2e,G1_open,g1cg2c_G1ir_G2e), (g1cg2c_G1e_G2e,G2_open,g1cg2c_G1e_G2ir),
                 (g2cg1o_G1ie_G2ir,G2_eoo,g1og2o_G1ie_G2r), (g2cg1o_G1ie_G2ir,G1_eoc,g2cg1c_G1e_G2ir), (g2cg1c_G1e_G2e,G2_open,g2cg1c_G1e_G2ir),
                 (g2cg1c_G1e_G2e,G1_open,g2cg1c_G1ir_G2e), (g1cg2c_G1ir_G2e,G1_eoo,g2cg1o_G1r_G2e), (g1cg2c_G1ir_G2e,G2_open,g1cg2c_G1ir_G2ir),
                 (g1cg2c_G1e_G2ir,G1_open,g1cg2c_G1ir_G2ir), (g1cg2c_G1e_G2ir,G2_eoo,g1cg2c_G1e_G2r), (g2cg1c_G1e_G2ir,G2_eoo,g1cg2o_G1e_G2r),
                 (g2cg1c_G1e_G2ir,G1_open,g2cg1c_G1ir_G2ir), (g2cg1c_G1ir_G2e,G2_open,g2cg1c_G1ir_G2ir), (g2cg1c_G1ir_G2e,G1_eoo,g2cg1c_G1r_G2e),
                 (g1cg2c_G1ir_G2ir,G1_eoo,g2cg1o_G1r_G2ir), (g1cg2c_G1ir_G2ir,G2_eoo,g1cg2c_G1ir_G2r), (g1cg2c_G1e_G2r,G1_open,g1cg2c_G1ir_G2r),
                 (g1cg2c_G1e_G2r,G2_close,g1cg2c_G1e_G2ie), (g2cg1c_G1ir_G2ir,G2_eoo,g1cg2o_G1ir_G2r), (g2cg1c_G1ir_G2ir,G1_eoo,g2cg1c_G1r_G2ir),
                 (g2cg1c_G1r_G2e,G2_open,g2cg1c_G1r_G2ir), (g2cg1c_G1r_G2e,G1_close,g2cg1c_G1ie_G2e), (g1cg2c_G1ir_G2r,G1_eoo,g2cg1o_G1r_G2r),
                 (g1cg2c_G1ir_G2r,G2_close,g1cg2c_G1ir_G2ie), (g1cg2c_G1e_G2ie,G1_open,g1cg2c_G1ir_G2ie), (g1cg2c_G1e_G2ie,G2_eoc,g1cg2c_G1e_G2e),
                 (g2cg1c_G1r_G2ir,G2_eoo,g1cg2o_G1r_G2r), (g2cg1c_G1r_G2ir,G1_close,g2cg1c_G1ie_G2ir), (g2cg1c_G1ie_G2e,G2_open,g2cg1c_G1ie_G2ir),
                 (g2cg1c_G1ie_G2e,G1_eoc,g2cg1c_G1e_G2e), (g2cg1o_G1r_G2r,G2_close,g2cg1o_G1r_G2ie), (g2cg1o_G1r_G2r,G1_close,g2cg1o_G1ie_G2r),
                 (g1cg2c_G1ir_G2ie,G1_eoo,g2cg1o_G1r_G2ie), (g1cg2c_G1ir_G2ie,G2_eoc,g1cg2c_G1ir_G2e), (g1cg2o_G1r_G2r,G1_close,g1cg2o_G1ie_G2r),
                 (g1cg2o_G1r_G2r,G2_close,g1cg2o_G1r_G2ie), (g2cg1c_G1ie_G2ir,G2_eoo,g1cg2o_G1ie_G2r), (g2cg1c_G1ie_G2ir,G1_eoc,g2cg1c_G1e_G2ir),
                 (g2cg1o_G1r_G2ie,G1_close,g2cg1o_G1ie_G2ie), (g2cg1o_G1r_G2ie,G2_eoc,g2cg1o_G1r_G2e), (g2cg1o_G1ie_G2r,G2_close,g2cg1o_G1ie_G2ie),
                 (g2cg1o_G1ie_G2r,G1_eoc,g2cg1c_G1e_G2r), (g1cg2o_G1ie_G2r,G2_close,g1cg2o_G1ie_G2ie), (g1cg2o_G1ie_G2r,G1_eoc,g1cg2o_G1e_G2r),
                 (g1cg2o_G1r_G2ie,G1_close,g1cg2o_G1ie_G2ie), (g1cg2o_G1r_G2ie,G2_eoc,g1cg2c_G1r_G2e), (g2cg1o_G1ie_G2ie,G1_eoc,g2cg1c_G1e_G2ie),
                 (g2cg1o_G1ie_G2ie,G2_eoc,g2cg1o_G1ie_G2e), (g2cg1c_G1e_G2r,G2_close,g2cg1c_G1e_G2ie), (g2cg1c_G1e_G2r,G1_open,g2cg1c_G1ir_G2r),
                 (g1cg2o_G1ie_G2ie,G2_eoc,g1cg2c_G1ie_G2e), (g1cg2o_G1ie_G2ie,G1_eoc,g1cg2o_G1e_G2ie), (g1cg2c_G1r_G2e,G1_close,g1cg2c_G1ie_G2e),
                 (g1cg2c_G1r_G2e,G2_open,g1cg2c_G1r_G2ir), (g2cg1c_G1e_G2ie,G1_open,g2cg1c_G1ir_G2ie), (g2cg1c_G1e_G2ie,G2_eoc,g2cg1c_G1e_G2e),
                 (g2cg1c_G1ir_G2r,G2_close,g2cg1c_G1ir_G2ie), (g2cg1c_G1ir_G2r,G1_eoo,g2cg1c_G1r_G2r), (g1cg2c_G1ie_G2e,G1_eoc,g1cg2c_G1e_G2e),
                 (g1cg2c_G1ie_G2e,G2_open,g1cg2c_G1ie_G2ir), (g1cg2c_G1r_G2ir,G1_close,g1cg2c_G1ie_G2ir), (g1cg2c_G1r_G2ir,G2_eoo,g1cg2c_G1r_G2r),
                 (g2cg1c_G1ir_G2ie,G2_eoc,g2cg1c_G1ir_G2e), (g2cg1c_G1ir_G2ie,G1_eoo,g2cg1c_G1r_G2ie), (g2cg1c_G1r_G2r,G2_close,g2cg1c_G1r_G2ie),
                 (g2cg1c_G1r_G2r,G1_close,g2cg1c_G1ie_G2r), (g1cg2c_G1ie_G2ir,G1_eoc,g1cg2c_G1e_G2ir), (g1cg2c_G1ie_G2ir,G2_eoo,g1cg2c_G1ie_G2r),
                 (g1cg2c_G1r_G2r,G1_close,g1cg2c_G1ie_G2r), (g1cg2c_G1r_G2r,G2_close,g1cg2c_G1r_G2ie), (g2cg1c_G1r_G2ie,G1_close,g2cg1c_G1ie_G2ie),
                 (g2cg1c_G1r_G2ie,G2_eoc,g2cg1c_G1r_G2e), (g2cg1c_G1ie_G2r,G2_close,g2cg1c_G1ie_G2ie), (g2cg1c_G1ie_G2r,G1_eoc,g2cg1c_G1e_G2r),
                 (g1cg2c_G1ie_G2r,G1_eoc,g1cg2c_G1e_G2r), (g1cg2c_G1ie_G2r,G2_close,g1cg2c_G1ie_G2ie), (g1cg2c_G1r_G2ie,G1_close,g1cg2c_G1ie_G2ie),
                 (g1cg2c_G1r_G2ie,G2_eoc,g1cg2c_G1r_G2e), (g2cg1c_G1ie_G2ie,G1_eoc,g2cg1c_G1e_G2ie), (g2cg1c_G1ie_G2ie,G2_eoc,g2cg1c_G1ie_G2e),
                 (g1cg2c_G1ie_G2ie,G1_eoc,g1cg2c_G1e_G2ie), (g1cg2c_G1ie_G2ie,G2_eoc,g1cg2c_G1ie_G2e) },
            x_0: g1og2o_G1r_G2r,
            Xm: {g1og2o_G1r_G2r, g1cg2o_G1e_G2r, g1cg2o_G1r_G2r,
           g2cg1o_G1r_G2e, g2cg1o_G1r_G2r},
            ctrlEvents: {G1_close, G1_open, G2_close, G2_open} )
 &
 seqD = {
      (g1og2o_G1r_G2r,g1og2o),(g1og2o_G1ie_G2r,g1og2o),(g1og2o_G1r_G2ie,g1og2o),
      (g1cg2o_G1e_G2r,g1cg2o),(g1og2o_G1ie_G2ie,g1og2o),(g2cg1o_G1r_G2e,g2cg1o),
      (g1cg2o_G1ir_G2r,g1cg2o),(g1cg2o_G1e_G2ie,g1cg2o),(g2cg1o_G1ie_G2e,g2cg1o),
      (g2cg1o_G1r_G2ir,g2cg1o),(g1cg2o_G1ir_G2ie,g1cg2o),(g1cg2c_G1e_G2e,g1cg2c),
      (g2cg1o_G1ie_G2ir,g2cg1o),(g2cg1c_G1e_G2e,g2cg1c),(g1cg2c_G1ir_G2e,g1cg2c),
      (g1cg2c_G1e_G2ir,g1cg2c),(g2cg1c_G1e_G2ir,g2cg1c),(g2cg1c_G1ir_G2e,g2cg1c),
      (g1cg2c_G1ir_G2ir,g1cg2c),(g1cg2c_G1e_G2r,g1cg2c),(g2cg1c_G1ir_G2ir,g2cg1c),
      (g2cg1c_G1r_G2e,g2cg1c),(g1cg2c_G1ir_G2r,g1cg2c),(g1cg2c_G1e_G2ie,g1cg2c),
      (g2cg1c_G1r_G2ir,g2cg1c),(g2cg1c_G1ie_G2e,g2cg1c),(g2cg1o_G1r_G2r,g2cg1o),
      (g1cg2c_G1ir_G2ie,g1cg2c),(g1cg2o_G1r_G2r,g1cg2o),(g2cg1c_G1ie_G2ir,g2cg1c),
      (g2cg1o_G1r_G2ie,g2cg1o),(g2cg1o_G1ie_G2r,g2cg1o),(g1cg2o_G1ie_G2r,g1cg2o),
      (g1cg2o_G1r_G2ie,g1cg2o),(g2cg1o_G1ie_G2ie,g2cg1o),(g2cg1c_G1e_G2r,g2cg1c),
      (g1cg2o_G1ie_G2ie,g1cg2o),(g1cg2c_G1r_G2e,g1cg2c),(g2cg1c_G1e_G2ie,g2cg1c),
      (g2cg1c_G1ir_G2r,g2cg1c),(g1cg2c_G1ie_G2e,g1cg2c),(g1cg2c_G1r_G2ir,g1cg2c),
      (g2cg1c_G1ir_G2ie,g2cg1c),(g2cg1c_G1r_G2r,g2cg1c),(g1cg2c_G1ie_G2ir,g1cg2c),
      (g1cg2c_G1r_G2r,g1cg2c),(g2cg1c_G1r_G2ie,g2cg1c),(g2cg1c_G1ie_G2r,g2cg1c),
      (g1cg2c_G1ie_G2r,g1cg2c),(g1cg2c_G1r_G2ie,g1cg2c),(g2cg1c_G1ie_G2ie,g2cg1c),
      (g1cg2c_G1ie_G2ie,g1cg2c) }
 &
 gate1 = {
      (g1og2o_G1r_G2r,G1r),(g1og2o_G1ie_G2r,G1ie),(g1og2o_G1r_G2ie,G1r),
      (g1cg2o_G1e_G2r,G1e),(g1og2o_G1ie_G2ie,G1ie),(g2cg1o_G1r_G2e,G1r),
      (g1cg2o_G1ir_G2r,G1ir),(g1cg2o_G1e_G2ie,G1e),(g2cg1o_G1ie_G2e,G1ie),
      (g2cg1o_G1r_G2ir,G1r),(g1cg2o_G1ir_G2ie,G1ir),(g1cg2c_G1e_G2e,G1e),
      (g2cg1o_G1ie_G2ir,G1ie),(g2cg1c_G1e_G2e,G1e),(g1cg2c_G1ir_G2e,G1ir),
      (g1cg2c_G1e_G2ir,G1e),(g2cg1c_G1e_G2ir,G1e),(g2cg1c_G1ir_G2e,G1ir),
      (g1cg2c_G1ir_G2ir,G1ir),(g1cg2c_G1e_G2r,G1e),(g2cg1c_G1ir_G2ir,G1ir),
      (g2cg1c_G1r_G2e,G1r),(g1cg2c_G1ir_G2r,G1ir),(g1cg2c_G1e_G2ie,G1e),
      (g2cg1c_G1r_G2ir,G1r),(g2cg1c_G1ie_G2e,G1ie),(g2cg1o_G1r_G2r,G1r),
      (g1cg2c_G1ir_G2ie,G1ir),(g1cg2o_G1r_G2r,G1r),(g2cg1c_G1ie_G2ir,G1ie),
      (g2cg1o_G1r_G2ie,G1r),(g2cg1o_G1ie_G2r,G1ie),(g1cg2o_G1ie_G2r,G1ie),
      (g1cg2o_G1r_G2ie,G1r),(g2cg1o_G1ie_G2ie,G1ie),(g2cg1c_G1e_G2r,G1e),
      (g1cg2o_G1ie_G2ie,G1ie),(g1cg2c_G1r_G2e,G1r),(g2cg1c_G1e_G2ie,G1e),
      (g2cg1c_G1ir_G2r,G1ir),(g1cg2c_G1ie_G2e,G1ie),(g1cg2c_G1r_G2ir,G1r),
      (g2cg1c_G1ir_G2ie,G1ir),(g2cg1c_G1r_G2r,G1r),(g1cg2c_G1ie_G2ir,G1ie),
      (g1cg2c_G1r_G2r,G1r),(g2cg1c_G1r_G2ie,G1r),(g2cg1c_G1ie_G2r,G1ie),
      (g1cg2c_G1ie_G2r,G1ie),(g1cg2c_G1r_G2ie,G1r),(g2cg1c_G1ie_G2ie,G1ie),
      (g1cg2c_G1ie_G2ie,G1ie) }
 &
 gate2 = {
      (g1og2o_G1r_G2r,G2r),(g1og2o_G1ie_G2r,G2r),(g1og2o_G1r_G2ie,G2ie),
      (g1cg2o_G1e_G2r,G2r),(g1og2o_G1ie_G2ie,G2ie),(g2cg1o_G1r_G2e,G2e),
      (g1cg2o_G1ir_G2r,G2r),(g1cg2o_G1e_G2ie,G2ie),(g2cg1o_G1ie_G2e,G2e),
      (g2cg1o_G1r_G2ir,G2ir),(g1cg2o_G1ir_G2ie,G2ie),(g1cg2c_G1e_G2e,G2e),
      (g2cg1o_G1ie_G2ir,G2ir),(g2cg1c_G1e_G2e,G2e),(g1cg2c_G1ir_G2e,G2e),
      (g1cg2c_G1e_G2ir,G2ir),(g2cg1c_G1e_G2ir,G2ir),(g2cg1c_G1ir_G2e,G2e),
      (g1cg2c_G1ir_G2ir,G2ir),(g1cg2c_G1e_G2r,G2r),(g2cg1c_G1ir_G2ir,G2ir),
      (g2cg1c_G1r_G2e,G2e),(g1cg2c_G1ir_G2r,G2r),(g1cg2c_G1e_G2ie,G2ie),
      (g2cg1c_G1r_G2ir,G2ir),(g2cg1c_G1ie_G2e,G2e),(g2cg1o_G1r_G2r,G2r),
      (g1cg2c_G1ir_G2ie,G2ie),(g1cg2o_G1r_G2r,G2r),(g2cg1c_G1ie_G2ir,G2ir),
      (g2cg1o_G1r_G2ie,G2ie),(g2cg1o_G1ie_G2r,G2r),(g1cg2o_G1ie_G2r,G2r),
      (g1cg2o_G1r_G2ie,G2ie),(g2cg1o_G1ie_G2ie,G2ie),(g2cg1c_G1e_G2r,G2r),
      (g1cg2o_G1ie_G2ie,G2ie),(g1cg2c_G1r_G2e,G2e),(g2cg1c_G1e_G2ie,G2ie),
      (g2cg1c_G1ir_G2r,G2r),(g1cg2c_G1ie_G2e,G2e),(g1cg2c_G1r_G2ir,G2ir),
      (g2cg1c_G1ir_G2ie,G2ie),(g2cg1c_G1r_G2r,G2r),(g1cg2c_G1ie_G2ir,G2ir),
      (g1cg2c_G1r_G2r,G2r),(g2cg1c_G1r_G2ie,G2ie),(g2cg1c_G1ie_G2r,G2r),
      (g1cg2c_G1ie_G2r,G2r),(g1cg2c_G1r_G2ie,G2ie),(g2cg1c_G1ie_G2ie,G2ie),
      (g1cg2c_G1ie_G2ie,G2ie) }
 &
 BadStates =  States    // to be completed
 &
 Qpred = States - BadStates
END