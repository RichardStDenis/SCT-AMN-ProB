// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
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
 G_FB =rec(   // Automaton modeling the free behavior
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
/*
  Standard constraints:
    1. The dispatcher must not attempt to open the second gate when the first gate
       has been closed before the second gate.
    2. The dispatcher must not attempt to open the first gate when the second gate
       has been closed before the first gate.
  Implicit constraint:
    3. The gates commands must be strictly alternated.
*/
 BadStates =
// The dispatcher is in the state where the first gate has been closed before the
// second gate and the second gate is in retraction:
    {x | x : States & seqD(x) = g1cg2c & gate2(x) = G2ir} \/
// The dispatcher is in the state where the second gate has been closed before the
// first gate and the first gate is in retraction:
    {x | x : States & seqD(x) = g2cg1c & gate1(x) = G1ir} \/
// The gates are in extension or in retraction:
    {x | x : States & gate1(x) : {G1ie,G1ir} & gate2(x) : {G2ie,G2ir}}
 &
 Qpred = States - BadStates
END

/*
OUTPUT:
The transition structure of the agent
 (i.e.,the solution of the control problem)
Number of states of the exhaustive model: 52
Number of transitions of the exhaustive model: 104
Cardinality of the predicate: 35
Supremal coreachable and controllable predicates (13):
{g1og2o_G1r_G2r,g1og2o_G1ie_G2r,g1og2o_G1r_G2ie,g1cg2o_G1e_G2r,g2cg1o_G1r_G2e,
 g1cg2o_G1ir_G2r,g1cg2o_G1e_G2ie,g2cg1o_G1ie_G2e,g2cg1o_G1r_G2ir,g1cg2c_G1e_G2e,
 g2cg1c_G1e_G2e,g1cg2c_G1ir_G2e,g2cg1c_G1e_G2ir}
Disabled events:
{(g1og2o_G1r_G2r|->{}),(g1og2o_G1ie_G2r|->{G2_close}),(g1og2o_G1r_G2ie|->{G1_close}),
 (g1cg2o_G1e_G2r|->{}),(g2cg1o_G1r_G2e|->{}),(g1cg2o_G1ir_G2r|->{G2_close}),
 (g1cg2o_G1e_G2ie|->{G1_open}),(g2cg1o_G1ie_G2e|->{G2_open}),
 (g2cg1o_G1r_G2ir|->{G1_close}),(g1cg2c_G1e_G2e|->{G2_open}),
 (g2cg1c_G1e_G2e|->{G1_open}),(g1cg2c_G1ir_G2e|->{G2_open}),
 (g2cg1c_G1e_G2ir|->{G1_open})}
Enabled events:
{(g1og2o_G1r_G2r|->{G1_close,G2_close}),(g1og2o_G1ie_G2r|->{G1_eoc}),
 (g1og2o_G1r_G2ie|->{G2_eoc}),(g1cg2o_G1e_G2r|->{G1_open,G2_close}),
 (g2cg1o_G1r_G2e|->{G1_close,G2_open}),(g1cg2o_G1ir_G2r|->{G1_eoo}),
 (g1cg2o_G1e_G2ie|->{G2_eoc}),(g2cg1o_G1ie_G2e|->{G1_eoc}),
 (g2cg1o_G1r_G2ir|->{G2_eoo}),(g1cg2c_G1e_G2e|->{G1_open}),
 (g2cg1c_G1e_G2e|->{G2_open}),(g1cg2c_G1ir_G2e|->{G1_eoo}),
 (g2cg1c_G1e_G2ir|->{G2_eoo})}
Closed-loop system:
 rec(Xm:{g1og2o_G1r_G2r,g1cg2o_G1e_G2r,g2cg1o_G1r_G2e},
     ctrlEvents:{G1_close,G1_open,G2_close,G2_open},
     events:{G1_eoc,G1_eoo,G2_eoc,G2_eoo,G1_close,G1_open,G2_close,G2_open},
     states:{g1og2o_G1r_G2r,g1og2o_G1ie_G2r,g1og2o_G1r_G2ie,g1cg2o_G1e_G2r,
             g2cg1o_G1r_G2e,g1cg2o_G1ir_G2r,g1cg2o_G1e_G2ie,g2cg1o_G1ie_G2e,
             g2cg1o_G1r_G2ir,g1cg2c_G1e_G2e,g2cg1c_G1e_G2e,g1cg2c_G1ir_G2e,
             g2cg1c_G1e_G2ir},
     tf:{((g1og2o_G1r_G2r|->G1_close)|->g1og2o_G1ie_G2r),
         ((g1og2o_G1r_G2r|->G2_close)|->g1og2o_G1r_G2ie),
         ((g1og2o_G1ie_G2r|->G1_eoc)|->g1cg2o_G1e_G2r),
         ((g1og2o_G1r_G2ie|->G2_eoc)|->g2cg1o_G1r_G2e),
         ((g1cg2o_G1e_G2r|->G1_open)|->g1cg2o_G1ir_G2r),
         ((g1cg2o_G1e_G2r|->G2_close)|->g1cg2o_G1e_G2ie),
         ((g2cg1o_G1r_G2e|->G1_close)|->g2cg1o_G1ie_G2e),
         ((g2cg1o_G1r_G2e|->G2_open)|->g2cg1o_G1r_G2ir),
         ((g1cg2o_G1ir_G2r|->G1_eoo)|->g1og2o_G1r_G2r),
         ((g1cg2o_G1e_G2ie|->G2_eoc)|->g1cg2c_G1e_G2e),
         ((g2cg1o_G1ie_G2e|->G1_eoc)|->g2cg1c_G1e_G2e),
         ((g2cg1o_G1r_G2ir|->G2_eoo)|->g1og2o_G1r_G2r),
         ((g1cg2c_G1e_G2e|->G1_open)|->g1cg2c_G1ir_G2e),
         ((g2cg1c_G1e_G2e|->G2_open)|->g2cg1c_G1e_G2ir),
         ((g1cg2c_G1ir_G2e|->G1_eoo)|->g2cg1o_G1r_G2e),
         ((g2cg1c_G1e_G2ir|->G2_eoo)|->g1cg2o_G1e_G2r)},
     x_0:g1og2o_G1r_G2r)
*/

/*
  ControlLaw(G1_close) = G1r & (G2e | G2r)
  ControlLaw(G1_open) = G1e & (g1cg2c | (g1cg2o & G2r))
  ControlLaw(G2_close) = G2r & (G1e | G1r)
  ControlLaw(G2_open) = G2e & (g2cg1c | (g2cg1o & G1r))
*/

/*
% Runtime for SOLUTION for SETUP_CONSTANTS: 100 ms (walltime: 100 ms)
% Finished searching for valid initial states 120 ms walltime (120 ms runtime), since start: 1780 ms
% Starting random_animate (max_steps=5)
perform_random_step(1,1)
random(2,1)
perform_random_step(1,2)
random(2,1)
perform_random_step(1,3)
random(2,1)
"Compute supCCP..."
"Compute state feedback functions..."
"Save results..."
perform_random_step(1,4)
random(2,1)
"The control is nonconflicting."

ALL OPERATIONS COVERED

perform_random_step(1,5)
random(2,1)
finished_random_animate(5,[runtime/290,total_runtime/290,walltime/300])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 470 ms walltime (450 ms runtime), since start: 2090 ms

% Finished process_file (errors=0, warnings=0)
*/