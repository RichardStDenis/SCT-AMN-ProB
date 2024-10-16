// copyright Richard St_Denis, Universite de Sherbrooke, 2023.
MODEL SBhook_e

SETS
 States = {grbStg_Wr_Hnv, grbStg_Wie_Hnv, grbStg_Wr_Hp,
           grbStg_We_Hnv, grbStg_Wie_Hp, grbStg_Wr_Hv,
           grbStg_Wir_Hnv, grbStg_We_Hp, grbStg_Wie_Hv,
           grbStg_Wr_Hl, drpStg_Wr_Hnv, grbStg_Wir_Hp,
           grbStg_We_Hv, grbStg_Wie_Hl, drpStg_Wie_Hnv,
           drpStg_Wr_Hp, grbStg_Wir_Hv, grbStg_We_Hl,
           drpStg_We_Hnv, drpStg_Wie_Hp, drpStg_Wr_Hv,
           grbStg_Wir_Hl, drpStg_Wir_Hnv, drpStg_We_Hp,
           drpStg_Wie_Hv, drpStg_Wr_Hl, drpStg_Wir_Hp,
           drpStg_We_Hv, drpStg_Wie_Hl, drpStg_Wir_Hv,
           drpStg_We_Hl, drpStg_Wir_Hl };
 Events = {W_eoa, W_eod, W_gd, W_gu, H_drop, H_eog, H_eod, H_grab};
 LocalStates = {drpStg, grbStg, We, Wie, Wir, Wr, Hl, Hnv, Hp, Hv}

CONSTANTS
 G_FB,
 seqH, winch, hook,
 Qpred, BadStates

PROPERTIES
 G_FB = rec(       // Automaton modeling the free behavior
            states: States,
            events: Events,
            tf: {(grbStg_Wr_Hnv,W_gd,grbStg_Wie_Hnv), (grbStg_Wr_Hnv,H_grab,grbStg_Wr_Hp), (grbStg_Wie_Hnv,W_eod,grbStg_We_Hnv),
                 (grbStg_Wie_Hnv,H_grab,grbStg_Wie_Hp), (grbStg_Wr_Hp,W_gd,grbStg_Wie_Hp), (grbStg_Wr_Hp,H_eog,grbStg_Wr_Hv),
                 (grbStg_We_Hnv,W_gu,grbStg_Wir_Hnv), (grbStg_We_Hnv,H_grab,grbStg_We_Hp), (grbStg_Wie_Hp,W_eod,grbStg_We_Hp),
                 (grbStg_Wie_Hp,H_eog,grbStg_Wie_Hv), (grbStg_Wr_Hv,W_gd,grbStg_Wie_Hv), (grbStg_Wr_Hv,H_drop,grbStg_Wr_Hl),
                 (grbStg_Wir_Hnv,W_eoa,drpStg_Wr_Hnv), (grbStg_Wir_Hnv,H_grab,grbStg_Wir_Hp), (grbStg_We_Hp,W_gu,grbStg_Wir_Hp),
                 (grbStg_We_Hp,H_eog,grbStg_We_Hv), (grbStg_Wie_Hv,W_eod,grbStg_We_Hv), (grbStg_Wie_Hv,H_drop,grbStg_Wie_Hl),
                 (grbStg_Wr_Hl,W_gd,grbStg_Wie_Hl), (grbStg_Wr_Hl,H_eod,grbStg_Wr_Hnv), (drpStg_Wr_Hnv,W_gd,drpStg_Wie_Hnv),
                 (drpStg_Wr_Hnv,H_grab,drpStg_Wr_Hp), (grbStg_Wir_Hp,W_eoa,drpStg_Wr_Hp), (grbStg_Wir_Hp,H_eog,grbStg_Wir_Hv),
                 (grbStg_We_Hv,W_gu,grbStg_Wir_Hv), (grbStg_We_Hv,H_drop,grbStg_We_Hl), (grbStg_Wie_Hl,W_eod,grbStg_We_Hl),
                 (grbStg_Wie_Hl,H_eod,grbStg_Wie_Hnv), (drpStg_Wie_Hnv,W_eod,drpStg_We_Hnv), (drpStg_Wie_Hnv,H_grab,drpStg_Wie_Hp),
                 (drpStg_Wr_Hp,W_gd,drpStg_Wie_Hp), (drpStg_Wr_Hp,H_eog,drpStg_Wr_Hv), (grbStg_Wir_Hv,W_eoa,drpStg_Wr_Hv),
                 (grbStg_Wir_Hv,H_drop,grbStg_Wir_Hl), (grbStg_We_Hl,W_gu,grbStg_Wir_Hl), (grbStg_We_Hl,H_eod,grbStg_We_Hnv),
                 (drpStg_We_Hnv,W_gu,drpStg_Wir_Hnv), (drpStg_We_Hnv,H_grab,drpStg_We_Hp), (drpStg_Wie_Hp,W_eod,drpStg_We_Hp),
                 (drpStg_Wie_Hp,H_eog,drpStg_Wie_Hv), (drpStg_Wr_Hv,W_gd,drpStg_Wie_Hv), (drpStg_Wr_Hv,H_drop,drpStg_Wr_Hl),
                 (grbStg_Wir_Hl,W_eoa,drpStg_Wr_Hl), (grbStg_Wir_Hl,H_eod,grbStg_Wir_Hnv), (drpStg_Wir_Hnv,W_eoa,grbStg_Wr_Hnv),
                 (drpStg_Wir_Hnv,H_grab,drpStg_Wir_Hp), (drpStg_We_Hp,W_gu,drpStg_Wir_Hp), (drpStg_We_Hp,H_eog,drpStg_We_Hv),
                 (drpStg_Wie_Hv,W_eod,drpStg_We_Hv), (drpStg_Wie_Hv,H_drop,drpStg_Wie_Hl), (drpStg_Wr_Hl,W_gd,drpStg_Wie_Hl),
                 (drpStg_Wr_Hl,H_eod,drpStg_Wr_Hnv), (drpStg_Wir_Hp,W_eoa,grbStg_Wr_Hp), (drpStg_Wir_Hp,H_eog,drpStg_Wir_Hv),
                 (drpStg_We_Hv,W_gu,drpStg_Wir_Hv), (drpStg_We_Hv,H_drop,drpStg_We_Hl), (drpStg_Wie_Hl,W_eod,drpStg_We_Hl),
                 (drpStg_Wie_Hl,H_eod,drpStg_Wie_Hnv), (drpStg_Wir_Hv,W_eoa,grbStg_Wr_Hv), (drpStg_Wir_Hv,H_drop,drpStg_Wir_Hl),
                 (drpStg_We_Hl,W_gu,drpStg_Wir_Hl), (drpStg_We_Hl,H_eod,drpStg_We_Hnv), (drpStg_Wir_Hl,W_eoa,grbStg_Wr_Hl),
                 (drpStg_Wir_Hl,H_eod,drpStg_Wir_Hnv) },
            x_0: grbStg_Wr_Hnv,
            Xm: {grbStg_Wr_Hnv},
            ctrlEvents: {W_gd, W_gu, H_drop, H_grab} )
 &
 seqH = {
      (grbStg_Wr_Hnv,grbStg),(grbStg_Wie_Hnv,grbStg),(grbStg_Wr_Hp,grbStg),
      (grbStg_We_Hnv,grbStg),(grbStg_Wie_Hp,grbStg),(grbStg_Wr_Hv,grbStg),
      (grbStg_Wir_Hnv,grbStg),(grbStg_We_Hp,grbStg),(grbStg_Wie_Hv,grbStg),
      (grbStg_Wr_Hl,grbStg),(drpStg_Wr_Hnv,drpStg),(grbStg_Wir_Hp,grbStg),
      (grbStg_We_Hv,grbStg),(grbStg_Wie_Hl,grbStg),(drpStg_Wie_Hnv,drpStg),
      (drpStg_Wr_Hp,drpStg),(grbStg_Wir_Hv,grbStg),(grbStg_We_Hl,grbStg),
      (drpStg_We_Hnv,drpStg),(drpStg_Wie_Hp,drpStg),(drpStg_Wr_Hv,drpStg),
      (grbStg_Wir_Hl,grbStg),(drpStg_Wir_Hnv,drpStg),(drpStg_We_Hp,drpStg),
      (drpStg_Wie_Hv,drpStg),(drpStg_Wr_Hl,drpStg),(drpStg_Wir_Hp,drpStg),
      (drpStg_We_Hv,drpStg),(drpStg_Wie_Hl,drpStg),(drpStg_Wir_Hv,drpStg),
      (drpStg_We_Hl,drpStg),(drpStg_Wir_Hl,drpStg) }
 &
 winch = {
      (grbStg_Wr_Hnv,Wr),(grbStg_Wie_Hnv,Wie),(grbStg_Wr_Hp,Wr),
      (grbStg_We_Hnv,We),(grbStg_Wie_Hp,Wie),(grbStg_Wr_Hv,Wr),
      (grbStg_Wir_Hnv,Wir),(grbStg_We_Hp,We),(grbStg_Wie_Hv,Wie),
      (grbStg_Wr_Hl,Wr),(drpStg_Wr_Hnv,Wr),(grbStg_Wir_Hp,Wir),
      (grbStg_We_Hv,We),(grbStg_Wie_Hl,Wie),(drpStg_Wie_Hnv,Wie),
      (drpStg_Wr_Hp,Wr),(grbStg_Wir_Hv,Wir),(grbStg_We_Hl,We),
      (drpStg_We_Hnv,We),(drpStg_Wie_Hp,Wie),(drpStg_Wr_Hv,Wr),
      (grbStg_Wir_Hl,Wir),(drpStg_Wir_Hnv,Wir),(drpStg_We_Hp,We),
      (drpStg_Wie_Hv,Wie),(drpStg_Wr_Hl,Wr),(drpStg_Wir_Hp,Wir),
      (drpStg_We_Hv,We),(drpStg_Wie_Hl,Wie),(drpStg_Wir_Hv,Wir),
      (drpStg_We_Hl,We),(drpStg_Wir_Hl,Wir) }
 &
 hook = {
      (grbStg_Wr_Hnv,Hnv),(grbStg_Wie_Hnv,Hnv),(grbStg_Wr_Hp,Hp),
      (grbStg_We_Hnv,Hnv),(grbStg_Wie_Hp,Hp),(grbStg_Wr_Hv,Hv),
      (grbStg_Wir_Hnv,Hnv),(grbStg_We_Hp,Hp),(grbStg_Wie_Hv,Hv),
      (grbStg_Wr_Hl,Hl),(drpStg_Wr_Hnv,Hnv),(grbStg_Wir_Hp,Hp),
      (grbStg_We_Hv,Hv),(grbStg_Wie_Hl,Hl),(drpStg_Wie_Hnv,Hnv),
      (drpStg_Wr_Hp,Hp),(grbStg_Wir_Hv,Hv),(grbStg_We_Hl,Hl),
      (drpStg_We_Hnv,Hnv),(drpStg_Wie_Hp,Hp),(drpStg_Wr_Hv,Hv),
      (grbStg_Wir_Hl,Hl),(drpStg_Wir_Hnv,Hnv),(drpStg_We_Hp,Hp),
      (drpStg_Wie_Hv,Hv),(drpStg_Wr_Hl,Hl),(drpStg_Wir_Hp,Hp),
      (drpStg_We_Hv,Hv),(drpStg_Wie_Hl,Hl),(drpStg_Wir_Hv,Hv),
      (drpStg_We_Hl,Hl),(drpStg_Wir_Hl,Hl) }
 &
 BadStates =  States    // to be completed
 &
 Qpred = States - BadStates
END