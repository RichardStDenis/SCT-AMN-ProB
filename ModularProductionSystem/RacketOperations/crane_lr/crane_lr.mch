// copyright Richard St_Denis, Universite de Sherbrooke, 2023.
MODEL SBcrane_lr

SETS
 States = {BMe_Hnv, BMir_Hnv, BMe_Hp,
           BMr_Hnv, BMir_Hp, BMe_Hv,
           BMie_Hnv, BMr_Hp, BMir_Hv,
           BMe_Hl, BMie_Hp, BMr_Hv,
           BMir_Hl, BMie_Hv, BMr_Hl,
           BMie_Hl };
 Events = {BM_eogr, BM_eogl, BM_gr, BM_gl, H_drop, H_eog, H_eod, H_grab};
 LocalStates = {BMe, BMie, BMir, BMr, Hl, Hnv, Hp, Hv}

CONSTANTS
 G_FB,
 boom, hook,
 Qpred, BadStates

PROPERTIES
 G_FB = rec(       // Automaton modeling the free behavior
            states: States,
            events: Events,
            tf: {(BMe_Hnv,BM_gl,BMir_Hnv), (BMe_Hnv,H_grab,BMe_Hp), (BMir_Hnv,BM_eogl,BMr_Hnv),
                 (BMir_Hnv,H_grab,BMir_Hp), (BMe_Hp,BM_gl,BMir_Hp), (BMe_Hp,H_eog,BMe_Hv),
                 (BMr_Hnv,BM_gr,BMie_Hnv), (BMr_Hnv,H_grab,BMr_Hp), (BMir_Hp,BM_eogl,BMr_Hp),
                 (BMir_Hp,H_eog,BMir_Hv), (BMe_Hv,BM_gl,BMir_Hv), (BMe_Hv,H_drop,BMe_Hl),
                 (BMie_Hnv,BM_eogr,BMe_Hnv), (BMie_Hnv,H_grab,BMie_Hp), (BMr_Hp,BM_gr,BMie_Hp),
                 (BMr_Hp,H_eog,BMr_Hv), (BMir_Hv,BM_eogl,BMr_Hv), (BMir_Hv,H_drop,BMir_Hl),
                 (BMe_Hl,BM_gl,BMir_Hl), (BMe_Hl,H_eod,BMe_Hnv), (BMie_Hp,BM_eogr,BMe_Hp),
                 (BMie_Hp,H_eog,BMie_Hv), (BMr_Hv,BM_gr,BMie_Hv), (BMr_Hv,H_drop,BMr_Hl),
                 (BMir_Hl,BM_eogl,BMr_Hl), (BMir_Hl,H_eod,BMir_Hnv), (BMie_Hv,BM_eogr,BMe_Hv),
                 (BMie_Hv,H_drop,BMie_Hl), (BMr_Hl,BM_gr,BMie_Hl), (BMr_Hl,H_eod,BMr_Hnv),
                 (BMie_Hl,BM_eogr,BMe_Hl), (BMie_Hl,H_eod,BMie_Hnv) },
            x_0: BMe_Hnv,
            Xm: {BMe_Hnv},
            ctrlEvents: {BM_gr, BM_gl, H_drop, H_grab} )
 &
 boom = {
      (BMe_Hnv,BMe),(BMir_Hnv,BMir),(BMe_Hp,BMe),
      (BMr_Hnv,BMr),(BMir_Hp,BMir),(BMe_Hv,BMe),
      (BMie_Hnv,BMie),(BMr_Hp,BMr),(BMir_Hv,BMir),
      (BMe_Hl,BMe),(BMie_Hp,BMie),(BMr_Hv,BMr),
      (BMir_Hl,BMir),(BMie_Hv,BMie),(BMr_Hl,BMr),
      (BMie_Hl,BMie) }
 &
 hook = {
      (BMe_Hnv,Hnv),(BMir_Hnv,Hnv),(BMe_Hp,Hp),
      (BMr_Hnv,Hnv),(BMir_Hp,Hp),(BMe_Hv,Hv),
      (BMie_Hnv,Hnv),(BMr_Hp,Hp),(BMir_Hv,Hv),
      (BMe_Hl,Hl),(BMie_Hp,Hp),(BMr_Hv,Hv),
      (BMir_Hl,Hl),(BMie_Hv,Hv),(BMr_Hl,Hl),
      (BMie_Hl,Hl) }
 &
 BadStates =  States    // to be completed
 &
 Qpred = States - BadStates
END