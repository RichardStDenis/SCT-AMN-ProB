// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBdrill

SETS
 States = {isOff_dp_Cr_Mnv_Ar, isOn_dp_Cr_Mnv_Ar, isOff_dp_Cr_Mnv_Anr,
           isOff_dp_Cr_Miv_Ar, isOff_dp_Cie_Mnv_Ar, isOn_dp_Cr_Mnv_Anr,
           isOn_dp_Cr_Miv_Ar, isOn_dp_Cie_Mnv_Ar, isOff_dp_Cr_Miv_Anr,
           isOff_dp_Cie_Mnv_Anr, isOff_fp_Cr_Mnv_Ar, isOff_dp_Cr_Mfv_Ar,
           isOff_dp_Cie_Miv_Ar, isOff_dp_Ce_Mnv_Ar, isOn_fp_Cr_Mnv_Ar,
           isOn_dp_Cr_Miv_Anr, isOn_dp_Cie_Mnv_Anr, isOn_dp_Cr_Mfv_Ar,
           isOn_dp_Cie_Miv_Ar, isOn_dp_Ce_Mnv_Ar, isOff_dp_Cr_Mfv_Anr,
           isOff_dp_Cie_Miv_Anr, isOff_fp_Cr_Miv_Ar, isOff_dp_Ce_Mnv_Anr,
           isOff_fp_Cie_Mnv_Ar, isOff_fp_Cr_Mnv_Anr, isOff_dp_Cr_Mdv_Ar,
           isOff_dp_Cie_Mfv_Ar, isOff_dp_Ce_Miv_Ar, isOff_dp_Cir_Mnv_Ar,
           isOn_fp_Cr_Mnv_Anr, isOn_fp_Cr_Miv_Ar, isOn_fp_Cie_Mnv_Ar,
           isOn_dp_Cr_Mfv_Anr, isOn_dp_Cie_Miv_Anr, isOn_dp_Ce_Mnv_Anr,
           isOn_dp_Cr_Mdv_Ar, isOn_dp_Cie_Mfv_Ar, isOn_dp_Ce_Miv_Ar,
           isOn_dp_Cir_Mnv_Ar, isOff_dp_Cr_Mdv_Anr, isOff_dp_Cie_Mfv_Anr,
           isOff_fp_Cr_Mfv_Ar, isOff_dp_Ce_Miv_Anr, isOff_fp_Cie_Miv_Ar,
           isOff_fp_Cr_Miv_Anr, isOff_dp_Cir_Mnv_Anr, isOff_fp_Ce_Mnv_Ar,
           isOff_fp_Cie_Mnv_Anr, isOff_dp_Cie_Mdv_Ar, isOff_dp_Ce_Mfv_Ar,
           isOff_dp_Cir_Miv_Ar, isOn_fp_Cr_Miv_Anr, isOn_fp_Cie_Mnv_Anr,
           isOn_fp_Cr_Mfv_Ar, isOn_fp_Cie_Miv_Ar, isOn_fp_Ce_Mnv_Ar,
           isOn_dp_Cr_Mdv_Anr, isOn_dp_Cie_Mfv_Anr, isOn_dp_Ce_Miv_Anr,
           isOn_dp_Cir_Mnv_Anr, isOn_dp_Cie_Mdv_Ar, isOn_dp_Ce_Mfv_Ar,
           isOn_dp_Cir_Miv_Ar, isOff_dp_Cie_Mdv_Anr, isOff_fp_Cr_Mdv_Ar,
           isOff_dp_Ce_Mfv_Anr, isOff_fp_Cie_Mfv_Ar, isOff_fp_Cr_Mfv_Anr,
           isOff_dp_Cir_Miv_Anr, isOff_fp_Ce_Miv_Ar, isOff_fp_Cie_Miv_Anr,
           isOff_fp_Cir_Mnv_Ar, isOff_fp_Ce_Mnv_Anr, isOff_dp_Ce_Mdv_Ar,
           isOff_dp_Cir_Mfv_Ar, isOn_fp_Cr_Mfv_Anr, isOn_fp_Cie_Miv_Anr,
           isOn_fp_Ce_Mnv_Anr, isOn_fp_Cr_Mdv_Ar, isOn_fp_Cie_Mfv_Ar,
           isOn_fp_Ce_Miv_Ar, isOn_fp_Cir_Mnv_Ar, isOn_dp_Cie_Mdv_Anr,
           isOn_dp_Ce_Mfv_Anr, isOn_dp_Cir_Miv_Anr, isOn_dp_Ce_Mdv_Ar,
           isOn_dp_Cir_Mfv_Ar, isOff_dp_Ce_Mdv_Anr, isOff_fp_Cie_Mdv_Ar,
           isOff_fp_Cr_Mdv_Anr, isOff_dp_Cir_Mfv_Anr, isOff_fp_Ce_Mfv_Ar,
           isOff_fp_Cie_Mfv_Anr, isOff_fp_Cir_Miv_Ar, isOff_fp_Ce_Miv_Anr,
           isOff_fp_Cir_Mnv_Anr, isOff_dp_Cir_Mdv_Ar, isOn_fp_Cr_Mdv_Anr,
           isOn_fp_Cie_Mfv_Anr, isOn_fp_Ce_Miv_Anr, isOn_fp_Cir_Mnv_Anr,
           isOn_fp_Cie_Mdv_Ar, isOn_fp_Ce_Mfv_Ar, isOn_fp_Cir_Miv_Ar,
           isOn_dp_Ce_Mdv_Anr, isOn_dp_Cir_Mfv_Anr, isOn_dp_Cir_Mdv_Ar,
           isOff_dp_Cir_Mdv_Anr, isOff_fp_Ce_Mdv_Ar, isOff_fp_Cie_Mdv_Anr,
           isOff_fp_Cir_Mfv_Ar, isOff_fp_Ce_Mfv_Anr, isOff_fp_Cir_Miv_Anr,
           isOn_fp_Cie_Mdv_Anr, isOn_fp_Ce_Mfv_Anr, isOn_fp_Cir_Miv_Anr,
           isOn_fp_Ce_Mdv_Ar, isOn_fp_Cir_Mfv_Ar, isOn_dp_Cir_Mdv_Anr,
           isOff_fp_Cir_Mdv_Ar, isOff_fp_Ce_Mdv_Anr, isOff_fp_Cir_Mfv_Anr,
           isOn_fp_Ce_Mdv_Anr, isOn_fp_Cir_Mfv_Anr, isOn_fp_Cir_Mdv_Ar,
           isOff_fp_Cir_Mdv_Anr, isOn_fp_Cir_Mdv_Anr };
 Events = {start, stop, A_eoa, C_eoc, C_eou, C_clamp, C_unclamp, M_tOn, M_tOff, M_fs, M_ns, A_gd};
 LocalStates = {isOff, isOn, dp, fp, Ce, Cie, Cir, Cr, Miv, Mnv, Mfv, Mdv, Anr, Ar}

CONSTANTS
 G_FB,
 switch, operation, clamp, motor, arm,
 Qpred, BadStates

PROPERTIES
 G_FB =rec(   // Automaton modeling the free behavior
      states: States,
      events: Events,
      tf: {(isOff_dp_Cr_Mnv_Ar,start,isOn_dp_Cr_Mnv_Ar), (isOff_dp_Cr_Mnv_Ar,A_gd,isOff_dp_Cr_Mnv_Anr), (isOff_dp_Cr_Mnv_Ar,M_tOn,isOff_dp_Cr_Miv_Ar),
           (isOff_dp_Cr_Mnv_Ar,C_clamp,isOff_dp_Cie_Mnv_Ar), (isOn_dp_Cr_Mnv_Ar,A_gd,isOn_dp_Cr_Mnv_Anr), (isOn_dp_Cr_Mnv_Ar,M_tOn,isOn_dp_Cr_Miv_Ar),
           (isOn_dp_Cr_Mnv_Ar,C_clamp,isOn_dp_Cie_Mnv_Ar), (isOff_dp_Cr_Mnv_Anr,start,isOn_dp_Cr_Mnv_Anr), (isOff_dp_Cr_Mnv_Anr,M_tOn,isOff_dp_Cr_Miv_Anr),
           (isOff_dp_Cr_Mnv_Anr,C_clamp,isOff_dp_Cie_Mnv_Anr), (isOff_dp_Cr_Mnv_Anr,A_eoa,isOff_fp_Cr_Mnv_Ar), (isOff_dp_Cr_Miv_Ar,start,isOn_dp_Cr_Miv_Ar),
           (isOff_dp_Cr_Miv_Ar,A_gd,isOff_dp_Cr_Miv_Anr), (isOff_dp_Cr_Miv_Ar,M_fs,isOff_dp_Cr_Mfv_Ar), (isOff_dp_Cr_Miv_Ar,C_clamp,isOff_dp_Cie_Miv_Ar),
           (isOff_dp_Cie_Mnv_Ar,start,isOn_dp_Cie_Mnv_Ar), (isOff_dp_Cie_Mnv_Ar,A_gd,isOff_dp_Cie_Mnv_Anr), (isOff_dp_Cie_Mnv_Ar,M_tOn,isOff_dp_Cie_Miv_Ar),
           (isOff_dp_Cie_Mnv_Ar,C_eoc,isOff_dp_Ce_Mnv_Ar), (isOn_dp_Cr_Mnv_Anr,A_eoa,isOn_fp_Cr_Mnv_Ar), (isOn_dp_Cr_Mnv_Anr,M_tOn,isOn_dp_Cr_Miv_Anr),
           (isOn_dp_Cr_Mnv_Anr,C_clamp,isOn_dp_Cie_Mnv_Anr), (isOn_dp_Cr_Miv_Ar,A_gd,isOn_dp_Cr_Miv_Anr), (isOn_dp_Cr_Miv_Ar,M_fs,isOn_dp_Cr_Mfv_Ar),
           (isOn_dp_Cr_Miv_Ar,C_clamp,isOn_dp_Cie_Miv_Ar), (isOn_dp_Cie_Mnv_Ar,A_gd,isOn_dp_Cie_Mnv_Anr), (isOn_dp_Cie_Mnv_Ar,M_tOn,isOn_dp_Cie_Miv_Ar),
           (isOn_dp_Cie_Mnv_Ar,C_eoc,isOn_dp_Ce_Mnv_Ar), (isOff_dp_Cr_Miv_Anr,start,isOn_dp_Cr_Miv_Anr), (isOff_dp_Cr_Miv_Anr,M_fs,isOff_dp_Cr_Mfv_Anr),
           (isOff_dp_Cr_Miv_Anr,C_clamp,isOff_dp_Cie_Miv_Anr), (isOff_dp_Cr_Miv_Anr,A_eoa,isOff_fp_Cr_Miv_Ar), (isOff_dp_Cie_Mnv_Anr,start,isOn_dp_Cie_Mnv_Anr),
           (isOff_dp_Cie_Mnv_Anr,M_tOn,isOff_dp_Cie_Miv_Anr), (isOff_dp_Cie_Mnv_Anr,C_eoc,isOff_dp_Ce_Mnv_Anr), (isOff_dp_Cie_Mnv_Anr,A_eoa,isOff_fp_Cie_Mnv_Ar),
           (isOff_fp_Cr_Mnv_Ar,start,isOn_fp_Cr_Mnv_Ar), (isOff_fp_Cr_Mnv_Ar,A_gd,isOff_fp_Cr_Mnv_Anr), (isOff_fp_Cr_Mnv_Ar,M_tOn,isOff_fp_Cr_Miv_Ar),
           (isOff_fp_Cr_Mnv_Ar,C_clamp,isOff_fp_Cie_Mnv_Ar), (isOff_dp_Cr_Mfv_Ar,start,isOn_dp_Cr_Mfv_Ar), (isOff_dp_Cr_Mfv_Ar,A_gd,isOff_dp_Cr_Mfv_Anr),
           (isOff_dp_Cr_Mfv_Ar,M_tOff,isOff_dp_Cr_Mdv_Ar), (isOff_dp_Cr_Mfv_Ar,C_clamp,isOff_dp_Cie_Mfv_Ar), (isOff_dp_Cie_Miv_Ar,start,isOn_dp_Cie_Miv_Ar),
           (isOff_dp_Cie_Miv_Ar,A_gd,isOff_dp_Cie_Miv_Anr), (isOff_dp_Cie_Miv_Ar,M_fs,isOff_dp_Cie_Mfv_Ar), (isOff_dp_Cie_Miv_Ar,C_eoc,isOff_dp_Ce_Miv_Ar),
           (isOff_dp_Ce_Mnv_Ar,start,isOn_dp_Ce_Mnv_Ar), (isOff_dp_Ce_Mnv_Ar,A_gd,isOff_dp_Ce_Mnv_Anr), (isOff_dp_Ce_Mnv_Ar,M_tOn,isOff_dp_Ce_Miv_Ar),
           (isOff_dp_Ce_Mnv_Ar,C_unclamp,isOff_dp_Cir_Mnv_Ar), (isOn_fp_Cr_Mnv_Ar,A_gd,isOn_fp_Cr_Mnv_Anr), (isOn_fp_Cr_Mnv_Ar,M_tOn,isOn_fp_Cr_Miv_Ar),
           (isOn_fp_Cr_Mnv_Ar,C_clamp,isOn_fp_Cie_Mnv_Ar), (isOn_fp_Cr_Mnv_Ar,stop,isOff_dp_Cr_Mnv_Ar), (isOn_dp_Cr_Miv_Anr,A_eoa,isOn_fp_Cr_Miv_Ar),
           (isOn_dp_Cr_Miv_Anr,M_fs,isOn_dp_Cr_Mfv_Anr), (isOn_dp_Cr_Miv_Anr,C_clamp,isOn_dp_Cie_Miv_Anr), (isOn_dp_Cie_Mnv_Anr,A_eoa,isOn_fp_Cie_Mnv_Ar),
           (isOn_dp_Cie_Mnv_Anr,M_tOn,isOn_dp_Cie_Miv_Anr), (isOn_dp_Cie_Mnv_Anr,C_eoc,isOn_dp_Ce_Mnv_Anr), (isOn_dp_Cr_Mfv_Ar,A_gd,isOn_dp_Cr_Mfv_Anr),
           (isOn_dp_Cr_Mfv_Ar,M_tOff,isOn_dp_Cr_Mdv_Ar), (isOn_dp_Cr_Mfv_Ar,C_clamp,isOn_dp_Cie_Mfv_Ar), (isOn_dp_Cie_Miv_Ar,A_gd,isOn_dp_Cie_Miv_Anr),
           (isOn_dp_Cie_Miv_Ar,M_fs,isOn_dp_Cie_Mfv_Ar), (isOn_dp_Cie_Miv_Ar,C_eoc,isOn_dp_Ce_Miv_Ar), (isOn_dp_Ce_Mnv_Ar,A_gd,isOn_dp_Ce_Mnv_Anr),
           (isOn_dp_Ce_Mnv_Ar,M_tOn,isOn_dp_Ce_Miv_Ar), (isOn_dp_Ce_Mnv_Ar,C_unclamp,isOn_dp_Cir_Mnv_Ar), (isOff_dp_Cr_Mfv_Anr,start,isOn_dp_Cr_Mfv_Anr),
           (isOff_dp_Cr_Mfv_Anr,M_tOff,isOff_dp_Cr_Mdv_Anr), (isOff_dp_Cr_Mfv_Anr,C_clamp,isOff_dp_Cie_Mfv_Anr), (isOff_dp_Cr_Mfv_Anr,A_eoa,isOff_fp_Cr_Mfv_Ar),
           (isOff_dp_Cie_Miv_Anr,start,isOn_dp_Cie_Miv_Anr), (isOff_dp_Cie_Miv_Anr,M_fs,isOff_dp_Cie_Mfv_Anr), (isOff_dp_Cie_Miv_Anr,C_eoc,isOff_dp_Ce_Miv_Anr),
           (isOff_dp_Cie_Miv_Anr,A_eoa,isOff_fp_Cie_Miv_Ar), (isOff_fp_Cr_Miv_Ar,start,isOn_fp_Cr_Miv_Ar), (isOff_fp_Cr_Miv_Ar,A_gd,isOff_fp_Cr_Miv_Anr),
           (isOff_fp_Cr_Miv_Ar,M_fs,isOff_fp_Cr_Mfv_Ar), (isOff_fp_Cr_Miv_Ar,C_clamp,isOff_fp_Cie_Miv_Ar), (isOff_dp_Ce_Mnv_Anr,start,isOn_dp_Ce_Mnv_Anr),
           (isOff_dp_Ce_Mnv_Anr,M_tOn,isOff_dp_Ce_Miv_Anr), (isOff_dp_Ce_Mnv_Anr,C_unclamp,isOff_dp_Cir_Mnv_Anr), (isOff_dp_Ce_Mnv_Anr,A_eoa,isOff_fp_Ce_Mnv_Ar),
           (isOff_fp_Cie_Mnv_Ar,start,isOn_fp_Cie_Mnv_Ar), (isOff_fp_Cie_Mnv_Ar,A_gd,isOff_fp_Cie_Mnv_Anr), (isOff_fp_Cie_Mnv_Ar,M_tOn,isOff_fp_Cie_Miv_Ar),
           (isOff_fp_Cie_Mnv_Ar,C_eoc,isOff_fp_Ce_Mnv_Ar), (isOff_fp_Cr_Mnv_Anr,start,isOn_fp_Cr_Mnv_Anr), (isOff_fp_Cr_Mnv_Anr,M_tOn,isOff_fp_Cr_Miv_Anr),
           (isOff_fp_Cr_Mnv_Anr,C_clamp,isOff_fp_Cie_Mnv_Anr), (isOff_fp_Cr_Mnv_Anr,A_eoa,isOff_fp_Cr_Mnv_Ar), (isOff_dp_Cr_Mdv_Ar,start,isOn_dp_Cr_Mdv_Ar),
           (isOff_dp_Cr_Mdv_Ar,A_gd,isOff_dp_Cr_Mdv_Anr), (isOff_dp_Cr_Mdv_Ar,M_ns,isOff_dp_Cr_Mnv_Ar), (isOff_dp_Cr_Mdv_Ar,C_clamp,isOff_dp_Cie_Mdv_Ar),
           (isOff_dp_Cie_Mfv_Ar,start,isOn_dp_Cie_Mfv_Ar), (isOff_dp_Cie_Mfv_Ar,A_gd,isOff_dp_Cie_Mfv_Anr), (isOff_dp_Cie_Mfv_Ar,M_tOff,isOff_dp_Cie_Mdv_Ar),
           (isOff_dp_Cie_Mfv_Ar,C_eoc,isOff_dp_Ce_Mfv_Ar), (isOff_dp_Ce_Miv_Ar,start,isOn_dp_Ce_Miv_Ar), (isOff_dp_Ce_Miv_Ar,A_gd,isOff_dp_Ce_Miv_Anr),
           (isOff_dp_Ce_Miv_Ar,M_fs,isOff_dp_Ce_Mfv_Ar), (isOff_dp_Ce_Miv_Ar,C_unclamp,isOff_dp_Cir_Miv_Ar), (isOff_dp_Cir_Mnv_Ar,start,isOn_dp_Cir_Mnv_Ar),
           (isOff_dp_Cir_Mnv_Ar,A_gd,isOff_dp_Cir_Mnv_Anr), (isOff_dp_Cir_Mnv_Ar,M_tOn,isOff_dp_Cir_Miv_Ar), (isOff_dp_Cir_Mnv_Ar,C_eou,isOff_dp_Cr_Mnv_Ar),
           (isOn_fp_Cr_Mnv_Anr,A_eoa,isOn_fp_Cr_Mnv_Ar), (isOn_fp_Cr_Mnv_Anr,M_tOn,isOn_fp_Cr_Miv_Anr), (isOn_fp_Cr_Mnv_Anr,C_clamp,isOn_fp_Cie_Mnv_Anr),
           (isOn_fp_Cr_Mnv_Anr,stop,isOff_dp_Cr_Mnv_Anr), (isOn_fp_Cr_Miv_Ar,A_gd,isOn_fp_Cr_Miv_Anr), (isOn_fp_Cr_Miv_Ar,M_fs,isOn_fp_Cr_Mfv_Ar),
           (isOn_fp_Cr_Miv_Ar,C_clamp,isOn_fp_Cie_Miv_Ar), (isOn_fp_Cr_Miv_Ar,stop,isOff_dp_Cr_Miv_Ar), (isOn_fp_Cie_Mnv_Ar,A_gd,isOn_fp_Cie_Mnv_Anr),
           (isOn_fp_Cie_Mnv_Ar,M_tOn,isOn_fp_Cie_Miv_Ar), (isOn_fp_Cie_Mnv_Ar,C_eoc,isOn_fp_Ce_Mnv_Ar), (isOn_fp_Cie_Mnv_Ar,stop,isOff_dp_Cie_Mnv_Ar),
           (isOn_dp_Cr_Mfv_Anr,A_eoa,isOn_fp_Cr_Mfv_Ar), (isOn_dp_Cr_Mfv_Anr,M_tOff,isOn_dp_Cr_Mdv_Anr), (isOn_dp_Cr_Mfv_Anr,C_clamp,isOn_dp_Cie_Mfv_Anr),
           (isOn_dp_Cie_Miv_Anr,A_eoa,isOn_fp_Cie_Miv_Ar), (isOn_dp_Cie_Miv_Anr,M_fs,isOn_dp_Cie_Mfv_Anr), (isOn_dp_Cie_Miv_Anr,C_eoc,isOn_dp_Ce_Miv_Anr),
           (isOn_dp_Ce_Mnv_Anr,A_eoa,isOn_fp_Ce_Mnv_Ar), (isOn_dp_Ce_Mnv_Anr,M_tOn,isOn_dp_Ce_Miv_Anr), (isOn_dp_Ce_Mnv_Anr,C_unclamp,isOn_dp_Cir_Mnv_Anr),
           (isOn_dp_Cr_Mdv_Ar,A_gd,isOn_dp_Cr_Mdv_Anr), (isOn_dp_Cr_Mdv_Ar,M_ns,isOn_dp_Cr_Mnv_Ar), (isOn_dp_Cr_Mdv_Ar,C_clamp,isOn_dp_Cie_Mdv_Ar),
           (isOn_dp_Cie_Mfv_Ar,A_gd,isOn_dp_Cie_Mfv_Anr), (isOn_dp_Cie_Mfv_Ar,M_tOff,isOn_dp_Cie_Mdv_Ar), (isOn_dp_Cie_Mfv_Ar,C_eoc,isOn_dp_Ce_Mfv_Ar),
           (isOn_dp_Ce_Miv_Ar,A_gd,isOn_dp_Ce_Miv_Anr), (isOn_dp_Ce_Miv_Ar,M_fs,isOn_dp_Ce_Mfv_Ar), (isOn_dp_Ce_Miv_Ar,C_unclamp,isOn_dp_Cir_Miv_Ar),
           (isOn_dp_Cir_Mnv_Ar,A_gd,isOn_dp_Cir_Mnv_Anr), (isOn_dp_Cir_Mnv_Ar,M_tOn,isOn_dp_Cir_Miv_Ar), (isOn_dp_Cir_Mnv_Ar,C_eou,isOn_dp_Cr_Mnv_Ar),
           (isOff_dp_Cr_Mdv_Anr,start,isOn_dp_Cr_Mdv_Anr), (isOff_dp_Cr_Mdv_Anr,M_ns,isOff_dp_Cr_Mnv_Anr), (isOff_dp_Cr_Mdv_Anr,C_clamp,isOff_dp_Cie_Mdv_Anr),
           (isOff_dp_Cr_Mdv_Anr,A_eoa,isOff_fp_Cr_Mdv_Ar), (isOff_dp_Cie_Mfv_Anr,start,isOn_dp_Cie_Mfv_Anr), (isOff_dp_Cie_Mfv_Anr,M_tOff,isOff_dp_Cie_Mdv_Anr),
           (isOff_dp_Cie_Mfv_Anr,C_eoc,isOff_dp_Ce_Mfv_Anr), (isOff_dp_Cie_Mfv_Anr,A_eoa,isOff_fp_Cie_Mfv_Ar), (isOff_fp_Cr_Mfv_Ar,start,isOn_fp_Cr_Mfv_Ar),
           (isOff_fp_Cr_Mfv_Ar,A_gd,isOff_fp_Cr_Mfv_Anr), (isOff_fp_Cr_Mfv_Ar,M_tOff,isOff_fp_Cr_Mdv_Ar), (isOff_fp_Cr_Mfv_Ar,C_clamp,isOff_fp_Cie_Mfv_Ar),
           (isOff_dp_Ce_Miv_Anr,start,isOn_dp_Ce_Miv_Anr), (isOff_dp_Ce_Miv_Anr,M_fs,isOff_dp_Ce_Mfv_Anr), (isOff_dp_Ce_Miv_Anr,C_unclamp,isOff_dp_Cir_Miv_Anr),
           (isOff_dp_Ce_Miv_Anr,A_eoa,isOff_fp_Ce_Miv_Ar), (isOff_fp_Cie_Miv_Ar,start,isOn_fp_Cie_Miv_Ar), (isOff_fp_Cie_Miv_Ar,A_gd,isOff_fp_Cie_Miv_Anr),
           (isOff_fp_Cie_Miv_Ar,M_fs,isOff_fp_Cie_Mfv_Ar), (isOff_fp_Cie_Miv_Ar,C_eoc,isOff_fp_Ce_Miv_Ar), (isOff_fp_Cr_Miv_Anr,start,isOn_fp_Cr_Miv_Anr),
           (isOff_fp_Cr_Miv_Anr,M_fs,isOff_fp_Cr_Mfv_Anr), (isOff_fp_Cr_Miv_Anr,C_clamp,isOff_fp_Cie_Miv_Anr), (isOff_fp_Cr_Miv_Anr,A_eoa,isOff_fp_Cr_Miv_Ar),
           (isOff_dp_Cir_Mnv_Anr,start,isOn_dp_Cir_Mnv_Anr), (isOff_dp_Cir_Mnv_Anr,M_tOn,isOff_dp_Cir_Miv_Anr), (isOff_dp_Cir_Mnv_Anr,C_eou,isOff_dp_Cr_Mnv_Anr),
           (isOff_dp_Cir_Mnv_Anr,A_eoa,isOff_fp_Cir_Mnv_Ar), (isOff_fp_Ce_Mnv_Ar,start,isOn_fp_Ce_Mnv_Ar), (isOff_fp_Ce_Mnv_Ar,A_gd,isOff_fp_Ce_Mnv_Anr),
           (isOff_fp_Ce_Mnv_Ar,M_tOn,isOff_fp_Ce_Miv_Ar), (isOff_fp_Ce_Mnv_Ar,C_unclamp,isOff_fp_Cir_Mnv_Ar), (isOff_fp_Cie_Mnv_Anr,start,isOn_fp_Cie_Mnv_Anr),
           (isOff_fp_Cie_Mnv_Anr,M_tOn,isOff_fp_Cie_Miv_Anr), (isOff_fp_Cie_Mnv_Anr,C_eoc,isOff_fp_Ce_Mnv_Anr), (isOff_fp_Cie_Mnv_Anr,A_eoa,isOff_fp_Cie_Mnv_Ar),
           (isOff_dp_Cie_Mdv_Ar,start,isOn_dp_Cie_Mdv_Ar), (isOff_dp_Cie_Mdv_Ar,A_gd,isOff_dp_Cie_Mdv_Anr), (isOff_dp_Cie_Mdv_Ar,M_ns,isOff_dp_Cie_Mnv_Ar),
           (isOff_dp_Cie_Mdv_Ar,C_eoc,isOff_dp_Ce_Mdv_Ar), (isOff_dp_Ce_Mfv_Ar,start,isOn_dp_Ce_Mfv_Ar), (isOff_dp_Ce_Mfv_Ar,A_gd,isOff_dp_Ce_Mfv_Anr),
           (isOff_dp_Ce_Mfv_Ar,M_tOff,isOff_dp_Ce_Mdv_Ar), (isOff_dp_Ce_Mfv_Ar,C_unclamp,isOff_dp_Cir_Mfv_Ar), (isOff_dp_Cir_Miv_Ar,start,isOn_dp_Cir_Miv_Ar),
           (isOff_dp_Cir_Miv_Ar,A_gd,isOff_dp_Cir_Miv_Anr), (isOff_dp_Cir_Miv_Ar,M_fs,isOff_dp_Cir_Mfv_Ar), (isOff_dp_Cir_Miv_Ar,C_eou,isOff_dp_Cr_Miv_Ar),
           (isOn_fp_Cr_Miv_Anr,A_eoa,isOn_fp_Cr_Miv_Ar), (isOn_fp_Cr_Miv_Anr,M_fs,isOn_fp_Cr_Mfv_Anr), (isOn_fp_Cr_Miv_Anr,C_clamp,isOn_fp_Cie_Miv_Anr),
           (isOn_fp_Cr_Miv_Anr,stop,isOff_dp_Cr_Miv_Anr), (isOn_fp_Cie_Mnv_Anr,A_eoa,isOn_fp_Cie_Mnv_Ar), (isOn_fp_Cie_Mnv_Anr,M_tOn,isOn_fp_Cie_Miv_Anr),
           (isOn_fp_Cie_Mnv_Anr,C_eoc,isOn_fp_Ce_Mnv_Anr), (isOn_fp_Cie_Mnv_Anr,stop,isOff_dp_Cie_Mnv_Anr), (isOn_fp_Cr_Mfv_Ar,A_gd,isOn_fp_Cr_Mfv_Anr),
           (isOn_fp_Cr_Mfv_Ar,M_tOff,isOn_fp_Cr_Mdv_Ar), (isOn_fp_Cr_Mfv_Ar,C_clamp,isOn_fp_Cie_Mfv_Ar), (isOn_fp_Cr_Mfv_Ar,stop,isOff_dp_Cr_Mfv_Ar),
           (isOn_fp_Cie_Miv_Ar,A_gd,isOn_fp_Cie_Miv_Anr), (isOn_fp_Cie_Miv_Ar,M_fs,isOn_fp_Cie_Mfv_Ar), (isOn_fp_Cie_Miv_Ar,C_eoc,isOn_fp_Ce_Miv_Ar),
           (isOn_fp_Cie_Miv_Ar,stop,isOff_dp_Cie_Miv_Ar), (isOn_fp_Ce_Mnv_Ar,A_gd,isOn_fp_Ce_Mnv_Anr), (isOn_fp_Ce_Mnv_Ar,M_tOn,isOn_fp_Ce_Miv_Ar),
           (isOn_fp_Ce_Mnv_Ar,C_unclamp,isOn_fp_Cir_Mnv_Ar), (isOn_fp_Ce_Mnv_Ar,stop,isOff_dp_Ce_Mnv_Ar), (isOn_dp_Cr_Mdv_Anr,A_eoa,isOn_fp_Cr_Mdv_Ar),
           (isOn_dp_Cr_Mdv_Anr,M_ns,isOn_dp_Cr_Mnv_Anr), (isOn_dp_Cr_Mdv_Anr,C_clamp,isOn_dp_Cie_Mdv_Anr), (isOn_dp_Cie_Mfv_Anr,A_eoa,isOn_fp_Cie_Mfv_Ar),
           (isOn_dp_Cie_Mfv_Anr,M_tOff,isOn_dp_Cie_Mdv_Anr), (isOn_dp_Cie_Mfv_Anr,C_eoc,isOn_dp_Ce_Mfv_Anr), (isOn_dp_Ce_Miv_Anr,A_eoa,isOn_fp_Ce_Miv_Ar),
           (isOn_dp_Ce_Miv_Anr,M_fs,isOn_dp_Ce_Mfv_Anr), (isOn_dp_Ce_Miv_Anr,C_unclamp,isOn_dp_Cir_Miv_Anr), (isOn_dp_Cir_Mnv_Anr,A_eoa,isOn_fp_Cir_Mnv_Ar),
           (isOn_dp_Cir_Mnv_Anr,M_tOn,isOn_dp_Cir_Miv_Anr), (isOn_dp_Cir_Mnv_Anr,C_eou,isOn_dp_Cr_Mnv_Anr), (isOn_dp_Cie_Mdv_Ar,A_gd,isOn_dp_Cie_Mdv_Anr),
           (isOn_dp_Cie_Mdv_Ar,M_ns,isOn_dp_Cie_Mnv_Ar), (isOn_dp_Cie_Mdv_Ar,C_eoc,isOn_dp_Ce_Mdv_Ar), (isOn_dp_Ce_Mfv_Ar,A_gd,isOn_dp_Ce_Mfv_Anr),
           (isOn_dp_Ce_Mfv_Ar,M_tOff,isOn_dp_Ce_Mdv_Ar), (isOn_dp_Ce_Mfv_Ar,C_unclamp,isOn_dp_Cir_Mfv_Ar), (isOn_dp_Cir_Miv_Ar,A_gd,isOn_dp_Cir_Miv_Anr),
           (isOn_dp_Cir_Miv_Ar,M_fs,isOn_dp_Cir_Mfv_Ar), (isOn_dp_Cir_Miv_Ar,C_eou,isOn_dp_Cr_Miv_Ar), (isOff_dp_Cie_Mdv_Anr,start,isOn_dp_Cie_Mdv_Anr),
           (isOff_dp_Cie_Mdv_Anr,M_ns,isOff_dp_Cie_Mnv_Anr), (isOff_dp_Cie_Mdv_Anr,C_eoc,isOff_dp_Ce_Mdv_Anr), (isOff_dp_Cie_Mdv_Anr,A_eoa,isOff_fp_Cie_Mdv_Ar),
           (isOff_fp_Cr_Mdv_Ar,start,isOn_fp_Cr_Mdv_Ar), (isOff_fp_Cr_Mdv_Ar,A_gd,isOff_fp_Cr_Mdv_Anr), (isOff_fp_Cr_Mdv_Ar,M_ns,isOff_fp_Cr_Mnv_Ar),
           (isOff_fp_Cr_Mdv_Ar,C_clamp,isOff_fp_Cie_Mdv_Ar), (isOff_dp_Ce_Mfv_Anr,start,isOn_dp_Ce_Mfv_Anr), (isOff_dp_Ce_Mfv_Anr,M_tOff,isOff_dp_Ce_Mdv_Anr),
           (isOff_dp_Ce_Mfv_Anr,C_unclamp,isOff_dp_Cir_Mfv_Anr), (isOff_dp_Ce_Mfv_Anr,A_eoa,isOff_fp_Ce_Mfv_Ar), (isOff_fp_Cie_Mfv_Ar,start,isOn_fp_Cie_Mfv_Ar),
           (isOff_fp_Cie_Mfv_Ar,A_gd,isOff_fp_Cie_Mfv_Anr), (isOff_fp_Cie_Mfv_Ar,M_tOff,isOff_fp_Cie_Mdv_Ar), (isOff_fp_Cie_Mfv_Ar,C_eoc,isOff_fp_Ce_Mfv_Ar),
           (isOff_fp_Cr_Mfv_Anr,start,isOn_fp_Cr_Mfv_Anr), (isOff_fp_Cr_Mfv_Anr,M_tOff,isOff_fp_Cr_Mdv_Anr), (isOff_fp_Cr_Mfv_Anr,C_clamp,isOff_fp_Cie_Mfv_Anr),
           (isOff_fp_Cr_Mfv_Anr,A_eoa,isOff_fp_Cr_Mfv_Ar), (isOff_dp_Cir_Miv_Anr,start,isOn_dp_Cir_Miv_Anr), (isOff_dp_Cir_Miv_Anr,M_fs,isOff_dp_Cir_Mfv_Anr),
           (isOff_dp_Cir_Miv_Anr,C_eou,isOff_dp_Cr_Miv_Anr), (isOff_dp_Cir_Miv_Anr,A_eoa,isOff_fp_Cir_Miv_Ar), (isOff_fp_Ce_Miv_Ar,start,isOn_fp_Ce_Miv_Ar),
           (isOff_fp_Ce_Miv_Ar,A_gd,isOff_fp_Ce_Miv_Anr), (isOff_fp_Ce_Miv_Ar,M_fs,isOff_fp_Ce_Mfv_Ar), (isOff_fp_Ce_Miv_Ar,C_unclamp,isOff_fp_Cir_Miv_Ar),
           (isOff_fp_Cie_Miv_Anr,start,isOn_fp_Cie_Miv_Anr), (isOff_fp_Cie_Miv_Anr,M_fs,isOff_fp_Cie_Mfv_Anr), (isOff_fp_Cie_Miv_Anr,C_eoc,isOff_fp_Ce_Miv_Anr),
           (isOff_fp_Cie_Miv_Anr,A_eoa,isOff_fp_Cie_Miv_Ar), (isOff_fp_Cir_Mnv_Ar,start,isOn_fp_Cir_Mnv_Ar), (isOff_fp_Cir_Mnv_Ar,A_gd,isOff_fp_Cir_Mnv_Anr),
           (isOff_fp_Cir_Mnv_Ar,M_tOn,isOff_fp_Cir_Miv_Ar), (isOff_fp_Cir_Mnv_Ar,C_eou,isOff_fp_Cr_Mnv_Ar), (isOff_fp_Ce_Mnv_Anr,start,isOn_fp_Ce_Mnv_Anr),
           (isOff_fp_Ce_Mnv_Anr,M_tOn,isOff_fp_Ce_Miv_Anr), (isOff_fp_Ce_Mnv_Anr,C_unclamp,isOff_fp_Cir_Mnv_Anr), (isOff_fp_Ce_Mnv_Anr,A_eoa,isOff_fp_Ce_Mnv_Ar),
           (isOff_dp_Ce_Mdv_Ar,start,isOn_dp_Ce_Mdv_Ar), (isOff_dp_Ce_Mdv_Ar,A_gd,isOff_dp_Ce_Mdv_Anr), (isOff_dp_Ce_Mdv_Ar,M_ns,isOff_dp_Ce_Mnv_Ar),
           (isOff_dp_Ce_Mdv_Ar,C_unclamp,isOff_dp_Cir_Mdv_Ar), (isOff_dp_Cir_Mfv_Ar,start,isOn_dp_Cir_Mfv_Ar), (isOff_dp_Cir_Mfv_Ar,A_gd,isOff_dp_Cir_Mfv_Anr),
           (isOff_dp_Cir_Mfv_Ar,M_tOff,isOff_dp_Cir_Mdv_Ar), (isOff_dp_Cir_Mfv_Ar,C_eou,isOff_dp_Cr_Mfv_Ar), (isOn_fp_Cr_Mfv_Anr,A_eoa,isOn_fp_Cr_Mfv_Ar),
           (isOn_fp_Cr_Mfv_Anr,M_tOff,isOn_fp_Cr_Mdv_Anr), (isOn_fp_Cr_Mfv_Anr,C_clamp,isOn_fp_Cie_Mfv_Anr), (isOn_fp_Cr_Mfv_Anr,stop,isOff_dp_Cr_Mfv_Anr),
           (isOn_fp_Cie_Miv_Anr,A_eoa,isOn_fp_Cie_Miv_Ar), (isOn_fp_Cie_Miv_Anr,M_fs,isOn_fp_Cie_Mfv_Anr), (isOn_fp_Cie_Miv_Anr,C_eoc,isOn_fp_Ce_Miv_Anr),
           (isOn_fp_Cie_Miv_Anr,stop,isOff_dp_Cie_Miv_Anr), (isOn_fp_Ce_Mnv_Anr,A_eoa,isOn_fp_Ce_Mnv_Ar), (isOn_fp_Ce_Mnv_Anr,M_tOn,isOn_fp_Ce_Miv_Anr),
           (isOn_fp_Ce_Mnv_Anr,C_unclamp,isOn_fp_Cir_Mnv_Anr), (isOn_fp_Ce_Mnv_Anr,stop,isOff_dp_Ce_Mnv_Anr), (isOn_fp_Cr_Mdv_Ar,A_gd,isOn_fp_Cr_Mdv_Anr),
           (isOn_fp_Cr_Mdv_Ar,M_ns,isOn_fp_Cr_Mnv_Ar), (isOn_fp_Cr_Mdv_Ar,C_clamp,isOn_fp_Cie_Mdv_Ar), (isOn_fp_Cr_Mdv_Ar,stop,isOff_dp_Cr_Mdv_Ar),
           (isOn_fp_Cie_Mfv_Ar,A_gd,isOn_fp_Cie_Mfv_Anr), (isOn_fp_Cie_Mfv_Ar,M_tOff,isOn_fp_Cie_Mdv_Ar), (isOn_fp_Cie_Mfv_Ar,C_eoc,isOn_fp_Ce_Mfv_Ar),
           (isOn_fp_Cie_Mfv_Ar,stop,isOff_dp_Cie_Mfv_Ar), (isOn_fp_Ce_Miv_Ar,A_gd,isOn_fp_Ce_Miv_Anr), (isOn_fp_Ce_Miv_Ar,M_fs,isOn_fp_Ce_Mfv_Ar),
           (isOn_fp_Ce_Miv_Ar,C_unclamp,isOn_fp_Cir_Miv_Ar), (isOn_fp_Ce_Miv_Ar,stop,isOff_dp_Ce_Miv_Ar), (isOn_fp_Cir_Mnv_Ar,A_gd,isOn_fp_Cir_Mnv_Anr),
           (isOn_fp_Cir_Mnv_Ar,M_tOn,isOn_fp_Cir_Miv_Ar), (isOn_fp_Cir_Mnv_Ar,C_eou,isOn_fp_Cr_Mnv_Ar), (isOn_fp_Cir_Mnv_Ar,stop,isOff_dp_Cir_Mnv_Ar),
           (isOn_dp_Cie_Mdv_Anr,A_eoa,isOn_fp_Cie_Mdv_Ar), (isOn_dp_Cie_Mdv_Anr,M_ns,isOn_dp_Cie_Mnv_Anr), (isOn_dp_Cie_Mdv_Anr,C_eoc,isOn_dp_Ce_Mdv_Anr),
           (isOn_dp_Ce_Mfv_Anr,A_eoa,isOn_fp_Ce_Mfv_Ar), (isOn_dp_Ce_Mfv_Anr,M_tOff,isOn_dp_Ce_Mdv_Anr), (isOn_dp_Ce_Mfv_Anr,C_unclamp,isOn_dp_Cir_Mfv_Anr),
           (isOn_dp_Cir_Miv_Anr,A_eoa,isOn_fp_Cir_Miv_Ar), (isOn_dp_Cir_Miv_Anr,M_fs,isOn_dp_Cir_Mfv_Anr), (isOn_dp_Cir_Miv_Anr,C_eou,isOn_dp_Cr_Miv_Anr),
           (isOn_dp_Ce_Mdv_Ar,A_gd,isOn_dp_Ce_Mdv_Anr), (isOn_dp_Ce_Mdv_Ar,M_ns,isOn_dp_Ce_Mnv_Ar), (isOn_dp_Ce_Mdv_Ar,C_unclamp,isOn_dp_Cir_Mdv_Ar),
           (isOn_dp_Cir_Mfv_Ar,A_gd,isOn_dp_Cir_Mfv_Anr), (isOn_dp_Cir_Mfv_Ar,M_tOff,isOn_dp_Cir_Mdv_Ar), (isOn_dp_Cir_Mfv_Ar,C_eou,isOn_dp_Cr_Mfv_Ar),
           (isOff_dp_Ce_Mdv_Anr,start,isOn_dp_Ce_Mdv_Anr), (isOff_dp_Ce_Mdv_Anr,M_ns,isOff_dp_Ce_Mnv_Anr), (isOff_dp_Ce_Mdv_Anr,C_unclamp,isOff_dp_Cir_Mdv_Anr),
           (isOff_dp_Ce_Mdv_Anr,A_eoa,isOff_fp_Ce_Mdv_Ar), (isOff_fp_Cie_Mdv_Ar,start,isOn_fp_Cie_Mdv_Ar), (isOff_fp_Cie_Mdv_Ar,A_gd,isOff_fp_Cie_Mdv_Anr),
           (isOff_fp_Cie_Mdv_Ar,M_ns,isOff_fp_Cie_Mnv_Ar), (isOff_fp_Cie_Mdv_Ar,C_eoc,isOff_fp_Ce_Mdv_Ar), (isOff_fp_Cr_Mdv_Anr,start,isOn_fp_Cr_Mdv_Anr),
           (isOff_fp_Cr_Mdv_Anr,M_ns,isOff_fp_Cr_Mnv_Anr), (isOff_fp_Cr_Mdv_Anr,C_clamp,isOff_fp_Cie_Mdv_Anr), (isOff_fp_Cr_Mdv_Anr,A_eoa,isOff_fp_Cr_Mdv_Ar),
           (isOff_dp_Cir_Mfv_Anr,start,isOn_dp_Cir_Mfv_Anr), (isOff_dp_Cir_Mfv_Anr,M_tOff,isOff_dp_Cir_Mdv_Anr), (isOff_dp_Cir_Mfv_Anr,C_eou,isOff_dp_Cr_Mfv_Anr),
           (isOff_dp_Cir_Mfv_Anr,A_eoa,isOff_fp_Cir_Mfv_Ar), (isOff_fp_Ce_Mfv_Ar,start,isOn_fp_Ce_Mfv_Ar), (isOff_fp_Ce_Mfv_Ar,A_gd,isOff_fp_Ce_Mfv_Anr),
           (isOff_fp_Ce_Mfv_Ar,M_tOff,isOff_fp_Ce_Mdv_Ar), (isOff_fp_Ce_Mfv_Ar,C_unclamp,isOff_fp_Cir_Mfv_Ar), (isOff_fp_Cie_Mfv_Anr,start,isOn_fp_Cie_Mfv_Anr),
           (isOff_fp_Cie_Mfv_Anr,M_tOff,isOff_fp_Cie_Mdv_Anr), (isOff_fp_Cie_Mfv_Anr,C_eoc,isOff_fp_Ce_Mfv_Anr), (isOff_fp_Cie_Mfv_Anr,A_eoa,isOff_fp_Cie_Mfv_Ar),
           (isOff_fp_Cir_Miv_Ar,start,isOn_fp_Cir_Miv_Ar), (isOff_fp_Cir_Miv_Ar,A_gd,isOff_fp_Cir_Miv_Anr), (isOff_fp_Cir_Miv_Ar,M_fs,isOff_fp_Cir_Mfv_Ar),
           (isOff_fp_Cir_Miv_Ar,C_eou,isOff_fp_Cr_Miv_Ar), (isOff_fp_Ce_Miv_Anr,start,isOn_fp_Ce_Miv_Anr), (isOff_fp_Ce_Miv_Anr,M_fs,isOff_fp_Ce_Mfv_Anr),
           (isOff_fp_Ce_Miv_Anr,C_unclamp,isOff_fp_Cir_Miv_Anr), (isOff_fp_Ce_Miv_Anr,A_eoa,isOff_fp_Ce_Miv_Ar), (isOff_fp_Cir_Mnv_Anr,start,isOn_fp_Cir_Mnv_Anr),
           (isOff_fp_Cir_Mnv_Anr,M_tOn,isOff_fp_Cir_Miv_Anr), (isOff_fp_Cir_Mnv_Anr,C_eou,isOff_fp_Cr_Mnv_Anr), (isOff_fp_Cir_Mnv_Anr,A_eoa,isOff_fp_Cir_Mnv_Ar),
           (isOff_dp_Cir_Mdv_Ar,start,isOn_dp_Cir_Mdv_Ar), (isOff_dp_Cir_Mdv_Ar,A_gd,isOff_dp_Cir_Mdv_Anr), (isOff_dp_Cir_Mdv_Ar,M_ns,isOff_dp_Cir_Mnv_Ar),
           (isOff_dp_Cir_Mdv_Ar,C_eou,isOff_dp_Cr_Mdv_Ar), (isOn_fp_Cr_Mdv_Anr,A_eoa,isOn_fp_Cr_Mdv_Ar), (isOn_fp_Cr_Mdv_Anr,M_ns,isOn_fp_Cr_Mnv_Anr),
           (isOn_fp_Cr_Mdv_Anr,C_clamp,isOn_fp_Cie_Mdv_Anr), (isOn_fp_Cr_Mdv_Anr,stop,isOff_dp_Cr_Mdv_Anr), (isOn_fp_Cie_Mfv_Anr,A_eoa,isOn_fp_Cie_Mfv_Ar),
           (isOn_fp_Cie_Mfv_Anr,M_tOff,isOn_fp_Cie_Mdv_Anr), (isOn_fp_Cie_Mfv_Anr,C_eoc,isOn_fp_Ce_Mfv_Anr), (isOn_fp_Cie_Mfv_Anr,stop,isOff_dp_Cie_Mfv_Anr),
           (isOn_fp_Ce_Miv_Anr,A_eoa,isOn_fp_Ce_Miv_Ar), (isOn_fp_Ce_Miv_Anr,M_fs,isOn_fp_Ce_Mfv_Anr), (isOn_fp_Ce_Miv_Anr,C_unclamp,isOn_fp_Cir_Miv_Anr),
           (isOn_fp_Ce_Miv_Anr,stop,isOff_dp_Ce_Miv_Anr), (isOn_fp_Cir_Mnv_Anr,A_eoa,isOn_fp_Cir_Mnv_Ar), (isOn_fp_Cir_Mnv_Anr,M_tOn,isOn_fp_Cir_Miv_Anr),
           (isOn_fp_Cir_Mnv_Anr,C_eou,isOn_fp_Cr_Mnv_Anr), (isOn_fp_Cir_Mnv_Anr,stop,isOff_dp_Cir_Mnv_Anr), (isOn_fp_Cie_Mdv_Ar,A_gd,isOn_fp_Cie_Mdv_Anr),
           (isOn_fp_Cie_Mdv_Ar,M_ns,isOn_fp_Cie_Mnv_Ar), (isOn_fp_Cie_Mdv_Ar,C_eoc,isOn_fp_Ce_Mdv_Ar), (isOn_fp_Cie_Mdv_Ar,stop,isOff_dp_Cie_Mdv_Ar),
           (isOn_fp_Ce_Mfv_Ar,A_gd,isOn_fp_Ce_Mfv_Anr), (isOn_fp_Ce_Mfv_Ar,M_tOff,isOn_fp_Ce_Mdv_Ar), (isOn_fp_Ce_Mfv_Ar,C_unclamp,isOn_fp_Cir_Mfv_Ar),
           (isOn_fp_Ce_Mfv_Ar,stop,isOff_dp_Ce_Mfv_Ar), (isOn_fp_Cir_Miv_Ar,A_gd,isOn_fp_Cir_Miv_Anr), (isOn_fp_Cir_Miv_Ar,M_fs,isOn_fp_Cir_Mfv_Ar),
           (isOn_fp_Cir_Miv_Ar,C_eou,isOn_fp_Cr_Miv_Ar), (isOn_fp_Cir_Miv_Ar,stop,isOff_dp_Cir_Miv_Ar), (isOn_dp_Ce_Mdv_Anr,A_eoa,isOn_fp_Ce_Mdv_Ar),
           (isOn_dp_Ce_Mdv_Anr,M_ns,isOn_dp_Ce_Mnv_Anr), (isOn_dp_Ce_Mdv_Anr,C_unclamp,isOn_dp_Cir_Mdv_Anr), (isOn_dp_Cir_Mfv_Anr,A_eoa,isOn_fp_Cir_Mfv_Ar),
           (isOn_dp_Cir_Mfv_Anr,M_tOff,isOn_dp_Cir_Mdv_Anr), (isOn_dp_Cir_Mfv_Anr,C_eou,isOn_dp_Cr_Mfv_Anr), (isOn_dp_Cir_Mdv_Ar,A_gd,isOn_dp_Cir_Mdv_Anr),
           (isOn_dp_Cir_Mdv_Ar,M_ns,isOn_dp_Cir_Mnv_Ar), (isOn_dp_Cir_Mdv_Ar,C_eou,isOn_dp_Cr_Mdv_Ar), (isOff_dp_Cir_Mdv_Anr,start,isOn_dp_Cir_Mdv_Anr),
           (isOff_dp_Cir_Mdv_Anr,M_ns,isOff_dp_Cir_Mnv_Anr), (isOff_dp_Cir_Mdv_Anr,C_eou,isOff_dp_Cr_Mdv_Anr), (isOff_dp_Cir_Mdv_Anr,A_eoa,isOff_fp_Cir_Mdv_Ar),
           (isOff_fp_Ce_Mdv_Ar,start,isOn_fp_Ce_Mdv_Ar), (isOff_fp_Ce_Mdv_Ar,A_gd,isOff_fp_Ce_Mdv_Anr), (isOff_fp_Ce_Mdv_Ar,M_ns,isOff_fp_Ce_Mnv_Ar),
           (isOff_fp_Ce_Mdv_Ar,C_unclamp,isOff_fp_Cir_Mdv_Ar), (isOff_fp_Cie_Mdv_Anr,start,isOn_fp_Cie_Mdv_Anr), (isOff_fp_Cie_Mdv_Anr,M_ns,isOff_fp_Cie_Mnv_Anr),
           (isOff_fp_Cie_Mdv_Anr,C_eoc,isOff_fp_Ce_Mdv_Anr), (isOff_fp_Cie_Mdv_Anr,A_eoa,isOff_fp_Cie_Mdv_Ar), (isOff_fp_Cir_Mfv_Ar,start,isOn_fp_Cir_Mfv_Ar),
           (isOff_fp_Cir_Mfv_Ar,A_gd,isOff_fp_Cir_Mfv_Anr), (isOff_fp_Cir_Mfv_Ar,M_tOff,isOff_fp_Cir_Mdv_Ar), (isOff_fp_Cir_Mfv_Ar,C_eou,isOff_fp_Cr_Mfv_Ar),
           (isOff_fp_Ce_Mfv_Anr,start,isOn_fp_Ce_Mfv_Anr), (isOff_fp_Ce_Mfv_Anr,M_tOff,isOff_fp_Ce_Mdv_Anr), (isOff_fp_Ce_Mfv_Anr,C_unclamp,isOff_fp_Cir_Mfv_Anr),
           (isOff_fp_Ce_Mfv_Anr,A_eoa,isOff_fp_Ce_Mfv_Ar), (isOff_fp_Cir_Miv_Anr,start,isOn_fp_Cir_Miv_Anr), (isOff_fp_Cir_Miv_Anr,M_fs,isOff_fp_Cir_Mfv_Anr),
           (isOff_fp_Cir_Miv_Anr,C_eou,isOff_fp_Cr_Miv_Anr), (isOff_fp_Cir_Miv_Anr,A_eoa,isOff_fp_Cir_Miv_Ar), (isOn_fp_Cie_Mdv_Anr,A_eoa,isOn_fp_Cie_Mdv_Ar),
           (isOn_fp_Cie_Mdv_Anr,M_ns,isOn_fp_Cie_Mnv_Anr), (isOn_fp_Cie_Mdv_Anr,C_eoc,isOn_fp_Ce_Mdv_Anr), (isOn_fp_Cie_Mdv_Anr,stop,isOff_dp_Cie_Mdv_Anr),
           (isOn_fp_Ce_Mfv_Anr,A_eoa,isOn_fp_Ce_Mfv_Ar), (isOn_fp_Ce_Mfv_Anr,M_tOff,isOn_fp_Ce_Mdv_Anr), (isOn_fp_Ce_Mfv_Anr,C_unclamp,isOn_fp_Cir_Mfv_Anr),
           (isOn_fp_Ce_Mfv_Anr,stop,isOff_dp_Ce_Mfv_Anr), (isOn_fp_Cir_Miv_Anr,A_eoa,isOn_fp_Cir_Miv_Ar), (isOn_fp_Cir_Miv_Anr,M_fs,isOn_fp_Cir_Mfv_Anr),
           (isOn_fp_Cir_Miv_Anr,C_eou,isOn_fp_Cr_Miv_Anr), (isOn_fp_Cir_Miv_Anr,stop,isOff_dp_Cir_Miv_Anr), (isOn_fp_Ce_Mdv_Ar,A_gd,isOn_fp_Ce_Mdv_Anr),
           (isOn_fp_Ce_Mdv_Ar,M_ns,isOn_fp_Ce_Mnv_Ar), (isOn_fp_Ce_Mdv_Ar,C_unclamp,isOn_fp_Cir_Mdv_Ar), (isOn_fp_Ce_Mdv_Ar,stop,isOff_dp_Ce_Mdv_Ar),
           (isOn_fp_Cir_Mfv_Ar,A_gd,isOn_fp_Cir_Mfv_Anr), (isOn_fp_Cir_Mfv_Ar,M_tOff,isOn_fp_Cir_Mdv_Ar), (isOn_fp_Cir_Mfv_Ar,C_eou,isOn_fp_Cr_Mfv_Ar),
           (isOn_fp_Cir_Mfv_Ar,stop,isOff_dp_Cir_Mfv_Ar), (isOn_dp_Cir_Mdv_Anr,A_eoa,isOn_fp_Cir_Mdv_Ar), (isOn_dp_Cir_Mdv_Anr,M_ns,isOn_dp_Cir_Mnv_Anr),
           (isOn_dp_Cir_Mdv_Anr,C_eou,isOn_dp_Cr_Mdv_Anr), (isOff_fp_Cir_Mdv_Ar,start,isOn_fp_Cir_Mdv_Ar), (isOff_fp_Cir_Mdv_Ar,A_gd,isOff_fp_Cir_Mdv_Anr),
           (isOff_fp_Cir_Mdv_Ar,M_ns,isOff_fp_Cir_Mnv_Ar), (isOff_fp_Cir_Mdv_Ar,C_eou,isOff_fp_Cr_Mdv_Ar), (isOff_fp_Ce_Mdv_Anr,start,isOn_fp_Ce_Mdv_Anr),
           (isOff_fp_Ce_Mdv_Anr,M_ns,isOff_fp_Ce_Mnv_Anr), (isOff_fp_Ce_Mdv_Anr,C_unclamp,isOff_fp_Cir_Mdv_Anr), (isOff_fp_Ce_Mdv_Anr,A_eoa,isOff_fp_Ce_Mdv_Ar),
           (isOff_fp_Cir_Mfv_Anr,start,isOn_fp_Cir_Mfv_Anr), (isOff_fp_Cir_Mfv_Anr,M_tOff,isOff_fp_Cir_Mdv_Anr), (isOff_fp_Cir_Mfv_Anr,C_eou,isOff_fp_Cr_Mfv_Anr),
           (isOff_fp_Cir_Mfv_Anr,A_eoa,isOff_fp_Cir_Mfv_Ar), (isOn_fp_Ce_Mdv_Anr,A_eoa,isOn_fp_Ce_Mdv_Ar), (isOn_fp_Ce_Mdv_Anr,M_ns,isOn_fp_Ce_Mnv_Anr),
           (isOn_fp_Ce_Mdv_Anr,C_unclamp,isOn_fp_Cir_Mdv_Anr), (isOn_fp_Ce_Mdv_Anr,stop,isOff_dp_Ce_Mdv_Anr), (isOn_fp_Cir_Mfv_Anr,A_eoa,isOn_fp_Cir_Mfv_Ar),
           (isOn_fp_Cir_Mfv_Anr,M_tOff,isOn_fp_Cir_Mdv_Anr), (isOn_fp_Cir_Mfv_Anr,C_eou,isOn_fp_Cr_Mfv_Anr), (isOn_fp_Cir_Mfv_Anr,stop,isOff_dp_Cir_Mfv_Anr),
           (isOn_fp_Cir_Mdv_Ar,A_gd,isOn_fp_Cir_Mdv_Anr), (isOn_fp_Cir_Mdv_Ar,M_ns,isOn_fp_Cir_Mnv_Ar), (isOn_fp_Cir_Mdv_Ar,C_eou,isOn_fp_Cr_Mdv_Ar),
           (isOn_fp_Cir_Mdv_Ar,stop,isOff_dp_Cir_Mdv_Ar), (isOff_fp_Cir_Mdv_Anr,start,isOn_fp_Cir_Mdv_Anr), (isOff_fp_Cir_Mdv_Anr,M_ns,isOff_fp_Cir_Mnv_Anr),
           (isOff_fp_Cir_Mdv_Anr,C_eou,isOff_fp_Cr_Mdv_Anr), (isOff_fp_Cir_Mdv_Anr,A_eoa,isOff_fp_Cir_Mdv_Ar), (isOn_fp_Cir_Mdv_Anr,A_eoa,isOn_fp_Cir_Mdv_Ar),
           (isOn_fp_Cir_Mdv_Anr,M_ns,isOn_fp_Cir_Mnv_Anr), (isOn_fp_Cir_Mdv_Anr,C_eou,isOn_fp_Cr_Mdv_Anr), (isOn_fp_Cir_Mdv_Anr,stop,isOff_dp_Cir_Mdv_Anr) },
      x_0: isOff_dp_Cr_Mnv_Ar,
      Xm: {isOff_dp_Cr_Mnv_Ar},
      ctrlEvents: {start, stop, C_clamp, C_unclamp, M_tOn, M_tOff, A_gd} )
 &
 switch = {
      (isOff_dp_Cr_Mnv_Ar,isOff),(isOn_dp_Cr_Mnv_Ar,isOn),(isOff_dp_Cr_Mnv_Anr,isOff),
      (isOff_dp_Cr_Miv_Ar,isOff),(isOff_dp_Cie_Mnv_Ar,isOff),(isOn_dp_Cr_Mnv_Anr,isOn),
      (isOn_dp_Cr_Miv_Ar,isOn),(isOn_dp_Cie_Mnv_Ar,isOn),(isOff_dp_Cr_Miv_Anr,isOff),
      (isOff_dp_Cie_Mnv_Anr,isOff),(isOff_fp_Cr_Mnv_Ar,isOff),(isOff_dp_Cr_Mfv_Ar,isOff),
      (isOff_dp_Cie_Miv_Ar,isOff),(isOff_dp_Ce_Mnv_Ar,isOff),(isOn_fp_Cr_Mnv_Ar,isOn),
      (isOn_dp_Cr_Miv_Anr,isOn),(isOn_dp_Cie_Mnv_Anr,isOn),(isOn_dp_Cr_Mfv_Ar,isOn),
      (isOn_dp_Cie_Miv_Ar,isOn),(isOn_dp_Ce_Mnv_Ar,isOn),(isOff_dp_Cr_Mfv_Anr,isOff),
      (isOff_dp_Cie_Miv_Anr,isOff),(isOff_fp_Cr_Miv_Ar,isOff),(isOff_dp_Ce_Mnv_Anr,isOff),
      (isOff_fp_Cie_Mnv_Ar,isOff),(isOff_fp_Cr_Mnv_Anr,isOff),(isOff_dp_Cr_Mdv_Ar,isOff),
      (isOff_dp_Cie_Mfv_Ar,isOff),(isOff_dp_Ce_Miv_Ar,isOff),(isOff_dp_Cir_Mnv_Ar,isOff),
      (isOn_fp_Cr_Mnv_Anr,isOn),(isOn_fp_Cr_Miv_Ar,isOn),(isOn_fp_Cie_Mnv_Ar,isOn),
      (isOn_dp_Cr_Mfv_Anr,isOn),(isOn_dp_Cie_Miv_Anr,isOn),(isOn_dp_Ce_Mnv_Anr,isOn),
      (isOn_dp_Cr_Mdv_Ar,isOn),(isOn_dp_Cie_Mfv_Ar,isOn),(isOn_dp_Ce_Miv_Ar,isOn),
      (isOn_dp_Cir_Mnv_Ar,isOn),(isOff_dp_Cr_Mdv_Anr,isOff),(isOff_dp_Cie_Mfv_Anr,isOff),
      (isOff_fp_Cr_Mfv_Ar,isOff),(isOff_dp_Ce_Miv_Anr,isOff),(isOff_fp_Cie_Miv_Ar,isOff),
      (isOff_fp_Cr_Miv_Anr,isOff),(isOff_dp_Cir_Mnv_Anr,isOff),(isOff_fp_Ce_Mnv_Ar,isOff),
      (isOff_fp_Cie_Mnv_Anr,isOff),(isOff_dp_Cie_Mdv_Ar,isOff),(isOff_dp_Ce_Mfv_Ar,isOff),
      (isOff_dp_Cir_Miv_Ar,isOff),(isOn_fp_Cr_Miv_Anr,isOn),(isOn_fp_Cie_Mnv_Anr,isOn),
      (isOn_fp_Cr_Mfv_Ar,isOn),(isOn_fp_Cie_Miv_Ar,isOn),(isOn_fp_Ce_Mnv_Ar,isOn),
      (isOn_dp_Cr_Mdv_Anr,isOn),(isOn_dp_Cie_Mfv_Anr,isOn),(isOn_dp_Ce_Miv_Anr,isOn),
      (isOn_dp_Cir_Mnv_Anr,isOn),(isOn_dp_Cie_Mdv_Ar,isOn),(isOn_dp_Ce_Mfv_Ar,isOn),
      (isOn_dp_Cir_Miv_Ar,isOn),(isOff_dp_Cie_Mdv_Anr,isOff),(isOff_fp_Cr_Mdv_Ar,isOff),
      (isOff_dp_Ce_Mfv_Anr,isOff),(isOff_fp_Cie_Mfv_Ar,isOff),(isOff_fp_Cr_Mfv_Anr,isOff),
      (isOff_dp_Cir_Miv_Anr,isOff),(isOff_fp_Ce_Miv_Ar,isOff),(isOff_fp_Cie_Miv_Anr,isOff),
      (isOff_fp_Cir_Mnv_Ar,isOff),(isOff_fp_Ce_Mnv_Anr,isOff),(isOff_dp_Ce_Mdv_Ar,isOff),
      (isOff_dp_Cir_Mfv_Ar,isOff),(isOn_fp_Cr_Mfv_Anr,isOn),(isOn_fp_Cie_Miv_Anr,isOn),
      (isOn_fp_Ce_Mnv_Anr,isOn),(isOn_fp_Cr_Mdv_Ar,isOn),(isOn_fp_Cie_Mfv_Ar,isOn),
      (isOn_fp_Ce_Miv_Ar,isOn),(isOn_fp_Cir_Mnv_Ar,isOn),(isOn_dp_Cie_Mdv_Anr,isOn),
      (isOn_dp_Ce_Mfv_Anr,isOn),(isOn_dp_Cir_Miv_Anr,isOn),(isOn_dp_Ce_Mdv_Ar,isOn),
      (isOn_dp_Cir_Mfv_Ar,isOn),(isOff_dp_Ce_Mdv_Anr,isOff),(isOff_fp_Cie_Mdv_Ar,isOff),
      (isOff_fp_Cr_Mdv_Anr,isOff),(isOff_dp_Cir_Mfv_Anr,isOff),(isOff_fp_Ce_Mfv_Ar,isOff),
      (isOff_fp_Cie_Mfv_Anr,isOff),(isOff_fp_Cir_Miv_Ar,isOff),(isOff_fp_Ce_Miv_Anr,isOff),
      (isOff_fp_Cir_Mnv_Anr,isOff),(isOff_dp_Cir_Mdv_Ar,isOff),(isOn_fp_Cr_Mdv_Anr,isOn),
      (isOn_fp_Cie_Mfv_Anr,isOn),(isOn_fp_Ce_Miv_Anr,isOn),(isOn_fp_Cir_Mnv_Anr,isOn),
      (isOn_fp_Cie_Mdv_Ar,isOn),(isOn_fp_Ce_Mfv_Ar,isOn),(isOn_fp_Cir_Miv_Ar,isOn),
      (isOn_dp_Ce_Mdv_Anr,isOn),(isOn_dp_Cir_Mfv_Anr,isOn),(isOn_dp_Cir_Mdv_Ar,isOn),
      (isOff_dp_Cir_Mdv_Anr,isOff),(isOff_fp_Ce_Mdv_Ar,isOff),(isOff_fp_Cie_Mdv_Anr,isOff),
      (isOff_fp_Cir_Mfv_Ar,isOff),(isOff_fp_Ce_Mfv_Anr,isOff),(isOff_fp_Cir_Miv_Anr,isOff),
      (isOn_fp_Cie_Mdv_Anr,isOn),(isOn_fp_Ce_Mfv_Anr,isOn),(isOn_fp_Cir_Miv_Anr,isOn),
      (isOn_fp_Ce_Mdv_Ar,isOn),(isOn_fp_Cir_Mfv_Ar,isOn),(isOn_dp_Cir_Mdv_Anr,isOn),
      (isOff_fp_Cir_Mdv_Ar,isOff),(isOff_fp_Ce_Mdv_Anr,isOff),(isOff_fp_Cir_Mfv_Anr,isOff),
      (isOn_fp_Ce_Mdv_Anr,isOn),(isOn_fp_Cir_Mfv_Anr,isOn),(isOn_fp_Cir_Mdv_Ar,isOn),
      (isOff_fp_Cir_Mdv_Anr,isOff),(isOn_fp_Cir_Mdv_Anr,isOn) }
 &
 operation = {
      (isOff_dp_Cr_Mnv_Ar,dp),(isOn_dp_Cr_Mnv_Ar,dp),(isOff_dp_Cr_Mnv_Anr,dp),
      (isOff_dp_Cr_Miv_Ar,dp),(isOff_dp_Cie_Mnv_Ar,dp),(isOn_dp_Cr_Mnv_Anr,dp),
      (isOn_dp_Cr_Miv_Ar,dp),(isOn_dp_Cie_Mnv_Ar,dp),(isOff_dp_Cr_Miv_Anr,dp),
      (isOff_dp_Cie_Mnv_Anr,dp),(isOff_fp_Cr_Mnv_Ar,fp),(isOff_dp_Cr_Mfv_Ar,dp),
      (isOff_dp_Cie_Miv_Ar,dp),(isOff_dp_Ce_Mnv_Ar,dp),(isOn_fp_Cr_Mnv_Ar,fp),
      (isOn_dp_Cr_Miv_Anr,dp),(isOn_dp_Cie_Mnv_Anr,dp),(isOn_dp_Cr_Mfv_Ar,dp),
      (isOn_dp_Cie_Miv_Ar,dp),(isOn_dp_Ce_Mnv_Ar,dp),(isOff_dp_Cr_Mfv_Anr,dp),
      (isOff_dp_Cie_Miv_Anr,dp),(isOff_fp_Cr_Miv_Ar,fp),(isOff_dp_Ce_Mnv_Anr,dp),
      (isOff_fp_Cie_Mnv_Ar,fp),(isOff_fp_Cr_Mnv_Anr,fp),(isOff_dp_Cr_Mdv_Ar,dp),
      (isOff_dp_Cie_Mfv_Ar,dp),(isOff_dp_Ce_Miv_Ar,dp),(isOff_dp_Cir_Mnv_Ar,dp),
      (isOn_fp_Cr_Mnv_Anr,fp),(isOn_fp_Cr_Miv_Ar,fp),(isOn_fp_Cie_Mnv_Ar,fp),
      (isOn_dp_Cr_Mfv_Anr,dp),(isOn_dp_Cie_Miv_Anr,dp),(isOn_dp_Ce_Mnv_Anr,dp),
      (isOn_dp_Cr_Mdv_Ar,dp),(isOn_dp_Cie_Mfv_Ar,dp),(isOn_dp_Ce_Miv_Ar,dp),
      (isOn_dp_Cir_Mnv_Ar,dp),(isOff_dp_Cr_Mdv_Anr,dp),(isOff_dp_Cie_Mfv_Anr,dp),
      (isOff_fp_Cr_Mfv_Ar,fp),(isOff_dp_Ce_Miv_Anr,dp),(isOff_fp_Cie_Miv_Ar,fp),
      (isOff_fp_Cr_Miv_Anr,fp),(isOff_dp_Cir_Mnv_Anr,dp),(isOff_fp_Ce_Mnv_Ar,fp),
      (isOff_fp_Cie_Mnv_Anr,fp),(isOff_dp_Cie_Mdv_Ar,dp),(isOff_dp_Ce_Mfv_Ar,dp),
      (isOff_dp_Cir_Miv_Ar,dp),(isOn_fp_Cr_Miv_Anr,fp),(isOn_fp_Cie_Mnv_Anr,fp),
      (isOn_fp_Cr_Mfv_Ar,fp),(isOn_fp_Cie_Miv_Ar,fp),(isOn_fp_Ce_Mnv_Ar,fp),
      (isOn_dp_Cr_Mdv_Anr,dp),(isOn_dp_Cie_Mfv_Anr,dp),(isOn_dp_Ce_Miv_Anr,dp),
      (isOn_dp_Cir_Mnv_Anr,dp),(isOn_dp_Cie_Mdv_Ar,dp),(isOn_dp_Ce_Mfv_Ar,dp),
      (isOn_dp_Cir_Miv_Ar,dp),(isOff_dp_Cie_Mdv_Anr,dp),(isOff_fp_Cr_Mdv_Ar,fp),
      (isOff_dp_Ce_Mfv_Anr,dp),(isOff_fp_Cie_Mfv_Ar,fp),(isOff_fp_Cr_Mfv_Anr,fp),
      (isOff_dp_Cir_Miv_Anr,dp),(isOff_fp_Ce_Miv_Ar,fp),(isOff_fp_Cie_Miv_Anr,fp),
      (isOff_fp_Cir_Mnv_Ar,fp),(isOff_fp_Ce_Mnv_Anr,fp),(isOff_dp_Ce_Mdv_Ar,dp),
      (isOff_dp_Cir_Mfv_Ar,dp),(isOn_fp_Cr_Mfv_Anr,fp),(isOn_fp_Cie_Miv_Anr,fp),
      (isOn_fp_Ce_Mnv_Anr,fp),(isOn_fp_Cr_Mdv_Ar,fp),(isOn_fp_Cie_Mfv_Ar,fp),
      (isOn_fp_Ce_Miv_Ar,fp),(isOn_fp_Cir_Mnv_Ar,fp),(isOn_dp_Cie_Mdv_Anr,dp),
      (isOn_dp_Ce_Mfv_Anr,dp),(isOn_dp_Cir_Miv_Anr,dp),(isOn_dp_Ce_Mdv_Ar,dp),
      (isOn_dp_Cir_Mfv_Ar,dp),(isOff_dp_Ce_Mdv_Anr,dp),(isOff_fp_Cie_Mdv_Ar,fp),
      (isOff_fp_Cr_Mdv_Anr,fp),(isOff_dp_Cir_Mfv_Anr,dp),(isOff_fp_Ce_Mfv_Ar,fp),
      (isOff_fp_Cie_Mfv_Anr,fp),(isOff_fp_Cir_Miv_Ar,fp),(isOff_fp_Ce_Miv_Anr,fp),
      (isOff_fp_Cir_Mnv_Anr,fp),(isOff_dp_Cir_Mdv_Ar,dp),(isOn_fp_Cr_Mdv_Anr,fp),
      (isOn_fp_Cie_Mfv_Anr,fp),(isOn_fp_Ce_Miv_Anr,fp),(isOn_fp_Cir_Mnv_Anr,fp),
      (isOn_fp_Cie_Mdv_Ar,fp),(isOn_fp_Ce_Mfv_Ar,fp),(isOn_fp_Cir_Miv_Ar,fp),
      (isOn_dp_Ce_Mdv_Anr,dp),(isOn_dp_Cir_Mfv_Anr,dp),(isOn_dp_Cir_Mdv_Ar,dp),
      (isOff_dp_Cir_Mdv_Anr,dp),(isOff_fp_Ce_Mdv_Ar,fp),(isOff_fp_Cie_Mdv_Anr,fp),
      (isOff_fp_Cir_Mfv_Ar,fp),(isOff_fp_Ce_Mfv_Anr,fp),(isOff_fp_Cir_Miv_Anr,fp),
      (isOn_fp_Cie_Mdv_Anr,fp),(isOn_fp_Ce_Mfv_Anr,fp),(isOn_fp_Cir_Miv_Anr,fp),
      (isOn_fp_Ce_Mdv_Ar,fp),(isOn_fp_Cir_Mfv_Ar,fp),(isOn_dp_Cir_Mdv_Anr,dp),
      (isOff_fp_Cir_Mdv_Ar,fp),(isOff_fp_Ce_Mdv_Anr,fp),(isOff_fp_Cir_Mfv_Anr,fp),
      (isOn_fp_Ce_Mdv_Anr,fp),(isOn_fp_Cir_Mfv_Anr,fp),(isOn_fp_Cir_Mdv_Ar,fp),
      (isOff_fp_Cir_Mdv_Anr,fp),(isOn_fp_Cir_Mdv_Anr,fp) }
 &
 clamp = {
      (isOff_dp_Cr_Mnv_Ar,Cr),(isOn_dp_Cr_Mnv_Ar,Cr),(isOff_dp_Cr_Mnv_Anr,Cr),
      (isOff_dp_Cr_Miv_Ar,Cr),(isOff_dp_Cie_Mnv_Ar,Cie),(isOn_dp_Cr_Mnv_Anr,Cr),
      (isOn_dp_Cr_Miv_Ar,Cr),(isOn_dp_Cie_Mnv_Ar,Cie),(isOff_dp_Cr_Miv_Anr,Cr),
      (isOff_dp_Cie_Mnv_Anr,Cie),(isOff_fp_Cr_Mnv_Ar,Cr),(isOff_dp_Cr_Mfv_Ar,Cr),
      (isOff_dp_Cie_Miv_Ar,Cie),(isOff_dp_Ce_Mnv_Ar,Ce),(isOn_fp_Cr_Mnv_Ar,Cr),
      (isOn_dp_Cr_Miv_Anr,Cr),(isOn_dp_Cie_Mnv_Anr,Cie),(isOn_dp_Cr_Mfv_Ar,Cr),
      (isOn_dp_Cie_Miv_Ar,Cie),(isOn_dp_Ce_Mnv_Ar,Ce),(isOff_dp_Cr_Mfv_Anr,Cr),
      (isOff_dp_Cie_Miv_Anr,Cie),(isOff_fp_Cr_Miv_Ar,Cr),(isOff_dp_Ce_Mnv_Anr,Ce),
      (isOff_fp_Cie_Mnv_Ar,Cie),(isOff_fp_Cr_Mnv_Anr,Cr),(isOff_dp_Cr_Mdv_Ar,Cr),
      (isOff_dp_Cie_Mfv_Ar,Cie),(isOff_dp_Ce_Miv_Ar,Ce),(isOff_dp_Cir_Mnv_Ar,Cir),
      (isOn_fp_Cr_Mnv_Anr,Cr),(isOn_fp_Cr_Miv_Ar,Cr),(isOn_fp_Cie_Mnv_Ar,Cie),
      (isOn_dp_Cr_Mfv_Anr,Cr),(isOn_dp_Cie_Miv_Anr,Cie),(isOn_dp_Ce_Mnv_Anr,Ce),
      (isOn_dp_Cr_Mdv_Ar,Cr),(isOn_dp_Cie_Mfv_Ar,Cie),(isOn_dp_Ce_Miv_Ar,Ce),
      (isOn_dp_Cir_Mnv_Ar,Cir),(isOff_dp_Cr_Mdv_Anr,Cr),(isOff_dp_Cie_Mfv_Anr,Cie),
      (isOff_fp_Cr_Mfv_Ar,Cr),(isOff_dp_Ce_Miv_Anr,Ce),(isOff_fp_Cie_Miv_Ar,Cie),
      (isOff_fp_Cr_Miv_Anr,Cr),(isOff_dp_Cir_Mnv_Anr,Cir),(isOff_fp_Ce_Mnv_Ar,Ce),
      (isOff_fp_Cie_Mnv_Anr,Cie),(isOff_dp_Cie_Mdv_Ar,Cie),(isOff_dp_Ce_Mfv_Ar,Ce),
      (isOff_dp_Cir_Miv_Ar,Cir),(isOn_fp_Cr_Miv_Anr,Cr),(isOn_fp_Cie_Mnv_Anr,Cie),
      (isOn_fp_Cr_Mfv_Ar,Cr),(isOn_fp_Cie_Miv_Ar,Cie),(isOn_fp_Ce_Mnv_Ar,Ce),
      (isOn_dp_Cr_Mdv_Anr,Cr),(isOn_dp_Cie_Mfv_Anr,Cie),(isOn_dp_Ce_Miv_Anr,Ce),
      (isOn_dp_Cir_Mnv_Anr,Cir),(isOn_dp_Cie_Mdv_Ar,Cie),(isOn_dp_Ce_Mfv_Ar,Ce),
      (isOn_dp_Cir_Miv_Ar,Cir),(isOff_dp_Cie_Mdv_Anr,Cie),(isOff_fp_Cr_Mdv_Ar,Cr),
      (isOff_dp_Ce_Mfv_Anr,Ce),(isOff_fp_Cie_Mfv_Ar,Cie),(isOff_fp_Cr_Mfv_Anr,Cr),
      (isOff_dp_Cir_Miv_Anr,Cir),(isOff_fp_Ce_Miv_Ar,Ce),(isOff_fp_Cie_Miv_Anr,Cie),
      (isOff_fp_Cir_Mnv_Ar,Cir),(isOff_fp_Ce_Mnv_Anr,Ce),(isOff_dp_Ce_Mdv_Ar,Ce),
      (isOff_dp_Cir_Mfv_Ar,Cir),(isOn_fp_Cr_Mfv_Anr,Cr),(isOn_fp_Cie_Miv_Anr,Cie),
      (isOn_fp_Ce_Mnv_Anr,Ce),(isOn_fp_Cr_Mdv_Ar,Cr),(isOn_fp_Cie_Mfv_Ar,Cie),
      (isOn_fp_Ce_Miv_Ar,Ce),(isOn_fp_Cir_Mnv_Ar,Cir),(isOn_dp_Cie_Mdv_Anr,Cie),
      (isOn_dp_Ce_Mfv_Anr,Ce),(isOn_dp_Cir_Miv_Anr,Cir),(isOn_dp_Ce_Mdv_Ar,Ce),
      (isOn_dp_Cir_Mfv_Ar,Cir),(isOff_dp_Ce_Mdv_Anr,Ce),(isOff_fp_Cie_Mdv_Ar,Cie),
      (isOff_fp_Cr_Mdv_Anr,Cr),(isOff_dp_Cir_Mfv_Anr,Cir),(isOff_fp_Ce_Mfv_Ar,Ce),
      (isOff_fp_Cie_Mfv_Anr,Cie),(isOff_fp_Cir_Miv_Ar,Cir),(isOff_fp_Ce_Miv_Anr,Ce),
      (isOff_fp_Cir_Mnv_Anr,Cir),(isOff_dp_Cir_Mdv_Ar,Cir),(isOn_fp_Cr_Mdv_Anr,Cr),
      (isOn_fp_Cie_Mfv_Anr,Cie),(isOn_fp_Ce_Miv_Anr,Ce),(isOn_fp_Cir_Mnv_Anr,Cir),
      (isOn_fp_Cie_Mdv_Ar,Cie),(isOn_fp_Ce_Mfv_Ar,Ce),(isOn_fp_Cir_Miv_Ar,Cir),
      (isOn_dp_Ce_Mdv_Anr,Ce),(isOn_dp_Cir_Mfv_Anr,Cir),(isOn_dp_Cir_Mdv_Ar,Cir),
      (isOff_dp_Cir_Mdv_Anr,Cir),(isOff_fp_Ce_Mdv_Ar,Ce),(isOff_fp_Cie_Mdv_Anr,Cie),
      (isOff_fp_Cir_Mfv_Ar,Cir),(isOff_fp_Ce_Mfv_Anr,Ce),(isOff_fp_Cir_Miv_Anr,Cir),
      (isOn_fp_Cie_Mdv_Anr,Cie),(isOn_fp_Ce_Mfv_Anr,Ce),(isOn_fp_Cir_Miv_Anr,Cir),
      (isOn_fp_Ce_Mdv_Ar,Ce),(isOn_fp_Cir_Mfv_Ar,Cir),(isOn_dp_Cir_Mdv_Anr,Cir),
      (isOff_fp_Cir_Mdv_Ar,Cir),(isOff_fp_Ce_Mdv_Anr,Ce),(isOff_fp_Cir_Mfv_Anr,Cir),
      (isOn_fp_Ce_Mdv_Anr,Ce),(isOn_fp_Cir_Mfv_Anr,Cir),(isOn_fp_Cir_Mdv_Ar,Cir),
      (isOff_fp_Cir_Mdv_Anr,Cir),(isOn_fp_Cir_Mdv_Anr,Cir) }
 &
 motor = {
      (isOff_dp_Cr_Mnv_Ar,Mnv),(isOn_dp_Cr_Mnv_Ar,Mnv),(isOff_dp_Cr_Mnv_Anr,Mnv),
      (isOff_dp_Cr_Miv_Ar,Miv),(isOff_dp_Cie_Mnv_Ar,Mnv),(isOn_dp_Cr_Mnv_Anr,Mnv),
      (isOn_dp_Cr_Miv_Ar,Miv),(isOn_dp_Cie_Mnv_Ar,Mnv),(isOff_dp_Cr_Miv_Anr,Miv),
      (isOff_dp_Cie_Mnv_Anr,Mnv),(isOff_fp_Cr_Mnv_Ar,Mnv),(isOff_dp_Cr_Mfv_Ar,Mfv),
      (isOff_dp_Cie_Miv_Ar,Miv),(isOff_dp_Ce_Mnv_Ar,Mnv),(isOn_fp_Cr_Mnv_Ar,Mnv),
      (isOn_dp_Cr_Miv_Anr,Miv),(isOn_dp_Cie_Mnv_Anr,Mnv),(isOn_dp_Cr_Mfv_Ar,Mfv),
      (isOn_dp_Cie_Miv_Ar,Miv),(isOn_dp_Ce_Mnv_Ar,Mnv),(isOff_dp_Cr_Mfv_Anr,Mfv),
      (isOff_dp_Cie_Miv_Anr,Miv),(isOff_fp_Cr_Miv_Ar,Miv),(isOff_dp_Ce_Mnv_Anr,Mnv),
      (isOff_fp_Cie_Mnv_Ar,Mnv),(isOff_fp_Cr_Mnv_Anr,Mnv),(isOff_dp_Cr_Mdv_Ar,Mdv),
      (isOff_dp_Cie_Mfv_Ar,Mfv),(isOff_dp_Ce_Miv_Ar,Miv),(isOff_dp_Cir_Mnv_Ar,Mnv),
      (isOn_fp_Cr_Mnv_Anr,Mnv),(isOn_fp_Cr_Miv_Ar,Miv),(isOn_fp_Cie_Mnv_Ar,Mnv),
      (isOn_dp_Cr_Mfv_Anr,Mfv),(isOn_dp_Cie_Miv_Anr,Miv),(isOn_dp_Ce_Mnv_Anr,Mnv),
      (isOn_dp_Cr_Mdv_Ar,Mdv),(isOn_dp_Cie_Mfv_Ar,Mfv),(isOn_dp_Ce_Miv_Ar,Miv),
      (isOn_dp_Cir_Mnv_Ar,Mnv),(isOff_dp_Cr_Mdv_Anr,Mdv),(isOff_dp_Cie_Mfv_Anr,Mfv),
      (isOff_fp_Cr_Mfv_Ar,Mfv),(isOff_dp_Ce_Miv_Anr,Miv),(isOff_fp_Cie_Miv_Ar,Miv),
      (isOff_fp_Cr_Miv_Anr,Miv),(isOff_dp_Cir_Mnv_Anr,Mnv),(isOff_fp_Ce_Mnv_Ar,Mnv),
      (isOff_fp_Cie_Mnv_Anr,Mnv),(isOff_dp_Cie_Mdv_Ar,Mdv),(isOff_dp_Ce_Mfv_Ar,Mfv),
      (isOff_dp_Cir_Miv_Ar,Miv),(isOn_fp_Cr_Miv_Anr,Miv),(isOn_fp_Cie_Mnv_Anr,Mnv),
      (isOn_fp_Cr_Mfv_Ar,Mfv),(isOn_fp_Cie_Miv_Ar,Miv),(isOn_fp_Ce_Mnv_Ar,Mnv),
      (isOn_dp_Cr_Mdv_Anr,Mdv),(isOn_dp_Cie_Mfv_Anr,Mfv),(isOn_dp_Ce_Miv_Anr,Miv),
      (isOn_dp_Cir_Mnv_Anr,Mnv),(isOn_dp_Cie_Mdv_Ar,Mdv),(isOn_dp_Ce_Mfv_Ar,Mfv),
      (isOn_dp_Cir_Miv_Ar,Miv),(isOff_dp_Cie_Mdv_Anr,Mdv),(isOff_fp_Cr_Mdv_Ar,Mdv),
      (isOff_dp_Ce_Mfv_Anr,Mfv),(isOff_fp_Cie_Mfv_Ar,Mfv),(isOff_fp_Cr_Mfv_Anr,Mfv),
      (isOff_dp_Cir_Miv_Anr,Miv),(isOff_fp_Ce_Miv_Ar,Miv),(isOff_fp_Cie_Miv_Anr,Miv),
      (isOff_fp_Cir_Mnv_Ar,Mnv),(isOff_fp_Ce_Mnv_Anr,Mnv),(isOff_dp_Ce_Mdv_Ar,Mdv),
      (isOff_dp_Cir_Mfv_Ar,Mfv),(isOn_fp_Cr_Mfv_Anr,Mfv),(isOn_fp_Cie_Miv_Anr,Miv),
      (isOn_fp_Ce_Mnv_Anr,Mnv),(isOn_fp_Cr_Mdv_Ar,Mdv),(isOn_fp_Cie_Mfv_Ar,Mfv),
      (isOn_fp_Ce_Miv_Ar,Miv),(isOn_fp_Cir_Mnv_Ar,Mnv),(isOn_dp_Cie_Mdv_Anr,Mdv),
      (isOn_dp_Ce_Mfv_Anr,Mfv),(isOn_dp_Cir_Miv_Anr,Miv),(isOn_dp_Ce_Mdv_Ar,Mdv),
      (isOn_dp_Cir_Mfv_Ar,Mfv),(isOff_dp_Ce_Mdv_Anr,Mdv),(isOff_fp_Cie_Mdv_Ar,Mdv),
      (isOff_fp_Cr_Mdv_Anr,Mdv),(isOff_dp_Cir_Mfv_Anr,Mfv),(isOff_fp_Ce_Mfv_Ar,Mfv),
      (isOff_fp_Cie_Mfv_Anr,Mfv),(isOff_fp_Cir_Miv_Ar,Miv),(isOff_fp_Ce_Miv_Anr,Miv),
      (isOff_fp_Cir_Mnv_Anr,Mnv),(isOff_dp_Cir_Mdv_Ar,Mdv),(isOn_fp_Cr_Mdv_Anr,Mdv),
      (isOn_fp_Cie_Mfv_Anr,Mfv),(isOn_fp_Ce_Miv_Anr,Miv),(isOn_fp_Cir_Mnv_Anr,Mnv),
      (isOn_fp_Cie_Mdv_Ar,Mdv),(isOn_fp_Ce_Mfv_Ar,Mfv),(isOn_fp_Cir_Miv_Ar,Miv),
      (isOn_dp_Ce_Mdv_Anr,Mdv),(isOn_dp_Cir_Mfv_Anr,Mfv),(isOn_dp_Cir_Mdv_Ar,Mdv),
      (isOff_dp_Cir_Mdv_Anr,Mdv),(isOff_fp_Ce_Mdv_Ar,Mdv),(isOff_fp_Cie_Mdv_Anr,Mdv),
      (isOff_fp_Cir_Mfv_Ar,Mfv),(isOff_fp_Ce_Mfv_Anr,Mfv),(isOff_fp_Cir_Miv_Anr,Miv),
      (isOn_fp_Cie_Mdv_Anr,Mdv),(isOn_fp_Ce_Mfv_Anr,Mfv),(isOn_fp_Cir_Miv_Anr,Miv),
      (isOn_fp_Ce_Mdv_Ar,Mdv),(isOn_fp_Cir_Mfv_Ar,Mfv),(isOn_dp_Cir_Mdv_Anr,Mdv),
      (isOff_fp_Cir_Mdv_Ar,Mdv),(isOff_fp_Ce_Mdv_Anr,Mdv),(isOff_fp_Cir_Mfv_Anr,Mfv),
      (isOn_fp_Ce_Mdv_Anr,Mdv),(isOn_fp_Cir_Mfv_Anr,Mfv),(isOn_fp_Cir_Mdv_Ar,Mdv),
      (isOff_fp_Cir_Mdv_Anr,Mdv),(isOn_fp_Cir_Mdv_Anr,Mdv) }
 &
 arm = {
      (isOff_dp_Cr_Mnv_Ar,Ar),(isOn_dp_Cr_Mnv_Ar,Ar),(isOff_dp_Cr_Mnv_Anr,Anr),
      (isOff_dp_Cr_Miv_Ar,Ar),(isOff_dp_Cie_Mnv_Ar,Ar),(isOn_dp_Cr_Mnv_Anr,Anr),
      (isOn_dp_Cr_Miv_Ar,Ar),(isOn_dp_Cie_Mnv_Ar,Ar),(isOff_dp_Cr_Miv_Anr,Anr),
      (isOff_dp_Cie_Mnv_Anr,Anr),(isOff_fp_Cr_Mnv_Ar,Ar),(isOff_dp_Cr_Mfv_Ar,Ar),
      (isOff_dp_Cie_Miv_Ar,Ar),(isOff_dp_Ce_Mnv_Ar,Ar),(isOn_fp_Cr_Mnv_Ar,Ar),
      (isOn_dp_Cr_Miv_Anr,Anr),(isOn_dp_Cie_Mnv_Anr,Anr),(isOn_dp_Cr_Mfv_Ar,Ar),
      (isOn_dp_Cie_Miv_Ar,Ar),(isOn_dp_Ce_Mnv_Ar,Ar),(isOff_dp_Cr_Mfv_Anr,Anr),
      (isOff_dp_Cie_Miv_Anr,Anr),(isOff_fp_Cr_Miv_Ar,Ar),(isOff_dp_Ce_Mnv_Anr,Anr),
      (isOff_fp_Cie_Mnv_Ar,Ar),(isOff_fp_Cr_Mnv_Anr,Anr),(isOff_dp_Cr_Mdv_Ar,Ar),
      (isOff_dp_Cie_Mfv_Ar,Ar),(isOff_dp_Ce_Miv_Ar,Ar),(isOff_dp_Cir_Mnv_Ar,Ar),
      (isOn_fp_Cr_Mnv_Anr,Anr),(isOn_fp_Cr_Miv_Ar,Ar),(isOn_fp_Cie_Mnv_Ar,Ar),
      (isOn_dp_Cr_Mfv_Anr,Anr),(isOn_dp_Cie_Miv_Anr,Anr),(isOn_dp_Ce_Mnv_Anr,Anr),
      (isOn_dp_Cr_Mdv_Ar,Ar),(isOn_dp_Cie_Mfv_Ar,Ar),(isOn_dp_Ce_Miv_Ar,Ar),
      (isOn_dp_Cir_Mnv_Ar,Ar),(isOff_dp_Cr_Mdv_Anr,Anr),(isOff_dp_Cie_Mfv_Anr,Anr),
      (isOff_fp_Cr_Mfv_Ar,Ar),(isOff_dp_Ce_Miv_Anr,Anr),(isOff_fp_Cie_Miv_Ar,Ar),
      (isOff_fp_Cr_Miv_Anr,Anr),(isOff_dp_Cir_Mnv_Anr,Anr),(isOff_fp_Ce_Mnv_Ar,Ar),
      (isOff_fp_Cie_Mnv_Anr,Anr),(isOff_dp_Cie_Mdv_Ar,Ar),(isOff_dp_Ce_Mfv_Ar,Ar),
      (isOff_dp_Cir_Miv_Ar,Ar),(isOn_fp_Cr_Miv_Anr,Anr),(isOn_fp_Cie_Mnv_Anr,Anr),
      (isOn_fp_Cr_Mfv_Ar,Ar),(isOn_fp_Cie_Miv_Ar,Ar),(isOn_fp_Ce_Mnv_Ar,Ar),
      (isOn_dp_Cr_Mdv_Anr,Anr),(isOn_dp_Cie_Mfv_Anr,Anr),(isOn_dp_Ce_Miv_Anr,Anr),
      (isOn_dp_Cir_Mnv_Anr,Anr),(isOn_dp_Cie_Mdv_Ar,Ar),(isOn_dp_Ce_Mfv_Ar,Ar),
      (isOn_dp_Cir_Miv_Ar,Ar),(isOff_dp_Cie_Mdv_Anr,Anr),(isOff_fp_Cr_Mdv_Ar,Ar),
      (isOff_dp_Ce_Mfv_Anr,Anr),(isOff_fp_Cie_Mfv_Ar,Ar),(isOff_fp_Cr_Mfv_Anr,Anr),
      (isOff_dp_Cir_Miv_Anr,Anr),(isOff_fp_Ce_Miv_Ar,Ar),(isOff_fp_Cie_Miv_Anr,Anr),
      (isOff_fp_Cir_Mnv_Ar,Ar),(isOff_fp_Ce_Mnv_Anr,Anr),(isOff_dp_Ce_Mdv_Ar,Ar),
      (isOff_dp_Cir_Mfv_Ar,Ar),(isOn_fp_Cr_Mfv_Anr,Anr),(isOn_fp_Cie_Miv_Anr,Anr),
      (isOn_fp_Ce_Mnv_Anr,Anr),(isOn_fp_Cr_Mdv_Ar,Ar),(isOn_fp_Cie_Mfv_Ar,Ar),
      (isOn_fp_Ce_Miv_Ar,Ar),(isOn_fp_Cir_Mnv_Ar,Ar),(isOn_dp_Cie_Mdv_Anr,Anr),
      (isOn_dp_Ce_Mfv_Anr,Anr),(isOn_dp_Cir_Miv_Anr,Anr),(isOn_dp_Ce_Mdv_Ar,Ar),
      (isOn_dp_Cir_Mfv_Ar,Ar),(isOff_dp_Ce_Mdv_Anr,Anr),(isOff_fp_Cie_Mdv_Ar,Ar),
      (isOff_fp_Cr_Mdv_Anr,Anr),(isOff_dp_Cir_Mfv_Anr,Anr),(isOff_fp_Ce_Mfv_Ar,Ar),
      (isOff_fp_Cie_Mfv_Anr,Anr),(isOff_fp_Cir_Miv_Ar,Ar),(isOff_fp_Ce_Miv_Anr,Anr),
      (isOff_fp_Cir_Mnv_Anr,Anr),(isOff_dp_Cir_Mdv_Ar,Ar),(isOn_fp_Cr_Mdv_Anr,Anr),
      (isOn_fp_Cie_Mfv_Anr,Anr),(isOn_fp_Ce_Miv_Anr,Anr),(isOn_fp_Cir_Mnv_Anr,Anr),
      (isOn_fp_Cie_Mdv_Ar,Ar),(isOn_fp_Ce_Mfv_Ar,Ar),(isOn_fp_Cir_Miv_Ar,Ar),
      (isOn_dp_Ce_Mdv_Anr,Anr),(isOn_dp_Cir_Mfv_Anr,Anr),(isOn_dp_Cir_Mdv_Ar,Ar),
      (isOff_dp_Cir_Mdv_Anr,Anr),(isOff_fp_Ce_Mdv_Ar,Ar),(isOff_fp_Cie_Mdv_Anr,Anr),
      (isOff_fp_Cir_Mfv_Ar,Ar),(isOff_fp_Ce_Mfv_Anr,Anr),(isOff_fp_Cir_Miv_Anr,Anr),
      (isOn_fp_Cie_Mdv_Anr,Anr),(isOn_fp_Ce_Mfv_Anr,Anr),(isOn_fp_Cir_Miv_Anr,Anr),
      (isOn_fp_Ce_Mdv_Ar,Ar),(isOn_fp_Cir_Mfv_Ar,Ar),(isOn_dp_Cir_Mdv_Anr,Anr),
      (isOff_fp_Cir_Mdv_Ar,Ar),(isOff_fp_Ce_Mdv_Anr,Anr),(isOff_fp_Cir_Mfv_Anr,Anr),
      (isOn_fp_Ce_Mdv_Anr,Anr),(isOn_fp_Cir_Mfv_Anr,Anr),(isOn_fp_Cir_Mdv_Ar,Ar),
      (isOff_fp_Cir_Mdv_Anr,Anr),(isOn_fp_Cir_Mdv_Anr,Anr) }
 &

/*
  Standard constraints:
    1. The drilling process cannot start unless all devices are in their initial state.
    2. During the drilling phase, the drill cannot attempt to unclench the clamp and
       reduce the motor velocity.
    3. During the final phase, the drill cannot attempt to clench the clamp and
       increase the motor velocity.
    4. During the final phase, the drill cannot attempt to descend the arm.
  Implicit constraint:
    5. The arm should not operate unless the clamp is completely extended and the
       motor is at full velocity.
*/
 BadStates =
// The drill is off, the clamp is completely extended, in extension or
// in retraction, the motor is at full speed or increasing or decreasing its velocity,
// and the arm is not retracted:
  {x | x : States & switch(x) = isOff & (clamp(x) : {Ce,Cie,Cir} or
                    motor(x) : {Mfv,Mdv,Miv} or arm(x) = Anr) } \/
// The drill is in the drilling phase, the clamp is in retraction or the motor is
// decreasing its velocity:
  {x | x : States & operation(x) = dp & (clamp(x) = Cir or motor(x) = Mdv)} \/
// The drill is in the final phase, the clamp is in extension or the motor is
// increasing its velocity:
  {x | x : States & operation(x) = fp & (clamp(x) = Cie or motor(x) = Miv)} \/
// The drill is in the final phase and the arm is not completely retracted:
  {x | x : States & operation(x) = fp & arm(x) = Anr} \/
// The arm is not retracted, the clamp is completely retracted, in extension or in
// retraction, and the motor has no velocity or is increasing or decreasing its
// velocity:
  {x | x : States & arm(x) = Anr & (clamp(x) : {Cie,Cir,Cr} or
       motor(x) : {Mnv,Mdv,Miv} ) }
 &
 Qpred = States - BadStates
END

/*
The transition structure of the agent
 (i.e.,the solution of the control problem)
Number of states of the exhaustive model: 128
Number of transitions of the exhaustive model: 480
Cardinality of the predicate: 21
Supremal coreachable and controllable predicates (20):
{isOff_dp_Cr_Mnv_Ar,isOn_dp_Cr_Mnv_Ar,isOn_dp_Cr_Miv_Ar,isOn_dp_Cie_Mnv_Ar,
 isOn_fp_Cr_Mnv_Ar,isOn_dp_Cr_Mfv_Ar,isOn_dp_Cie_Miv_Ar,isOn_dp_Ce_Mnv_Ar,
 isOn_dp_Cie_Mfv_Ar,isOn_dp_Ce_Miv_Ar,isOn_fp_Cr_Mfv_Ar,isOn_fp_Ce_Mnv_Ar,
 isOn_dp_Ce_Mfv_Ar,isOn_fp_Cr_Mdv_Ar,isOn_fp_Cir_Mnv_Ar,isOn_dp_Ce_Mfv_Anr,
 isOn_fp_Ce_Mfv_Ar,isOn_fp_Ce_Mdv_Ar,isOn_fp_Cir_Mfv_Ar,isOn_fp_Cir_Mdv_Ar}
Disabled events:
{(isOff_dp_Cr_Mnv_Ar|->{C_clamp,M_tOn,A_gd}),(isOn_dp_Cr_Mnv_Ar|->{A_gd}),
 (isOn_dp_Cr_Miv_Ar|->{A_gd}),(isOn_dp_Cie_Mnv_Ar|->{A_gd}),
 (isOn_fp_Cr_Mnv_Ar|->{C_clamp,M_tOn,A_gd}),(isOn_dp_Cr_Mfv_Ar|->{M_tOff,A_gd}),
 (isOn_dp_Cie_Miv_Ar|->{A_gd}),(isOn_dp_Ce_Mnv_Ar|->{C_unclamp,A_gd}),
 (isOn_dp_Cie_Mfv_Ar|->{M_tOff,A_gd}),(isOn_dp_Ce_Miv_Ar|->{C_unclamp,A_gd}),
 (isOn_fp_Cr_Mfv_Ar|->{stop,C_clamp,A_gd}),(isOn_fp_Ce_Mnv_Ar|->{stop,M_tOn,A_gd}),
 (isOn_dp_Ce_Mfv_Ar|->{C_unclamp,M_tOff}),(isOn_fp_Cr_Mdv_Ar|->{stop,C_clamp,A_gd}),
 (isOn_fp_Cir_Mnv_Ar|->{stop,M_tOn,A_gd}),(isOn_dp_Ce_Mfv_Anr|->{C_unclamp,M_tOff}),
 (isOn_fp_Ce_Mfv_Ar|->{stop,A_gd}),(isOn_fp_Ce_Mdv_Ar|->{stop,A_gd}),
 (isOn_fp_Cir_Mfv_Ar|->{stop,A_gd}),(isOn_fp_Cir_Mdv_Ar|->{stop,A_gd})}
Enabled events:
{(isOff_dp_Cr_Mnv_Ar|->{start}),(isOn_dp_Cr_Mnv_Ar|->{C_clamp,M_tOn}),
 (isOn_dp_Cr_Miv_Ar|->{C_clamp,M_fs}),(isOn_dp_Cie_Mnv_Ar|->{C_eoc,M_tOn})
 (isOn_fp_Cr_Mnv_Ar|->{stop}),(isOn_dp_Cr_Mfv_Ar|->{C_clamp}),
 (isOn_dp_Cie_Miv_Ar|->{C_eoc,M_fs}),(isOn_dp_Ce_Mnv_Ar|->{M_tOn}),
 (isOn_dp_Cie_Mfv_Ar|->{C_eoc}),(isOn_dp_Ce_Miv_Ar|->{M_fs}),
 (isOn_fp_Cr_Mfv_Ar|->{M_tOff}),(isOn_fp_Ce_Mnv_Ar|->{C_unclamp}),
 (isOn_dp_Ce_Mfv_Ar|->{A_gd}),(isOn_fp_Cr_Mdv_Ar|->{M_ns}),
 (isOn_fp_Cir_Mnv_Ar|->{C_eou}),(isOn_dp_Ce_Mfv_Anr|->{A_eoa}),
 (isOn_fp_Ce_Mfv_Ar|->{C_unclamp,M_tOff}),(isOn_fp_Ce_Mdv_Ar|->{C_unclamp,M_ns}),
 (isOn_fp_Cir_Mfv_Ar|->{C_eou,M_tOff}),(isOn_fp_Cir_Mdv_Ar|->{C_eou,M_ns})}
Closed-loop system:
 rec(Xm:{isOff_dp_Cr_Mnv_Ar},
     ctrlEvents:{start,stop,C_clamp,C_unclamp,M_tOn,M_tOff,A_gd},
     events:{start,stop,A_eoa,C_eoc,C_eou,C_clamp,C_unclamp,M_tOn,M_tOff,M_fs,M_ns,A_gd},
     states:{isOff_dp_Cr_Mnv_Ar,isOn_dp_Cr_Mnv_Ar,isOn_dp_Cr_Miv_Ar,isOn_dp_Cie_Mnv_Ar,
             isOn_fp_Cr_Mnv_Ar,isOn_dp_Cr_Mfv_Ar,isOn_dp_Cie_Miv_Ar,isOn_dp_Ce_Mnv_Ar,
             isOn_dp_Cie_Mfv_Ar,isOn_dp_Ce_Miv_Ar,isOn_fp_Cr_Mfv_Ar,isOn_fp_Ce_Mnv_Ar,
             isOn_dp_Ce_Mfv_Ar,isOn_fp_Cr_Mdv_Ar,isOn_fp_Cir_Mnv_Ar,isOn_dp_Ce_Mfv_Anr,
             isOn_fp_Ce_Mfv_Ar,isOn_fp_Ce_Mdv_Ar,isOn_fp_Cir_Mfv_Ar,isOn_fp_Cir_Mdv_Ar},
     tf:{((isOff_dp_Cr_Mnv_Ar|->start)|->isOn_dp_Cr_Mnv_Ar),
         ((isOn_dp_Cr_Mnv_Ar|->C_clamp)|->isOn_dp_Cie_Mnv_Ar),
         ((isOn_dp_Cr_Mnv_Ar|->M_tOn)|->isOn_dp_Cr_Miv_Ar),
         ((isOn_dp_Cr_Miv_Ar|->C_clamp)|->isOn_dp_Cie_Miv_Ar),
         ((isOn_dp_Cr_Miv_Ar|->M_fs)|->isOn_dp_Cr_Mfv_Ar),
         ((isOn_dp_Cie_Mnv_Ar|->C_eoc)|->isOn_dp_Ce_Mnv_Ar),
         ((isOn_dp_Cie_Mnv_Ar|->M_tOn)|->isOn_dp_Cie_Miv_Ar),
         ((isOn_fp_Cr_Mnv_Ar|->stop)|->isOff_dp_Cr_Mnv_Ar),
         ((isOn_dp_Cr_Mfv_Ar|->C_clamp)|->isOn_dp_Cie_Mfv_Ar),
         ((isOn_dp_Cie_Miv_Ar|->C_eoc)|->isOn_dp_Ce_Miv_Ar),
         ((isOn_dp_Cie_Miv_Ar|->M_fs)|->isOn_dp_Cie_Mfv_Ar),
         ((isOn_dp_Ce_Mnv_Ar|->M_tOn)|->isOn_dp_Ce_Miv_Ar),
         ((isOn_dp_Cie_Mfv_Ar|->C_eoc)|->isOn_dp_Ce_Mfv_Ar),
         ((isOn_dp_Ce_Miv_Ar|->M_fs)|->isOn_dp_Ce_Mfv_Ar),
         ((isOn_fp_Cr_Mfv_Ar|->M_tOff)|->isOn_fp_Cr_Mdv_Ar),
         ((isOn_fp_Ce_Mnv_Ar|->C_unclamp)|->isOn_fp_Cir_Mnv_Ar),
         ((isOn_dp_Ce_Mfv_Ar|->A_gd)|->isOn_dp_Ce_Mfv_Anr),
         ((isOn_fp_Cr_Mdv_Ar|->M_ns)|->isOn_fp_Cr_Mnv_Ar),
         ((isOn_fp_Cir_Mnv_Ar|->C_eou)|->isOn_fp_Cr_Mnv_Ar),
         ((isOn_dp_Ce_Mfv_Anr|->A_eoa)|->isOn_fp_Ce_Mfv_Ar),
         ((isOn_fp_Ce_Mfv_Ar|->C_unclamp)|->isOn_fp_Cir_Mfv_Ar),
         ((isOn_fp_Ce_Mfv_Ar|->M_tOff)|->isOn_fp_Ce_Mdv_Ar),
         ((isOn_fp_Ce_Mdv_Ar|->C_unclamp)|->isOn_fp_Cir_Mdv_Ar),
         ((isOn_fp_Ce_Mdv_Ar|->M_ns)|->isOn_fp_Ce_Mnv_Ar),
         ((isOn_fp_Cir_Mfv_Ar|->C_eou)|->isOn_fp_Cr_Mfv_Ar),
         ((isOn_fp_Cir_Mfv_Ar|->M_tOff)|->isOn_fp_Cir_Mdv_Ar),
         ((isOn_fp_Cir_Mdv_Ar|->C_eou)|->isOn_fp_Cr_Mdv_Ar),
         ((isOn_fp_Cir_Mdv_Ar|->M_ns)|->isOn_fp_Cir_Mnv_Ar)},
     x_0:isOff_dp_Cr_Mnv_Ar)
*/

/*
  ControlLaw(start) = isOff
  ControlLaw(stop) = fp & Cr & Mnv
  ControlLaw(C_clamp) = isOn & dp
  ControlLaw(C_unclamp) = isOn & fp
  ControlLaw(M_tOn) = isOn & dp
  ControlLaw(M_tOff) = isOn & fp
  ControlLaw(A_gd) = isOn & dp & Ce & Mfv
*/

/*
% Runtime for SOLUTION for SETUP_CONSTANTS: 120 ms (walltime: 120 ms)
% Finished searching for valid initial states 140 ms walltime (140 ms runtime), since start: 2020 ms
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
finished_random_animate(5,[runtime/120,total_runtime/120,walltime/130])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 330 ms walltime (320 ms runtime), since start: 2150 ms

% Finished process_file (errors=0, warnings=0)
*/