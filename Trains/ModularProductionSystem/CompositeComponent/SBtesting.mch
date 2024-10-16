// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBtesting

SETS
 States = {idle_Lr_Midl_Er, waiting_Lr_Midl_Er, idle_Lr_Midl_Enr,
           idle_Lr_Mim_Er, idle_Lie_Midl_Er, waiting_Lie_Midl_Er,
           waiting_Lr_Midl_Enr, waiting_Lr_Mim_Er, idle_Lr_Mim_Enr,
           idle_Lie_Midl_Enr, idle_Lie_Mim_Er, idle_Lr_Mrk_Er,
           idle_Le_Midl_Er, waiting_Le_Midl_Er, waiting_Lie_Midl_Enr,
           waiting_Lie_Mim_Er, waiting_Lr_Mim_Enr, passm1_Lr_Mrk_Er,
           failm1_Lr_Mrk_Er, idle_Lie_Mim_Enr, idle_Lr_Mrk_Enr,
           idle_Le_Midl_Enr, idle_Le_Mim_Er, idle_Lie_Mrk_Er,
           waiting_Lr_Mrk_Er, idle_Lr_Mmc_Er, idle_Lir_Midl_Er,
           waiting_Lir_Midl_Er, waiting_Le_Midl_Enr, waiting_Le_Mim_Er,
           waiting_Lie_Mim_Enr, passm1_Lie_Mrk_Er, failm1_Lie_Mrk_Er,
           passm1_Lr_Mrk_Enr, failm1_Lr_Mrk_Enr, passm2_Lr_Mmc_Er,
           failm2_Lr_Mmc_Er, idle_Le_Mim_Enr, idle_Lie_Mrk_Enr,
           waiting_Lr_Mrk_Enr, idle_Lr_Mmc_Enr, idle_Lir_Midl_Enr,
           idle_Lir_Mim_Er, idle_Le_Mrk_Er, waiting_Lie_Mrk_Er,
           idle_Lie_Mmc_Er, waiting_Lr_Mmc_Er, waiting_Lir_Mim_Er,
           waiting_Lir_Midl_Enr, waiting_Le_Mim_Enr, passm1_Le_Mrk_Er,
           failm1_Le_Mrk_Er, passm1_Lie_Mrk_Enr, failm1_Lie_Mrk_Enr,
           passm2_Lie_Mmc_Er, failm2_Lie_Mmc_Er, passm2_Lr_Mmc_Enr,
           failm2_Lr_Mmc_Enr, idle_Lir_Mim_Enr, idle_Le_Mrk_Enr,
           waiting_Lie_Mrk_Enr, idle_Lie_Mmc_Enr, waiting_Lr_Mmc_Enr,
           idle_Lir_Mrk_Er, waiting_Le_Mrk_Er, idle_Le_Mmc_Er,
           waiting_Lie_Mmc_Er, passm1_Lir_Mrk_Er, waiting_Lir_Mim_Enr,
           failm1_Lir_Mrk_Er, passm1_Le_Mrk_Enr, failm1_Le_Mrk_Enr,
           passm2_Le_Mmc_Er, failm2_Le_Mmc_Er, passm2_Lie_Mmc_Enr,
           failm2_Lie_Mmc_Enr, passm3_Lr_Mmc_Er, idle_Lir_Mrk_Enr,
           waiting_Le_Mrk_Enr, idle_Le_Mmc_Enr, waiting_Lie_Mmc_Enr,
           waiting_Lir_Mrk_Er, idle_Lir_Mmc_Er, waiting_Le_Mmc_Er,
           passm2_Lir_Mmc_Er, passm1_Lir_Mrk_Enr, failm1_Lir_Mrk_Enr,
           failm2_Lir_Mmc_Er, passm2_Le_Mmc_Enr, failm2_Le_Mmc_Enr,
           passm3_Lie_Mmc_Er, passm3_Lr_Mmc_Enr, waiting_Lir_Mrk_Enr,
           idle_Lir_Mmc_Enr, waiting_Le_Mmc_Enr, waiting_Lir_Mmc_Er,
           passm2_Lir_Mmc_Enr, failm2_Lir_Mmc_Enr, failm3_Lr_Mmc_Er,
           passm3_Le_Mmc_Er, passm3_Lie_Mmc_Enr, waiting_Lir_Mmc_Enr,
           passm3_Lir_Mmc_Er, failm3_Lr_Mmc_Enr, failm3_Lie_Mmc_Er,
           passm3_Le_Mmc_Enr, passm4_Lr_Mmc_Er, passm3_Lir_Mmc_Enr,
           failm4_Lr_Mmc_Er, failm3_Lie_Mmc_Enr, failm3_Le_Mmc_Er,
           passm4_Lr_Mmc_Enr, passm4_Lie_Mmc_Er, failm4_Lr_Mmc_Enr,
           failm4_Lie_Mmc_Er, failm3_Le_Mmc_Enr, failm3_Lir_Mmc_Er,
           passm4_Lie_Mmc_Enr, passm4_Le_Mmc_Er, failm4_Lie_Mmc_Enr,
           failm4_Le_Mmc_Er, failm3_Lir_Mmc_Enr, passm4_Le_Mmc_Enr,
           passm4_Lir_Mmc_Er, failm4_Le_Mmc_Enr, failm4_Lir_Mmc_Er,
           passm4_Lir_Mmc_Enr, failm4_Lir_Mmc_Enr };
 Events = {start, L_eolu, L_eold, M_eot, M_fail, M_reset, M_pass, E_eoe, L_liftdown, L_liftup, M_test, E_evacuate};
 LocalStates = {idle, waiting, passm1, passm2, passm3, passm4, failm1, failm2, failm3, failm4, Le, Lie, Lir, Lr, Midl, Mim, Mmc, Mrk, Enr, Er}

CONSTANTS
 G_FB,
 seqT, lift, micrometer, evacuator,
 Qpred, BadStates

PROPERTIES
 G_FB =rec(   // Automaton modeling the free behavior
      states: States,
      events: Events,
      tf: {(idle_Lr_Midl_Er,start,waiting_Lr_Midl_Er), (idle_Lr_Midl_Er,E_evacuate,idle_Lr_Midl_Enr), (idle_Lr_Midl_Er,M_test,idle_Lr_Mim_Er),
           (idle_Lr_Midl_Er,L_liftup,idle_Lie_Midl_Er), (waiting_Lr_Midl_Er,L_liftup,waiting_Lie_Midl_Er), (waiting_Lr_Midl_Er,E_evacuate,waiting_Lr_Midl_Enr),
           (waiting_Lr_Midl_Er,M_test,waiting_Lr_Mim_Er), (idle_Lr_Midl_Enr,start,waiting_Lr_Midl_Enr), (idle_Lr_Midl_Enr,M_test,idle_Lr_Mim_Enr),
           (idle_Lr_Midl_Enr,L_liftup,idle_Lie_Midl_Enr), (idle_Lr_Midl_Enr,E_eoe,idle_Lr_Midl_Er), (idle_Lr_Mim_Er,start,waiting_Lr_Mim_Er),
           (idle_Lr_Mim_Er,E_evacuate,idle_Lr_Mim_Enr), (idle_Lr_Mim_Er,L_liftup,idle_Lie_Mim_Er), (idle_Lr_Mim_Er,M_fail,idle_Lr_Mrk_Er),
           (idle_Lr_Mim_Er,M_pass,idle_Lr_Mrk_Er), (idle_Lie_Midl_Er,start,waiting_Lie_Midl_Er), (idle_Lie_Midl_Er,E_evacuate,idle_Lie_Midl_Enr),
           (idle_Lie_Midl_Er,M_test,idle_Lie_Mim_Er), (idle_Lie_Midl_Er,L_eolu,idle_Le_Midl_Er), (waiting_Lie_Midl_Er,L_eolu,waiting_Le_Midl_Er),
           (waiting_Lie_Midl_Er,E_evacuate,waiting_Lie_Midl_Enr), (waiting_Lie_Midl_Er,M_test,waiting_Lie_Mim_Er), (waiting_Lr_Midl_Enr,L_liftup,waiting_Lie_Midl_Enr),
           (waiting_Lr_Midl_Enr,M_test,waiting_Lr_Mim_Enr), (waiting_Lr_Midl_Enr,E_eoe,waiting_Lr_Midl_Er), (waiting_Lr_Mim_Er,L_liftup,waiting_Lie_Mim_Er),
           (waiting_Lr_Mim_Er,E_evacuate,waiting_Lr_Mim_Enr), (waiting_Lr_Mim_Er,M_pass,passm1_Lr_Mrk_Er), (waiting_Lr_Mim_Er,M_fail,failm1_Lr_Mrk_Er),
           (idle_Lr_Mim_Enr,start,waiting_Lr_Mim_Enr), (idle_Lr_Mim_Enr,L_liftup,idle_Lie_Mim_Enr), (idle_Lr_Mim_Enr,M_fail,idle_Lr_Mrk_Enr),
           (idle_Lr_Mim_Enr,M_pass,idle_Lr_Mrk_Enr), (idle_Lr_Mim_Enr,E_eoe,idle_Lr_Mim_Er), (idle_Lie_Midl_Enr,start,waiting_Lie_Midl_Enr),
           (idle_Lie_Midl_Enr,M_test,idle_Lie_Mim_Enr), (idle_Lie_Midl_Enr,L_eolu,idle_Le_Midl_Enr), (idle_Lie_Midl_Enr,E_eoe,idle_Lie_Midl_Er),
           (idle_Lie_Mim_Er,start,waiting_Lie_Mim_Er), (idle_Lie_Mim_Er,E_evacuate,idle_Lie_Mim_Enr), (idle_Lie_Mim_Er,L_eolu,idle_Le_Mim_Er),
           (idle_Lie_Mim_Er,M_fail,idle_Lie_Mrk_Er), (idle_Lie_Mim_Er,M_pass,idle_Lie_Mrk_Er), (idle_Lr_Mrk_Er,start,waiting_Lr_Mrk_Er),
           (idle_Lr_Mrk_Er,E_evacuate,idle_Lr_Mrk_Enr), (idle_Lr_Mrk_Er,L_liftup,idle_Lie_Mrk_Er), (idle_Lr_Mrk_Er,M_eot,idle_Lr_Mmc_Er),
           (idle_Le_Midl_Er,start,waiting_Le_Midl_Er), (idle_Le_Midl_Er,E_evacuate,idle_Le_Midl_Enr), (idle_Le_Midl_Er,M_test,idle_Le_Mim_Er),
           (idle_Le_Midl_Er,L_liftdown,idle_Lir_Midl_Er), (waiting_Le_Midl_Er,L_liftdown,waiting_Lir_Midl_Er), (waiting_Le_Midl_Er,E_evacuate,waiting_Le_Midl_Enr),
           (waiting_Le_Midl_Er,M_test,waiting_Le_Mim_Er), (waiting_Lie_Midl_Enr,L_eolu,waiting_Le_Midl_Enr), (waiting_Lie_Midl_Enr,M_test,waiting_Lie_Mim_Enr),
           (waiting_Lie_Midl_Enr,E_eoe,waiting_Lie_Midl_Er), (waiting_Lie_Mim_Er,L_eolu,waiting_Le_Mim_Er), (waiting_Lie_Mim_Er,E_evacuate,waiting_Lie_Mim_Enr),
           (waiting_Lie_Mim_Er,M_pass,passm1_Lie_Mrk_Er), (waiting_Lie_Mim_Er,M_fail,failm1_Lie_Mrk_Er), (waiting_Lr_Mim_Enr,L_liftup,waiting_Lie_Mim_Enr),
           (waiting_Lr_Mim_Enr,M_pass,passm1_Lr_Mrk_Enr), (waiting_Lr_Mim_Enr,M_fail,failm1_Lr_Mrk_Enr), (waiting_Lr_Mim_Enr,E_eoe,waiting_Lr_Mim_Er),
           (passm1_Lr_Mrk_Er,M_eot,passm2_Lr_Mmc_Er), (passm1_Lr_Mrk_Er,E_evacuate,passm1_Lr_Mrk_Enr), (passm1_Lr_Mrk_Er,L_liftup,passm1_Lie_Mrk_Er),
           (failm1_Lr_Mrk_Er,M_eot,failm2_Lr_Mmc_Er), (failm1_Lr_Mrk_Er,E_evacuate,failm1_Lr_Mrk_Enr), (failm1_Lr_Mrk_Er,L_liftup,failm1_Lie_Mrk_Er),
           (idle_Lie_Mim_Enr,start,waiting_Lie_Mim_Enr), (idle_Lie_Mim_Enr,L_eolu,idle_Le_Mim_Enr), (idle_Lie_Mim_Enr,M_fail,idle_Lie_Mrk_Enr),
           (idle_Lie_Mim_Enr,M_pass,idle_Lie_Mrk_Enr), (idle_Lie_Mim_Enr,E_eoe,idle_Lie_Mim_Er), (idle_Lr_Mrk_Enr,start,waiting_Lr_Mrk_Enr),
           (idle_Lr_Mrk_Enr,L_liftup,idle_Lie_Mrk_Enr), (idle_Lr_Mrk_Enr,M_eot,idle_Lr_Mmc_Enr), (idle_Lr_Mrk_Enr,E_eoe,idle_Lr_Mrk_Er),
           (idle_Le_Midl_Enr,start,waiting_Le_Midl_Enr), (idle_Le_Midl_Enr,M_test,idle_Le_Mim_Enr), (idle_Le_Midl_Enr,L_liftdown,idle_Lir_Midl_Enr),
           (idle_Le_Midl_Enr,E_eoe,idle_Le_Midl_Er), (idle_Le_Mim_Er,start,waiting_Le_Mim_Er), (idle_Le_Mim_Er,E_evacuate,idle_Le_Mim_Enr),
           (idle_Le_Mim_Er,L_liftdown,idle_Lir_Mim_Er), (idle_Le_Mim_Er,M_fail,idle_Le_Mrk_Er), (idle_Le_Mim_Er,M_pass,idle_Le_Mrk_Er),
           (idle_Lie_Mrk_Er,start,waiting_Lie_Mrk_Er), (idle_Lie_Mrk_Er,E_evacuate,idle_Lie_Mrk_Enr), (idle_Lie_Mrk_Er,L_eolu,idle_Le_Mrk_Er),
           (idle_Lie_Mrk_Er,M_eot,idle_Lie_Mmc_Er), (waiting_Lr_Mrk_Er,L_liftup,waiting_Lie_Mrk_Er), (waiting_Lr_Mrk_Er,E_evacuate,waiting_Lr_Mrk_Enr),
           (waiting_Lr_Mrk_Er,M_eot,waiting_Lr_Mmc_Er), (idle_Lr_Mmc_Er,start,waiting_Lr_Mmc_Er), (idle_Lr_Mmc_Er,E_evacuate,idle_Lr_Mmc_Enr),
           (idle_Lr_Mmc_Er,L_liftup,idle_Lie_Mmc_Er), (idle_Lir_Midl_Er,start,waiting_Lir_Midl_Er), (idle_Lir_Midl_Er,E_evacuate,idle_Lir_Midl_Enr),
           (idle_Lir_Midl_Er,M_test,idle_Lir_Mim_Er), (idle_Lir_Midl_Er,L_eold,idle_Lr_Midl_Er), (waiting_Lir_Midl_Er,M_test,waiting_Lir_Mim_Er),
           (waiting_Lir_Midl_Er,E_evacuate,waiting_Lir_Midl_Enr), (waiting_Lir_Midl_Er,L_eold,waiting_Lr_Midl_Er), (waiting_Le_Midl_Enr,L_liftdown,waiting_Lir_Midl_Enr),
           (waiting_Le_Midl_Enr,M_test,waiting_Le_Mim_Enr), (waiting_Le_Midl_Enr,E_eoe,waiting_Le_Midl_Er), (waiting_Le_Mim_Er,L_liftdown,waiting_Lir_Mim_Er),
           (waiting_Le_Mim_Er,E_evacuate,waiting_Le_Mim_Enr), (waiting_Le_Mim_Er,M_pass,passm1_Le_Mrk_Er), (waiting_Le_Mim_Er,M_fail,failm1_Le_Mrk_Er),
           (waiting_Lie_Mim_Enr,L_eolu,waiting_Le_Mim_Enr), (waiting_Lie_Mim_Enr,M_pass,passm1_Lie_Mrk_Enr), (waiting_Lie_Mim_Enr,M_fail,failm1_Lie_Mrk_Enr),
           (waiting_Lie_Mim_Enr,E_eoe,waiting_Lie_Mim_Er), (passm1_Lie_Mrk_Er,M_eot,passm2_Lie_Mmc_Er), (passm1_Lie_Mrk_Er,E_evacuate,passm1_Lie_Mrk_Enr),
           (passm1_Lie_Mrk_Er,L_eolu,passm1_Le_Mrk_Er), (failm1_Lie_Mrk_Er,M_eot,failm2_Lie_Mmc_Er), (failm1_Lie_Mrk_Er,E_evacuate,failm1_Lie_Mrk_Enr),
           (failm1_Lie_Mrk_Er,L_eolu,failm1_Le_Mrk_Er), (passm1_Lr_Mrk_Enr,M_eot,passm2_Lr_Mmc_Enr), (passm1_Lr_Mrk_Enr,L_liftup,passm1_Lie_Mrk_Enr),
           (passm1_Lr_Mrk_Enr,E_eoe,passm1_Lr_Mrk_Er), (failm1_Lr_Mrk_Enr,M_eot,failm2_Lr_Mmc_Enr), (failm1_Lr_Mrk_Enr,L_liftup,failm1_Lie_Mrk_Enr),
           (failm1_Lr_Mrk_Enr,E_eoe,failm1_Lr_Mrk_Er), (passm2_Lr_Mmc_Er,E_evacuate,passm2_Lr_Mmc_Enr), (passm2_Lr_Mmc_Er,L_liftup,passm2_Lie_Mmc_Er),
           (failm2_Lr_Mmc_Er,L_liftup,failm2_Lie_Mmc_Er), (failm2_Lr_Mmc_Er,E_evacuate,failm2_Lr_Mmc_Enr), (idle_Le_Mim_Enr,start,waiting_Le_Mim_Enr),
           (idle_Le_Mim_Enr,L_liftdown,idle_Lir_Mim_Enr), (idle_Le_Mim_Enr,M_fail,idle_Le_Mrk_Enr), (idle_Le_Mim_Enr,M_pass,idle_Le_Mrk_Enr),
           (idle_Le_Mim_Enr,E_eoe,idle_Le_Mim_Er), (idle_Lie_Mrk_Enr,start,waiting_Lie_Mrk_Enr), (idle_Lie_Mrk_Enr,L_eolu,idle_Le_Mrk_Enr),
           (idle_Lie_Mrk_Enr,M_eot,idle_Lie_Mmc_Enr), (idle_Lie_Mrk_Enr,E_eoe,idle_Lie_Mrk_Er), (waiting_Lr_Mrk_Enr,L_liftup,waiting_Lie_Mrk_Enr),
           (waiting_Lr_Mrk_Enr,M_eot,waiting_Lr_Mmc_Enr), (waiting_Lr_Mrk_Enr,E_eoe,waiting_Lr_Mrk_Er), (idle_Lr_Mmc_Enr,start,waiting_Lr_Mmc_Enr),
           (idle_Lr_Mmc_Enr,L_liftup,idle_Lie_Mmc_Enr), (idle_Lr_Mmc_Enr,E_eoe,idle_Lr_Mmc_Er), (idle_Lir_Midl_Enr,start,waiting_Lir_Midl_Enr),
           (idle_Lir_Midl_Enr,M_test,idle_Lir_Mim_Enr), (idle_Lir_Midl_Enr,L_eold,idle_Lr_Midl_Enr), (idle_Lir_Midl_Enr,E_eoe,idle_Lir_Midl_Er),
           (idle_Lir_Mim_Er,start,waiting_Lir_Mim_Er), (idle_Lir_Mim_Er,E_evacuate,idle_Lir_Mim_Enr), (idle_Lir_Mim_Er,L_eold,idle_Lr_Mim_Er),
           (idle_Lir_Mim_Er,M_fail,idle_Lir_Mrk_Er), (idle_Lir_Mim_Er,M_pass,idle_Lir_Mrk_Er), (idle_Le_Mrk_Er,start,waiting_Le_Mrk_Er),
           (idle_Le_Mrk_Er,E_evacuate,idle_Le_Mrk_Enr), (idle_Le_Mrk_Er,L_liftdown,idle_Lir_Mrk_Er), (idle_Le_Mrk_Er,M_eot,idle_Le_Mmc_Er),
           (waiting_Lie_Mrk_Er,L_eolu,waiting_Le_Mrk_Er), (waiting_Lie_Mrk_Er,E_evacuate,waiting_Lie_Mrk_Enr), (waiting_Lie_Mrk_Er,M_eot,waiting_Lie_Mmc_Er),
           (idle_Lie_Mmc_Er,start,waiting_Lie_Mmc_Er), (idle_Lie_Mmc_Er,E_evacuate,idle_Lie_Mmc_Enr), (idle_Lie_Mmc_Er,L_eolu,idle_Le_Mmc_Er),
           (waiting_Lr_Mmc_Er,L_liftup,waiting_Lie_Mmc_Er), (waiting_Lr_Mmc_Er,E_evacuate,waiting_Lr_Mmc_Enr), (waiting_Lir_Mim_Er,M_pass,passm1_Lir_Mrk_Er),
           (waiting_Lir_Mim_Er,E_evacuate,waiting_Lir_Mim_Enr), (waiting_Lir_Mim_Er,M_fail,failm1_Lir_Mrk_Er), (waiting_Lir_Mim_Er,L_eold,waiting_Lr_Mim_Er),
           (waiting_Lir_Midl_Enr,M_test,waiting_Lir_Mim_Enr), (waiting_Lir_Midl_Enr,L_eold,waiting_Lr_Midl_Enr), (waiting_Lir_Midl_Enr,E_eoe,waiting_Lir_Midl_Er),
           (waiting_Le_Mim_Enr,L_liftdown,waiting_Lir_Mim_Enr), (waiting_Le_Mim_Enr,M_pass,passm1_Le_Mrk_Enr), (waiting_Le_Mim_Enr,M_fail,failm1_Le_Mrk_Enr),
           (waiting_Le_Mim_Enr,E_eoe,waiting_Le_Mim_Er), (passm1_Le_Mrk_Er,M_eot,passm2_Le_Mmc_Er), (passm1_Le_Mrk_Er,E_evacuate,passm1_Le_Mrk_Enr),
           (passm1_Le_Mrk_Er,L_liftdown,passm1_Lir_Mrk_Er), (failm1_Le_Mrk_Er,M_eot,failm2_Le_Mmc_Er), (failm1_Le_Mrk_Er,E_evacuate,failm1_Le_Mrk_Enr),
           (failm1_Le_Mrk_Er,L_liftdown,failm1_Lir_Mrk_Er), (passm1_Lie_Mrk_Enr,M_eot,passm2_Lie_Mmc_Enr), (passm1_Lie_Mrk_Enr,L_eolu,passm1_Le_Mrk_Enr),
           (passm1_Lie_Mrk_Enr,E_eoe,passm1_Lie_Mrk_Er), (failm1_Lie_Mrk_Enr,M_eot,failm2_Lie_Mmc_Enr), (failm1_Lie_Mrk_Enr,L_eolu,failm1_Le_Mrk_Enr),
           (failm1_Lie_Mrk_Enr,E_eoe,failm1_Lie_Mrk_Er), (passm2_Lie_Mmc_Er,E_evacuate,passm2_Lie_Mmc_Enr), (passm2_Lie_Mmc_Er,L_eolu,passm2_Le_Mmc_Er),
           (failm2_Lie_Mmc_Er,L_eolu,failm2_Le_Mmc_Er), (failm2_Lie_Mmc_Er,E_evacuate,failm2_Lie_Mmc_Enr), (passm2_Lr_Mmc_Enr,E_eoe,passm3_Lr_Mmc_Er),
           (passm2_Lr_Mmc_Enr,L_liftup,passm2_Lie_Mmc_Enr), (failm2_Lr_Mmc_Enr,L_liftup,failm2_Lie_Mmc_Enr), (failm2_Lr_Mmc_Enr,E_eoe,failm2_Lr_Mmc_Er),
           (idle_Lir_Mim_Enr,start,waiting_Lir_Mim_Enr), (idle_Lir_Mim_Enr,L_eold,idle_Lr_Mim_Enr), (idle_Lir_Mim_Enr,M_fail,idle_Lir_Mrk_Enr),
           (idle_Lir_Mim_Enr,M_pass,idle_Lir_Mrk_Enr), (idle_Lir_Mim_Enr,E_eoe,idle_Lir_Mim_Er), (idle_Le_Mrk_Enr,start,waiting_Le_Mrk_Enr),
           (idle_Le_Mrk_Enr,L_liftdown,idle_Lir_Mrk_Enr), (idle_Le_Mrk_Enr,M_eot,idle_Le_Mmc_Enr), (idle_Le_Mrk_Enr,E_eoe,idle_Le_Mrk_Er),
           (waiting_Lie_Mrk_Enr,L_eolu,waiting_Le_Mrk_Enr), (waiting_Lie_Mrk_Enr,M_eot,waiting_Lie_Mmc_Enr), (waiting_Lie_Mrk_Enr,E_eoe,waiting_Lie_Mrk_Er),
           (idle_Lie_Mmc_Enr,start,waiting_Lie_Mmc_Enr), (idle_Lie_Mmc_Enr,L_eolu,idle_Le_Mmc_Enr), (idle_Lie_Mmc_Enr,E_eoe,idle_Lie_Mmc_Er),
           (waiting_Lr_Mmc_Enr,L_liftup,waiting_Lie_Mmc_Enr), (waiting_Lr_Mmc_Enr,E_eoe,waiting_Lr_Mmc_Er), (idle_Lir_Mrk_Er,start,waiting_Lir_Mrk_Er),
           (idle_Lir_Mrk_Er,E_evacuate,idle_Lir_Mrk_Enr), (idle_Lir_Mrk_Er,L_eold,idle_Lr_Mrk_Er), (idle_Lir_Mrk_Er,M_eot,idle_Lir_Mmc_Er),
           (waiting_Le_Mrk_Er,L_liftdown,waiting_Lir_Mrk_Er), (waiting_Le_Mrk_Er,E_evacuate,waiting_Le_Mrk_Enr), (waiting_Le_Mrk_Er,M_eot,waiting_Le_Mmc_Er),
           (idle_Le_Mmc_Er,start,waiting_Le_Mmc_Er), (idle_Le_Mmc_Er,E_evacuate,idle_Le_Mmc_Enr), (idle_Le_Mmc_Er,L_liftdown,idle_Lir_Mmc_Er),
           (waiting_Lie_Mmc_Er,L_eolu,waiting_Le_Mmc_Er), (waiting_Lie_Mmc_Er,E_evacuate,waiting_Lie_Mmc_Enr), (passm1_Lir_Mrk_Er,M_eot,passm2_Lir_Mmc_Er),
           (passm1_Lir_Mrk_Er,E_evacuate,passm1_Lir_Mrk_Enr), (passm1_Lir_Mrk_Er,L_eold,passm1_Lr_Mrk_Er), (waiting_Lir_Mim_Enr,M_pass,passm1_Lir_Mrk_Enr),
           (waiting_Lir_Mim_Enr,M_fail,failm1_Lir_Mrk_Enr), (waiting_Lir_Mim_Enr,L_eold,waiting_Lr_Mim_Enr), (waiting_Lir_Mim_Enr,E_eoe,waiting_Lir_Mim_Er),
           (failm1_Lir_Mrk_Er,M_eot,failm2_Lir_Mmc_Er), (failm1_Lir_Mrk_Er,E_evacuate,failm1_Lir_Mrk_Enr), (failm1_Lir_Mrk_Er,L_eold,failm1_Lr_Mrk_Er),
           (passm1_Le_Mrk_Enr,M_eot,passm2_Le_Mmc_Enr), (passm1_Le_Mrk_Enr,L_liftdown,passm1_Lir_Mrk_Enr), (passm1_Le_Mrk_Enr,E_eoe,passm1_Le_Mrk_Er),
           (failm1_Le_Mrk_Enr,M_eot,failm2_Le_Mmc_Enr), (failm1_Le_Mrk_Enr,L_liftdown,failm1_Lir_Mrk_Enr), (failm1_Le_Mrk_Enr,E_eoe,failm1_Le_Mrk_Er),
           (passm2_Le_Mmc_Er,E_evacuate,passm2_Le_Mmc_Enr), (passm2_Le_Mmc_Er,L_liftdown,passm2_Lir_Mmc_Er), (failm2_Le_Mmc_Er,L_liftdown,failm2_Lir_Mmc_Er),
           (failm2_Le_Mmc_Er,E_evacuate,failm2_Le_Mmc_Enr), (passm2_Lie_Mmc_Enr,E_eoe,passm3_Lie_Mmc_Er), (passm2_Lie_Mmc_Enr,L_eolu,passm2_Le_Mmc_Enr),
           (failm2_Lie_Mmc_Enr,L_eolu,failm2_Le_Mmc_Enr), (failm2_Lie_Mmc_Enr,E_eoe,failm2_Lie_Mmc_Er), (passm3_Lr_Mmc_Er,L_liftup,passm3_Lie_Mmc_Er),
           (passm3_Lr_Mmc_Er,E_evacuate,passm3_Lr_Mmc_Enr), (idle_Lir_Mrk_Enr,start,waiting_Lir_Mrk_Enr), (idle_Lir_Mrk_Enr,L_eold,idle_Lr_Mrk_Enr),
           (idle_Lir_Mrk_Enr,M_eot,idle_Lir_Mmc_Enr), (idle_Lir_Mrk_Enr,E_eoe,idle_Lir_Mrk_Er), (waiting_Le_Mrk_Enr,L_liftdown,waiting_Lir_Mrk_Enr),
           (waiting_Le_Mrk_Enr,M_eot,waiting_Le_Mmc_Enr), (waiting_Le_Mrk_Enr,E_eoe,waiting_Le_Mrk_Er), (idle_Le_Mmc_Enr,start,waiting_Le_Mmc_Enr),
           (idle_Le_Mmc_Enr,L_liftdown,idle_Lir_Mmc_Enr), (idle_Le_Mmc_Enr,E_eoe,idle_Le_Mmc_Er), (waiting_Lie_Mmc_Enr,L_eolu,waiting_Le_Mmc_Enr),
           (waiting_Lie_Mmc_Enr,E_eoe,waiting_Lie_Mmc_Er), (waiting_Lir_Mrk_Er,L_eold,waiting_Lr_Mrk_Er), (waiting_Lir_Mrk_Er,E_evacuate,waiting_Lir_Mrk_Enr),
           (waiting_Lir_Mrk_Er,M_eot,waiting_Lir_Mmc_Er), (idle_Lir_Mmc_Er,start,waiting_Lir_Mmc_Er), (idle_Lir_Mmc_Er,E_evacuate,idle_Lir_Mmc_Enr),
           (idle_Lir_Mmc_Er,L_eold,idle_Lr_Mmc_Er), (waiting_Le_Mmc_Er,L_liftdown,waiting_Lir_Mmc_Er), (waiting_Le_Mmc_Er,E_evacuate,waiting_Le_Mmc_Enr),
           (passm2_Lir_Mmc_Er,E_evacuate,passm2_Lir_Mmc_Enr), (passm2_Lir_Mmc_Er,L_eold,passm2_Lr_Mmc_Er), (passm1_Lir_Mrk_Enr,M_eot,passm2_Lir_Mmc_Enr),
           (passm1_Lir_Mrk_Enr,L_eold,passm1_Lr_Mrk_Enr), (passm1_Lir_Mrk_Enr,E_eoe,passm1_Lir_Mrk_Er), (failm1_Lir_Mrk_Enr,M_eot,failm2_Lir_Mmc_Enr),
           (failm1_Lir_Mrk_Enr,L_eold,failm1_Lr_Mrk_Enr), (failm1_Lir_Mrk_Enr,E_eoe,failm1_Lir_Mrk_Er), (failm2_Lir_Mmc_Er,L_eold,failm3_Lr_Mmc_Er),
           (failm2_Lir_Mmc_Er,E_evacuate,failm2_Lir_Mmc_Enr), (passm2_Le_Mmc_Enr,E_eoe,passm3_Le_Mmc_Er), (passm2_Le_Mmc_Enr,L_liftdown,passm2_Lir_Mmc_Enr),
           (failm2_Le_Mmc_Enr,L_liftdown,failm2_Lir_Mmc_Enr), (failm2_Le_Mmc_Enr,E_eoe,failm2_Le_Mmc_Er), (passm3_Lie_Mmc_Er,L_eolu,passm3_Le_Mmc_Er),
           (passm3_Lie_Mmc_Er,E_evacuate,passm3_Lie_Mmc_Enr), (passm3_Lr_Mmc_Enr,L_liftup,passm3_Lie_Mmc_Enr), (passm3_Lr_Mmc_Enr,E_eoe,passm3_Lr_Mmc_Er),
           (waiting_Lir_Mrk_Enr,L_eold,waiting_Lr_Mrk_Enr), (waiting_Lir_Mrk_Enr,M_eot,waiting_Lir_Mmc_Enr), (waiting_Lir_Mrk_Enr,E_eoe,waiting_Lir_Mrk_Er),
           (idle_Lir_Mmc_Enr,start,waiting_Lir_Mmc_Enr), (idle_Lir_Mmc_Enr,L_eold,idle_Lr_Mmc_Enr), (idle_Lir_Mmc_Enr,E_eoe,idle_Lir_Mmc_Er),
           (waiting_Le_Mmc_Enr,L_liftdown,waiting_Lir_Mmc_Enr), (waiting_Le_Mmc_Enr,E_eoe,waiting_Le_Mmc_Er), (waiting_Lir_Mmc_Er,L_eold,waiting_Lr_Mmc_Er),
           (waiting_Lir_Mmc_Er,E_evacuate,waiting_Lir_Mmc_Enr), (passm2_Lir_Mmc_Enr,E_eoe,passm3_Lir_Mmc_Er), (passm2_Lir_Mmc_Enr,L_eold,passm2_Lr_Mmc_Enr),
           (failm2_Lir_Mmc_Enr,L_eold,failm3_Lr_Mmc_Enr), (failm2_Lir_Mmc_Enr,E_eoe,failm2_Lir_Mmc_Er), (failm3_Lr_Mmc_Er,E_evacuate,failm3_Lr_Mmc_Enr),
           (failm3_Lr_Mmc_Er,L_liftup,failm3_Lie_Mmc_Er), (passm3_Le_Mmc_Er,L_liftdown,passm3_Lir_Mmc_Er), (passm3_Le_Mmc_Er,E_evacuate,passm3_Le_Mmc_Enr),
           (passm3_Lie_Mmc_Enr,L_eolu,passm3_Le_Mmc_Enr), (passm3_Lie_Mmc_Enr,E_eoe,passm3_Lie_Mmc_Er), (waiting_Lir_Mmc_Enr,L_eold,waiting_Lr_Mmc_Enr),
           (waiting_Lir_Mmc_Enr,E_eoe,waiting_Lir_Mmc_Er), (passm3_Lir_Mmc_Er,L_eold,passm4_Lr_Mmc_Er), (passm3_Lir_Mmc_Er,E_evacuate,passm3_Lir_Mmc_Enr),
           (failm3_Lr_Mmc_Enr,E_eoe,failm4_Lr_Mmc_Er), (failm3_Lr_Mmc_Enr,L_liftup,failm3_Lie_Mmc_Enr), (failm3_Lie_Mmc_Er,E_evacuate,failm3_Lie_Mmc_Enr),
           (failm3_Lie_Mmc_Er,L_eolu,failm3_Le_Mmc_Er), (passm3_Le_Mmc_Enr,L_liftdown,passm3_Lir_Mmc_Enr), (passm3_Le_Mmc_Enr,E_eoe,passm3_Le_Mmc_Er),
           (passm4_Lr_Mmc_Er,M_reset,idle_Lr_Midl_Er), (passm4_Lr_Mmc_Er,E_evacuate,passm4_Lr_Mmc_Enr), (passm4_Lr_Mmc_Er,L_liftup,passm4_Lie_Mmc_Er),
           (passm3_Lir_Mmc_Enr,L_eold,passm4_Lr_Mmc_Enr), (passm3_Lir_Mmc_Enr,E_eoe,passm3_Lir_Mmc_Er), (failm4_Lr_Mmc_Er,M_reset,idle_Lr_Midl_Er),
           (failm4_Lr_Mmc_Er,E_evacuate,failm4_Lr_Mmc_Enr), (failm4_Lr_Mmc_Er,L_liftup,failm4_Lie_Mmc_Er), (failm3_Lie_Mmc_Enr,E_eoe,failm4_Lie_Mmc_Er),
           (failm3_Lie_Mmc_Enr,L_eolu,failm3_Le_Mmc_Enr), (failm3_Le_Mmc_Er,E_evacuate,failm3_Le_Mmc_Enr), (failm3_Le_Mmc_Er,L_liftdown,failm3_Lir_Mmc_Er),
           (passm4_Lr_Mmc_Enr,M_reset,idle_Lr_Midl_Enr), (passm4_Lr_Mmc_Enr,L_liftup,passm4_Lie_Mmc_Enr), (passm4_Lr_Mmc_Enr,E_eoe,passm4_Lr_Mmc_Er),
           (passm4_Lie_Mmc_Er,M_reset,idle_Lie_Midl_Er), (passm4_Lie_Mmc_Er,E_evacuate,passm4_Lie_Mmc_Enr), (passm4_Lie_Mmc_Er,L_eolu,passm4_Le_Mmc_Er),
           (failm4_Lr_Mmc_Enr,M_reset,idle_Lr_Midl_Enr), (failm4_Lr_Mmc_Enr,L_liftup,failm4_Lie_Mmc_Enr), (failm4_Lr_Mmc_Enr,E_eoe,failm4_Lr_Mmc_Er),
           (failm4_Lie_Mmc_Er,M_reset,idle_Lie_Midl_Er), (failm4_Lie_Mmc_Er,E_evacuate,failm4_Lie_Mmc_Enr), (failm4_Lie_Mmc_Er,L_eolu,failm4_Le_Mmc_Er),
           (failm3_Le_Mmc_Enr,E_eoe,failm4_Le_Mmc_Er), (failm3_Le_Mmc_Enr,L_liftdown,failm3_Lir_Mmc_Enr), (failm3_Lir_Mmc_Er,E_evacuate,failm3_Lir_Mmc_Enr),
           (failm3_Lir_Mmc_Er,L_eold,failm3_Lr_Mmc_Er), (passm4_Lie_Mmc_Enr,M_reset,idle_Lie_Midl_Enr), (passm4_Lie_Mmc_Enr,L_eolu,passm4_Le_Mmc_Enr),
           (passm4_Lie_Mmc_Enr,E_eoe,passm4_Lie_Mmc_Er), (passm4_Le_Mmc_Er,M_reset,idle_Le_Midl_Er), (passm4_Le_Mmc_Er,E_evacuate,passm4_Le_Mmc_Enr),
           (passm4_Le_Mmc_Er,L_liftdown,passm4_Lir_Mmc_Er), (failm4_Lie_Mmc_Enr,M_reset,idle_Lie_Midl_Enr), (failm4_Lie_Mmc_Enr,L_eolu,failm4_Le_Mmc_Enr),
           (failm4_Lie_Mmc_Enr,E_eoe,failm4_Lie_Mmc_Er), (failm4_Le_Mmc_Er,M_reset,idle_Le_Midl_Er), (failm4_Le_Mmc_Er,E_evacuate,failm4_Le_Mmc_Enr),
           (failm4_Le_Mmc_Er,L_liftdown,failm4_Lir_Mmc_Er), (failm3_Lir_Mmc_Enr,E_eoe,failm4_Lir_Mmc_Er), (failm3_Lir_Mmc_Enr,L_eold,failm3_Lr_Mmc_Enr),
           (passm4_Le_Mmc_Enr,M_reset,idle_Le_Midl_Enr), (passm4_Le_Mmc_Enr,L_liftdown,passm4_Lir_Mmc_Enr), (passm4_Le_Mmc_Enr,E_eoe,passm4_Le_Mmc_Er),
           (passm4_Lir_Mmc_Er,M_reset,idle_Lir_Midl_Er), (passm4_Lir_Mmc_Er,E_evacuate,passm4_Lir_Mmc_Enr), (passm4_Lir_Mmc_Er,L_eold,passm4_Lr_Mmc_Er),
           (failm4_Le_Mmc_Enr,M_reset,idle_Le_Midl_Enr), (failm4_Le_Mmc_Enr,L_liftdown,failm4_Lir_Mmc_Enr), (failm4_Le_Mmc_Enr,E_eoe,failm4_Le_Mmc_Er),
           (failm4_Lir_Mmc_Er,M_reset,idle_Lir_Midl_Er), (failm4_Lir_Mmc_Er,E_evacuate,failm4_Lir_Mmc_Enr), (failm4_Lir_Mmc_Er,L_eold,failm4_Lr_Mmc_Er),
           (passm4_Lir_Mmc_Enr,M_reset,idle_Lir_Midl_Enr), (passm4_Lir_Mmc_Enr,L_eold,passm4_Lr_Mmc_Enr), (passm4_Lir_Mmc_Enr,E_eoe,passm4_Lir_Mmc_Er),
           (failm4_Lir_Mmc_Enr,M_reset,idle_Lir_Midl_Enr), (failm4_Lir_Mmc_Enr,L_eold,failm4_Lr_Mmc_Enr), (failm4_Lir_Mmc_Enr,E_eoe,failm4_Lir_Mmc_Er) },
      x_0: idle_Lr_Midl_Er,
      Xm: {idle_Lr_Midl_Er},
      ctrlEvents: {start, M_reset, L_liftup, L_liftdown, M_test, E_evacuate} )
 &
 seqT = {
      (idle_Lr_Midl_Er,idle),(waiting_Lr_Midl_Er,waiting),(idle_Lr_Midl_Enr,idle),
      (idle_Lr_Mim_Er,idle),(idle_Lie_Midl_Er,idle),(waiting_Lie_Midl_Er,waiting),
      (waiting_Lr_Midl_Enr,waiting),(waiting_Lr_Mim_Er,waiting),(idle_Lr_Mim_Enr,idle),
      (idle_Lie_Midl_Enr,idle),(idle_Lie_Mim_Er,idle),(idle_Lr_Mrk_Er,idle),
      (idle_Le_Midl_Er,idle),(waiting_Le_Midl_Er,waiting),(waiting_Lie_Midl_Enr,waiting),
      (waiting_Lie_Mim_Er,waiting),(waiting_Lr_Mim_Enr,waiting),(passm1_Lr_Mrk_Er,passm1),
      (failm1_Lr_Mrk_Er,failm1),(idle_Lie_Mim_Enr,idle),(idle_Lr_Mrk_Enr,idle),
      (idle_Le_Midl_Enr,idle),(idle_Le_Mim_Er,idle),(idle_Lie_Mrk_Er,idle),
      (waiting_Lr_Mrk_Er,waiting),(idle_Lr_Mmc_Er,idle),(idle_Lir_Midl_Er,idle),
      (waiting_Lir_Midl_Er,waiting),(waiting_Le_Midl_Enr,waiting),(waiting_Le_Mim_Er,waiting),
      (waiting_Lie_Mim_Enr,waiting),(passm1_Lie_Mrk_Er,passm1),(failm1_Lie_Mrk_Er,failm1),
      (passm1_Lr_Mrk_Enr,passm1),(failm1_Lr_Mrk_Enr,failm1),(passm2_Lr_Mmc_Er,passm2),
      (failm2_Lr_Mmc_Er,failm2),(idle_Le_Mim_Enr,idle),(idle_Lie_Mrk_Enr,idle),
      (waiting_Lr_Mrk_Enr,waiting),(idle_Lr_Mmc_Enr,idle),(idle_Lir_Midl_Enr,idle),
      (idle_Lir_Mim_Er,idle),(idle_Le_Mrk_Er,idle),(waiting_Lie_Mrk_Er,waiting),
      (idle_Lie_Mmc_Er,idle),(waiting_Lr_Mmc_Er,waiting),(waiting_Lir_Mim_Er,waiting),
      (waiting_Lir_Midl_Enr,waiting),(waiting_Le_Mim_Enr,waiting),(passm1_Le_Mrk_Er,passm1),
      (failm1_Le_Mrk_Er,failm1),(passm1_Lie_Mrk_Enr,passm1),(failm1_Lie_Mrk_Enr,failm1),
      (passm2_Lie_Mmc_Er,passm2),(failm2_Lie_Mmc_Er,failm2),(passm2_Lr_Mmc_Enr,passm2),
      (failm2_Lr_Mmc_Enr,failm2),(idle_Lir_Mim_Enr,idle),(idle_Le_Mrk_Enr,idle),
      (waiting_Lie_Mrk_Enr,waiting),(idle_Lie_Mmc_Enr,idle),(waiting_Lr_Mmc_Enr,waiting),
      (idle_Lir_Mrk_Er,idle),(waiting_Le_Mrk_Er,waiting),(idle_Le_Mmc_Er,idle),
      (waiting_Lie_Mmc_Er,waiting),(passm1_Lir_Mrk_Er,passm1),(waiting_Lir_Mim_Enr,waiting),
      (failm1_Lir_Mrk_Er,failm1),(passm1_Le_Mrk_Enr,passm1),(failm1_Le_Mrk_Enr,failm1),
      (passm2_Le_Mmc_Er,passm2),(failm2_Le_Mmc_Er,failm2),(passm2_Lie_Mmc_Enr,passm2),
      (failm2_Lie_Mmc_Enr,failm2),(passm3_Lr_Mmc_Er,passm3),(idle_Lir_Mrk_Enr,idle),
      (waiting_Le_Mrk_Enr,waiting),(idle_Le_Mmc_Enr,idle),(waiting_Lie_Mmc_Enr,waiting),
      (waiting_Lir_Mrk_Er,waiting),(idle_Lir_Mmc_Er,idle),(waiting_Le_Mmc_Er,waiting),
      (passm2_Lir_Mmc_Er,passm2),(passm1_Lir_Mrk_Enr,passm1),(failm1_Lir_Mrk_Enr,failm1),
      (failm2_Lir_Mmc_Er,failm2),(passm2_Le_Mmc_Enr,passm2),(failm2_Le_Mmc_Enr,failm2),
      (passm3_Lie_Mmc_Er,passm3),(passm3_Lr_Mmc_Enr,passm3),(waiting_Lir_Mrk_Enr,waiting),
      (idle_Lir_Mmc_Enr,idle),(waiting_Le_Mmc_Enr,waiting),(waiting_Lir_Mmc_Er,waiting),
      (passm2_Lir_Mmc_Enr,passm2),(failm2_Lir_Mmc_Enr,failm2),(failm3_Lr_Mmc_Er,failm3),
      (passm3_Le_Mmc_Er,passm3),(passm3_Lie_Mmc_Enr,passm3),(waiting_Lir_Mmc_Enr,waiting),
      (passm3_Lir_Mmc_Er,passm3),(failm3_Lr_Mmc_Enr,failm3),(failm3_Lie_Mmc_Er,failm3),
      (passm3_Le_Mmc_Enr,passm3),(passm4_Lr_Mmc_Er,passm4),(passm3_Lir_Mmc_Enr,passm3),
      (failm4_Lr_Mmc_Er,failm4),(failm3_Lie_Mmc_Enr,failm3),(failm3_Le_Mmc_Er,failm3),
      (passm4_Lr_Mmc_Enr,passm4),(passm4_Lie_Mmc_Er,passm4),(failm4_Lr_Mmc_Enr,failm4),
      (failm4_Lie_Mmc_Er,failm4),(failm3_Le_Mmc_Enr,failm3),(failm3_Lir_Mmc_Er,failm3),
      (passm4_Lie_Mmc_Enr,passm4),(passm4_Le_Mmc_Er,passm4),(failm4_Lie_Mmc_Enr,failm4),
      (failm4_Le_Mmc_Er,failm4),(failm3_Lir_Mmc_Enr,failm3),(passm4_Le_Mmc_Enr,passm4),
      (passm4_Lir_Mmc_Er,passm4),(failm4_Le_Mmc_Enr,failm4),(failm4_Lir_Mmc_Er,failm4),
      (passm4_Lir_Mmc_Enr,passm4),(failm4_Lir_Mmc_Enr,failm4) }
 &
 lift = {
      (idle_Lr_Midl_Er,Lr),(waiting_Lr_Midl_Er,Lr),(idle_Lr_Midl_Enr,Lr),
      (idle_Lr_Mim_Er,Lr),(idle_Lie_Midl_Er,Lie),(waiting_Lie_Midl_Er,Lie),
      (waiting_Lr_Midl_Enr,Lr),(waiting_Lr_Mim_Er,Lr),(idle_Lr_Mim_Enr,Lr),
      (idle_Lie_Midl_Enr,Lie),(idle_Lie_Mim_Er,Lie),(idle_Lr_Mrk_Er,Lr),
      (idle_Le_Midl_Er,Le),(waiting_Le_Midl_Er,Le),(waiting_Lie_Midl_Enr,Lie),
      (waiting_Lie_Mim_Er,Lie),(waiting_Lr_Mim_Enr,Lr),(passm1_Lr_Mrk_Er,Lr),
      (failm1_Lr_Mrk_Er,Lr),(idle_Lie_Mim_Enr,Lie),(idle_Lr_Mrk_Enr,Lr),
      (idle_Le_Midl_Enr,Le),(idle_Le_Mim_Er,Le),(idle_Lie_Mrk_Er,Lie),
      (waiting_Lr_Mrk_Er,Lr),(idle_Lr_Mmc_Er,Lr),(idle_Lir_Midl_Er,Lir),
      (waiting_Lir_Midl_Er,Lir),(waiting_Le_Midl_Enr,Le),(waiting_Le_Mim_Er,Le),
      (waiting_Lie_Mim_Enr,Lie),(passm1_Lie_Mrk_Er,Lie),(failm1_Lie_Mrk_Er,Lie),
      (passm1_Lr_Mrk_Enr,Lr),(failm1_Lr_Mrk_Enr,Lr),(passm2_Lr_Mmc_Er,Lr),
      (failm2_Lr_Mmc_Er,Lr),(idle_Le_Mim_Enr,Le),(idle_Lie_Mrk_Enr,Lie),
      (waiting_Lr_Mrk_Enr,Lr),(idle_Lr_Mmc_Enr,Lr),(idle_Lir_Midl_Enr,Lir),
      (idle_Lir_Mim_Er,Lir),(idle_Le_Mrk_Er,Le),(waiting_Lie_Mrk_Er,Lie),
      (idle_Lie_Mmc_Er,Lie),(waiting_Lr_Mmc_Er,Lr),(waiting_Lir_Mim_Er,Lir),
      (waiting_Lir_Midl_Enr,Lir),(waiting_Le_Mim_Enr,Le),(passm1_Le_Mrk_Er,Le),
      (failm1_Le_Mrk_Er,Le),(passm1_Lie_Mrk_Enr,Lie),(failm1_Lie_Mrk_Enr,Lie),
      (passm2_Lie_Mmc_Er,Lie),(failm2_Lie_Mmc_Er,Lie),(passm2_Lr_Mmc_Enr,Lr),
      (failm2_Lr_Mmc_Enr,Lr),(idle_Lir_Mim_Enr,Lir),(idle_Le_Mrk_Enr,Le),
      (waiting_Lie_Mrk_Enr,Lie),(idle_Lie_Mmc_Enr,Lie),(waiting_Lr_Mmc_Enr,Lr),
      (idle_Lir_Mrk_Er,Lir),(waiting_Le_Mrk_Er,Le),(idle_Le_Mmc_Er,Le),
      (waiting_Lie_Mmc_Er,Lie),(passm1_Lir_Mrk_Er,Lir),(waiting_Lir_Mim_Enr,Lir),
      (failm1_Lir_Mrk_Er,Lir),(passm1_Le_Mrk_Enr,Le),(failm1_Le_Mrk_Enr,Le),
      (passm2_Le_Mmc_Er,Le),(failm2_Le_Mmc_Er,Le),(passm2_Lie_Mmc_Enr,Lie),
      (failm2_Lie_Mmc_Enr,Lie),(passm3_Lr_Mmc_Er,Lr),(idle_Lir_Mrk_Enr,Lir),
      (waiting_Le_Mrk_Enr,Le),(idle_Le_Mmc_Enr,Le),(waiting_Lie_Mmc_Enr,Lie),
      (waiting_Lir_Mrk_Er,Lir),(idle_Lir_Mmc_Er,Lir),(waiting_Le_Mmc_Er,Le),
      (passm2_Lir_Mmc_Er,Lir),(passm1_Lir_Mrk_Enr,Lir),(failm1_Lir_Mrk_Enr,Lir),
      (failm2_Lir_Mmc_Er,Lir),(passm2_Le_Mmc_Enr,Le),(failm2_Le_Mmc_Enr,Le),
      (passm3_Lie_Mmc_Er,Lie),(passm3_Lr_Mmc_Enr,Lr),(waiting_Lir_Mrk_Enr,Lir),
      (idle_Lir_Mmc_Enr,Lir),(waiting_Le_Mmc_Enr,Le),(waiting_Lir_Mmc_Er,Lir),
      (passm2_Lir_Mmc_Enr,Lir),(failm2_Lir_Mmc_Enr,Lir),(failm3_Lr_Mmc_Er,Lr),
      (passm3_Le_Mmc_Er,Le),(passm3_Lie_Mmc_Enr,Lie),(waiting_Lir_Mmc_Enr,Lir),
      (passm3_Lir_Mmc_Er,Lir),(failm3_Lr_Mmc_Enr,Lr),(failm3_Lie_Mmc_Er,Lie),
      (passm3_Le_Mmc_Enr,Le),(passm4_Lr_Mmc_Er,Lr),(passm3_Lir_Mmc_Enr,Lir),
      (failm4_Lr_Mmc_Er,Lr),(failm3_Lie_Mmc_Enr,Lie),(failm3_Le_Mmc_Er,Le),
      (passm4_Lr_Mmc_Enr,Lr),(passm4_Lie_Mmc_Er,Lie),(failm4_Lr_Mmc_Enr,Lr),
      (failm4_Lie_Mmc_Er,Lie),(failm3_Le_Mmc_Enr,Le),(failm3_Lir_Mmc_Er,Lir),
      (passm4_Lie_Mmc_Enr,Lie),(passm4_Le_Mmc_Er,Le),(failm4_Lie_Mmc_Enr,Lie),
      (failm4_Le_Mmc_Er,Le),(failm3_Lir_Mmc_Enr,Lir),(passm4_Le_Mmc_Enr,Le),
      (passm4_Lir_Mmc_Er,Lir),(failm4_Le_Mmc_Enr,Le),(failm4_Lir_Mmc_Er,Lir),
      (passm4_Lir_Mmc_Enr,Lir),(failm4_Lir_Mmc_Enr,Lir) }
 &
 micrometer = {
      (idle_Lr_Midl_Er,Midl),(waiting_Lr_Midl_Er,Midl),(idle_Lr_Midl_Enr,Midl),
      (idle_Lr_Mim_Er,Mim),(idle_Lie_Midl_Er,Midl),(waiting_Lie_Midl_Er,Midl),
      (waiting_Lr_Midl_Enr,Midl),(waiting_Lr_Mim_Er,Mim),(idle_Lr_Mim_Enr,Mim),
      (idle_Lie_Midl_Enr,Midl),(idle_Lie_Mim_Er,Mim),(idle_Lr_Mrk_Er,Mrk),
      (idle_Le_Midl_Er,Midl),(waiting_Le_Midl_Er,Midl),(waiting_Lie_Midl_Enr,Midl),
      (waiting_Lie_Mim_Er,Mim),(waiting_Lr_Mim_Enr,Mim),(passm1_Lr_Mrk_Er,Mrk),
      (failm1_Lr_Mrk_Er,Mrk),(idle_Lie_Mim_Enr,Mim),(idle_Lr_Mrk_Enr,Mrk),
      (idle_Le_Midl_Enr,Midl),(idle_Le_Mim_Er,Mim),(idle_Lie_Mrk_Er,Mrk),
      (waiting_Lr_Mrk_Er,Mrk),(idle_Lr_Mmc_Er,Mmc),(idle_Lir_Midl_Er,Midl),
      (waiting_Lir_Midl_Er,Midl),(waiting_Le_Midl_Enr,Midl),(waiting_Le_Mim_Er,Mim),
      (waiting_Lie_Mim_Enr,Mim),(passm1_Lie_Mrk_Er,Mrk),(failm1_Lie_Mrk_Er,Mrk),
      (passm1_Lr_Mrk_Enr,Mrk),(failm1_Lr_Mrk_Enr,Mrk),(passm2_Lr_Mmc_Er,Mmc),
      (failm2_Lr_Mmc_Er,Mmc),(idle_Le_Mim_Enr,Mim),(idle_Lie_Mrk_Enr,Mrk),
      (waiting_Lr_Mrk_Enr,Mrk),(idle_Lr_Mmc_Enr,Mmc),(idle_Lir_Midl_Enr,Midl),
      (idle_Lir_Mim_Er,Mim),(idle_Le_Mrk_Er,Mrk),(waiting_Lie_Mrk_Er,Mrk),
      (idle_Lie_Mmc_Er,Mmc),(waiting_Lr_Mmc_Er,Mmc),(waiting_Lir_Mim_Er,Mim),
      (waiting_Lir_Midl_Enr,Midl),(waiting_Le_Mim_Enr,Mim),(passm1_Le_Mrk_Er,Mrk),
      (failm1_Le_Mrk_Er,Mrk),(passm1_Lie_Mrk_Enr,Mrk),(failm1_Lie_Mrk_Enr,Mrk),
      (passm2_Lie_Mmc_Er,Mmc),(failm2_Lie_Mmc_Er,Mmc),(passm2_Lr_Mmc_Enr,Mmc),
      (failm2_Lr_Mmc_Enr,Mmc),(idle_Lir_Mim_Enr,Mim),(idle_Le_Mrk_Enr,Mrk),
      (waiting_Lie_Mrk_Enr,Mrk),(idle_Lie_Mmc_Enr,Mmc),(waiting_Lr_Mmc_Enr,Mmc),
      (idle_Lir_Mrk_Er,Mrk),(waiting_Le_Mrk_Er,Mrk),(idle_Le_Mmc_Er,Mmc),
      (waiting_Lie_Mmc_Er,Mmc),(passm1_Lir_Mrk_Er,Mrk),(waiting_Lir_Mim_Enr,Mim),
      (failm1_Lir_Mrk_Er,Mrk),(passm1_Le_Mrk_Enr,Mrk),(failm1_Le_Mrk_Enr,Mrk),
      (passm2_Le_Mmc_Er,Mmc),(failm2_Le_Mmc_Er,Mmc),(passm2_Lie_Mmc_Enr,Mmc),
      (failm2_Lie_Mmc_Enr,Mmc),(passm3_Lr_Mmc_Er,Mmc),(idle_Lir_Mrk_Enr,Mrk),
      (waiting_Le_Mrk_Enr,Mrk),(idle_Le_Mmc_Enr,Mmc),(waiting_Lie_Mmc_Enr,Mmc),
      (waiting_Lir_Mrk_Er,Mrk),(idle_Lir_Mmc_Er,Mmc),(waiting_Le_Mmc_Er,Mmc),
      (passm2_Lir_Mmc_Er,Mmc),(passm1_Lir_Mrk_Enr,Mrk),(failm1_Lir_Mrk_Enr,Mrk),
      (failm2_Lir_Mmc_Er,Mmc),(passm2_Le_Mmc_Enr,Mmc),(failm2_Le_Mmc_Enr,Mmc),
      (passm3_Lie_Mmc_Er,Mmc),(passm3_Lr_Mmc_Enr,Mmc),(waiting_Lir_Mrk_Enr,Mrk),
      (idle_Lir_Mmc_Enr,Mmc),(waiting_Le_Mmc_Enr,Mmc),(waiting_Lir_Mmc_Er,Mmc),
      (passm2_Lir_Mmc_Enr,Mmc),(failm2_Lir_Mmc_Enr,Mmc),(failm3_Lr_Mmc_Er,Mmc),
      (passm3_Le_Mmc_Er,Mmc),(passm3_Lie_Mmc_Enr,Mmc),(waiting_Lir_Mmc_Enr,Mmc),
      (passm3_Lir_Mmc_Er,Mmc),(failm3_Lr_Mmc_Enr,Mmc),(failm3_Lie_Mmc_Er,Mmc),
      (passm3_Le_Mmc_Enr,Mmc),(passm4_Lr_Mmc_Er,Mmc),(passm3_Lir_Mmc_Enr,Mmc),
      (failm4_Lr_Mmc_Er,Mmc),(failm3_Lie_Mmc_Enr,Mmc),(failm3_Le_Mmc_Er,Mmc),
      (passm4_Lr_Mmc_Enr,Mmc),(passm4_Lie_Mmc_Er,Mmc),(failm4_Lr_Mmc_Enr,Mmc),
      (failm4_Lie_Mmc_Er,Mmc),(failm3_Le_Mmc_Enr,Mmc),(failm3_Lir_Mmc_Er,Mmc),
      (passm4_Lie_Mmc_Enr,Mmc),(passm4_Le_Mmc_Er,Mmc),(failm4_Lie_Mmc_Enr,Mmc),
      (failm4_Le_Mmc_Er,Mmc),(failm3_Lir_Mmc_Enr,Mmc),(passm4_Le_Mmc_Enr,Mmc),
      (passm4_Lir_Mmc_Er,Mmc),(failm4_Le_Mmc_Enr,Mmc),(failm4_Lir_Mmc_Er,Mmc),
      (passm4_Lir_Mmc_Enr,Mmc),(failm4_Lir_Mmc_Enr,Mmc) }
 &
 evacuator = {
      (idle_Lr_Midl_Er,Er),(waiting_Lr_Midl_Er,Er),(idle_Lr_Midl_Enr,Enr),
      (idle_Lr_Mim_Er,Er),(idle_Lie_Midl_Er,Er),(waiting_Lie_Midl_Er,Er),
      (waiting_Lr_Midl_Enr,Enr),(waiting_Lr_Mim_Er,Er),(idle_Lr_Mim_Enr,Enr),
      (idle_Lie_Midl_Enr,Enr),(idle_Lie_Mim_Er,Er),(idle_Lr_Mrk_Er,Er),
      (idle_Le_Midl_Er,Er),(waiting_Le_Midl_Er,Er),(waiting_Lie_Midl_Enr,Enr),
      (waiting_Lie_Mim_Er,Er),(waiting_Lr_Mim_Enr,Enr),(passm1_Lr_Mrk_Er,Er),
      (failm1_Lr_Mrk_Er,Er),(idle_Lie_Mim_Enr,Enr),(idle_Lr_Mrk_Enr,Enr),
      (idle_Le_Midl_Enr,Enr),(idle_Le_Mim_Er,Er),(idle_Lie_Mrk_Er,Er),
      (waiting_Lr_Mrk_Er,Er),(idle_Lr_Mmc_Er,Er),(idle_Lir_Midl_Er,Er),
      (waiting_Lir_Midl_Er,Er),(waiting_Le_Midl_Enr,Enr),(waiting_Le_Mim_Er,Er),
      (waiting_Lie_Mim_Enr,Enr),(passm1_Lie_Mrk_Er,Er),(failm1_Lie_Mrk_Er,Er),
      (passm1_Lr_Mrk_Enr,Enr),(failm1_Lr_Mrk_Enr,Enr),(passm2_Lr_Mmc_Er,Er),
      (failm2_Lr_Mmc_Er,Er),(idle_Le_Mim_Enr,Enr),(idle_Lie_Mrk_Enr,Enr),
      (waiting_Lr_Mrk_Enr,Enr),(idle_Lr_Mmc_Enr,Enr),(idle_Lir_Midl_Enr,Enr),
      (idle_Lir_Mim_Er,Er),(idle_Le_Mrk_Er,Er),(waiting_Lie_Mrk_Er,Er),
      (idle_Lie_Mmc_Er,Er),(waiting_Lr_Mmc_Er,Er),(waiting_Lir_Mim_Er,Er),
      (waiting_Lir_Midl_Enr,Enr),(waiting_Le_Mim_Enr,Enr),(passm1_Le_Mrk_Er,Er),
      (failm1_Le_Mrk_Er,Er),(passm1_Lie_Mrk_Enr,Enr),(failm1_Lie_Mrk_Enr,Enr),
      (passm2_Lie_Mmc_Er,Er),(failm2_Lie_Mmc_Er,Er),(passm2_Lr_Mmc_Enr,Enr),
      (failm2_Lr_Mmc_Enr,Enr),(idle_Lir_Mim_Enr,Enr),(idle_Le_Mrk_Enr,Enr),
      (waiting_Lie_Mrk_Enr,Enr),(idle_Lie_Mmc_Enr,Enr),(waiting_Lr_Mmc_Enr,Enr),
      (idle_Lir_Mrk_Er,Er),(waiting_Le_Mrk_Er,Er),(idle_Le_Mmc_Er,Er),
      (waiting_Lie_Mmc_Er,Er),(passm1_Lir_Mrk_Er,Er),(waiting_Lir_Mim_Enr,Enr),
      (failm1_Lir_Mrk_Er,Er),(passm1_Le_Mrk_Enr,Enr),(failm1_Le_Mrk_Enr,Enr),
      (passm2_Le_Mmc_Er,Er),(failm2_Le_Mmc_Er,Er),(passm2_Lie_Mmc_Enr,Enr),
      (failm2_Lie_Mmc_Enr,Enr),(passm3_Lr_Mmc_Er,Er),(idle_Lir_Mrk_Enr,Enr),
      (waiting_Le_Mrk_Enr,Enr),(idle_Le_Mmc_Enr,Enr),(waiting_Lie_Mmc_Enr,Enr),
      (waiting_Lir_Mrk_Er,Er),(idle_Lir_Mmc_Er,Er),(waiting_Le_Mmc_Er,Er),
      (passm2_Lir_Mmc_Er,Er),(passm1_Lir_Mrk_Enr,Enr),(failm1_Lir_Mrk_Enr,Enr),
      (failm2_Lir_Mmc_Er,Er),(passm2_Le_Mmc_Enr,Enr),(failm2_Le_Mmc_Enr,Enr),
      (passm3_Lie_Mmc_Er,Er),(passm3_Lr_Mmc_Enr,Enr),(waiting_Lir_Mrk_Enr,Enr),
      (idle_Lir_Mmc_Enr,Enr),(waiting_Le_Mmc_Enr,Enr),(waiting_Lir_Mmc_Er,Er),
      (passm2_Lir_Mmc_Enr,Enr),(failm2_Lir_Mmc_Enr,Enr),(failm3_Lr_Mmc_Er,Er),
      (passm3_Le_Mmc_Er,Er),(passm3_Lie_Mmc_Enr,Enr),(waiting_Lir_Mmc_Enr,Enr),
      (passm3_Lir_Mmc_Er,Er),(failm3_Lr_Mmc_Enr,Enr),(failm3_Lie_Mmc_Er,Er),
      (passm3_Le_Mmc_Enr,Enr),(passm4_Lr_Mmc_Er,Er),(passm3_Lir_Mmc_Enr,Enr),
      (failm4_Lr_Mmc_Er,Er),(failm3_Lie_Mmc_Enr,Enr),(failm3_Le_Mmc_Er,Er),
      (passm4_Lr_Mmc_Enr,Enr),(passm4_Lie_Mmc_Er,Er),(failm4_Lr_Mmc_Enr,Enr),
      (failm4_Lie_Mmc_Er,Er),(failm3_Le_Mmc_Enr,Enr),(failm3_Lir_Mmc_Er,Er),
      (passm4_Lie_Mmc_Enr,Enr),(passm4_Le_Mmc_Er,Er),(failm4_Lie_Mmc_Enr,Enr),
      (failm4_Le_Mmc_Er,Er),(failm3_Lir_Mmc_Enr,Enr),(passm4_Le_Mmc_Enr,Enr),
      (passm4_Lir_Mmc_Er,Er),(failm4_Le_Mmc_Enr,Enr),(failm4_Lir_Mmc_Er,Er),
      (passm4_Lir_Mmc_Enr,Enr),(failm4_Lir_Mmc_Enr,Enr) }
 &

/*
  Implicit constraints:
     1. The lift commands and micrometer responses must be strictly alternated.
     2. The micrometer cannot attempt to start when the lift is at the bottom position.
     3. The lift cannot attempt to lift up when a test has been already performed.
     4. The lift cannot attempt to lift down before a test is performed.
     5. The lift and evacuator commands must be strictly alternated.
     6. The micrometer responses and the evacuator command must be strictly alternated.
     7. The evacuator cannot attempt to evacuate a piece before the result of a test.
  Standard constraints:
     8. When the testing station is idle or in the last step of the pass or fail mode,
        the lift commands, micrometer responses, or evacuator commands must not occurred.
     9. When the testing station is waiting a result, the lift cannot lift down,
        the micrometer cannot move, and evacuator cannot evacuate a piece.
    10. In the first step of the fail and pass modes, the lift, micrometer, or
        evacuator commands (except M_reset) must not occurred.
    11. In the the second step of the fail mode and third step of pass mode, the lift,
        micrometer, and evacuator commands (except L_liftdown, M_reset) must not occurred.
    12. In the the third step of the fail mode and second step of pass mode, the lift,
        or micrometer commands (except M_reset) must not occurred.
  Explicit constraints:
    13. The event "start" can only occur if the value of the state variable is <idle,_,_,_>.
*/
 BadStates =
// The lift is in extension or retraction and the micrometer is in a measurement
// sequence or its results is known:
  { x | x : States & lift(x) : {Lie,Lir} & micrometer(x) : {Mim,Mrk}} \/
// The lift is completely retracted and the micrometer is in a measurement sequence
// or its results is known:
  { x | x : States & lift(x) = Lr & micrometer(x) : {Mim,Mrk}} \/
// The lift is in extension and the measurement sequence of the micrometer is completed:
  { x | x : States & lift(x) = Lie & micrometer(x) = Mmc} \/
// The lift is in retraction and the micrometer is idle:
  { x | x : States & lift(x) = Lir & micrometer(x) = Midl} \/
// The lift is in extension or retraction and the evacuator in not completely retracted
  { x | x : States & lift(x) : {Lie,Lir} & evacuator(x) = Enr} \/
// The micrometer is in a measurement sequence or its results is known and the
// the evacuator in not completely retracted:
  { x | x : States & micrometer(x) : {Mim,Mrk} & evacuator(x) = Enr} \/
// The micrometer is idle and the evacuator in not completely retracted:
  { x | x : States & micrometer(x) = Midl & evacuator(x) = Enr} \/

/* fail mode state sequence: failm1 (the result of the measure was fail),
                             failm2 (the measurement sequence is completed),
                             failm3 (the lift is down), and
                             failm4 (the piece has been evacuated).

   pass mode state sequence: passm1 (the result of the measure was pass),
                             passm2 (the measurement sequence is completed),
                             passm3 (the piece has been evacuated), and
                             passm4 (the lift is down).
*/

// In states idle, failm4 and passm4 of the testing station, the lift is in extension
// or retraction, the the micrometer is in the measurement sequence or its results is
// known, or the evacuator is not completely retracted:
  { x | x : States & seqT(x) : {idle,failm4,passm4} & (lift(x) : {Lie,Lir} or
        micrometer(x) : {Mim,Mrk} or evacuator(x) = Enr )} \/
// In the state waiting of the testing station, the lift is in retraction, the result
// of the micrometer is known, or the evacuator is not completely retracted:
  { x | x : States & seqT(x) = waiting & (lift(x) = Lir or 
        micrometer(x) = Mrk or evacuator(x) = Enr )} \/
// In states failm1 and passm1 of the testing station, the lift is in extension or
// retraction, the micrometer is in a measurement sequence, or the evacuator is
// not completely retracted:
  { x | x : States & seqT(x) : {failm1,passm1} & (lift(x) : {Lie,Lir} or 
        micrometer(x) = Mim or evacuator(x) = Enr )} \/
// In states failm2 and passm3 of the testing station, the lift is in extension, the
// micrometer is in a measurement sequence or its results is known, or the
// evacuator is not completely retracted:
  { x | x : States & seqT(x) : {failm2,passm3} & (lift(x) = Lie or
        micrometer(x) : {Mim,Mrk} or evacuator(x) = Enr )} \/
// In states failm3 and passm2 of the testing station, the lift is in extension or
// in retraction or the micrometer is in a measurement sequence or its results is
// known:
  { x | x : States & seqT(x) : {failm3,passm2} & (lift(x) : {Lie,Lir} or
        micrometer(x) : {Mim,Mrk} )}
 &
 Qpred = States - BadStates
END

/*
OUTPUT:
The transition structure of the agent
 (i.e.,the solution of the control problem)
Number of states of the exhaustive model: 128
Number of transitions of the exhaustive model: 384
Cardinality of the predicate: 30
Supremal coreachable and controllable predicates (17)
{idle_Lr_Midl_Er,waiting_Lr_Midl_Er,waiting_Lie_Midl_Er,waiting_Le_Midl_Er,
 waiting_Le_Mim_Er,passm1_Le_Mrk_Er,failm1_Le_Mrk_Er,passm2_Le_Mmc_Er,
 failm2_Le_Mmc_Er,failm2_Lir_Mmc_Er,passm2_Le_Mmc_Enr,failm3_Lr_Mmc_Er,
 passm3_Le_Mmc_Er,passm3_Lir_Mmc_Er,failm3_Lr_Mmc_Enr,passm4_Lr_Mmc_Er,
 failm4_Lr_Mmc_Er}
Disabled events:
{(idle_Lr_Midl_Er|->{L_liftup,M_test,E_evacuate}),
 (waiting_Lr_Midl_Er|->{M_test,E_evacuate}),
 (waiting_Lie_Midl_Er|->{M_test,E_evacuate}),
 (waiting_Le_Midl_Er|->{L_liftdown,E_evacuate}),
 (waiting_Le_Mim_Er|->{L_liftdown,E_evacuate}),
 (passm1_Le_Mrk_Er|->{L_liftdown,E_evacuate}),
 (failm1_Le_Mrk_Er|->{L_liftdown,E_evacuate}),
 (passm2_Le_Mmc_Er|->{L_liftdown}),(failm2_Le_Mmc_Er|->{E_evacuate}),
 (failm2_Lir_Mmc_Er|->{E_evacuate}),(passm2_Le_Mmc_Enr|->{L_liftdown}),
 (failm3_Lr_Mmc_Er|->{L_liftup}),(passm3_Le_Mmc_Er|->{E_evacuate}),
 (passm3_Lir_Mmc_Er|->{E_evacuate}),(failm3_Lr_Mmc_Enr|->{L_liftup}),
 (passm4_Lr_Mmc_Er|->{L_liftup,E_evacuate}),
 (failm4_Lr_Mmc_Er|->{L_liftup,E_evacuate})}
Enabled events:
{(idle_Lr_Midl_Er|->{start}),(waiting_Lr_Midl_Er|->{L_liftup}),
 (waiting_Lie_Midl_Er|->{L_eolu}),(waiting_Le_Midl_Er|->{M_test}),
 (waiting_Le_Mim_Er|->{M_fail,M_pass}),(passm1_Le_Mrk_Er|->{M_eot}),
 (failm1_Le_Mrk_Er|->{M_eot}),(passm2_Le_Mmc_Er|->{E_evacuate}),
 (failm2_Le_Mmc_Er|->{L_liftdown}),(failm2_Lir_Mmc_Er|->{L_eold}),
 (passm2_Le_Mmc_Enr|->{E_eoe}),(failm3_Lr_Mmc_Er|->{E_evacuate}),
 (passm3_Le_Mmc_Er|->{L_liftdown}),(passm3_Lir_Mmc_Er|->{L_eold}),
 (failm3_Lr_Mmc_Enr|->{E_eoe}),(passm4_Lr_Mmc_Er|->{M_reset}),
 (failm4_Lr_Mmc_Er|->{M_reset})}
Closed-loop system: 
 rec(Xm:{idle_Lr_Midl_Er},
     ctrlEvents:{start,M_reset,L_liftdown,L_liftup,M_test,E_evacuate},
     events:{start,L_eolu,L_eold,M_eot,M_fail,M_reset,M_pass,E_eoe,L_liftdown,
             L_liftup,M_test,E_evacuate},
     states:{idle_Lr_Midl_Er,waiting_Lr_Midl_Er,waiting_Lie_Midl_Er,waiting_Le_Midl_Er,
             waiting_Le_Mim_Er,passm1_Le_Mrk_Er,failm1_Le_Mrk_Er,passm2_Le_Mmc_Er,
             failm2_Le_Mmc_Er,failm2_Lir_Mmc_Er,passm2_Le_Mmc_Enr,failm3_Lr_Mmc_Er,
             passm3_Le_Mmc_Er,passm3_Lir_Mmc_Er,failm3_Lr_Mmc_Enr,passm4_Lr_Mmc_Er,
             failm4_Lr_Mmc_Er},
     tf:{((idle_Lr_Midl_Er|->start)|->waiting_Lr_Midl_Er),
         ((waiting_Lr_Midl_Er|->L_liftup)|->waiting_Lie_Midl_Er),
         ((waiting_Lie_Midl_Er|->L_eolu)|->waiting_Le_Midl_Er),
         ((waiting_Le_Midl_Er|->M_test)|->waiting_Le_Mim_Er),
         ((waiting_Le_Mim_Er|->M_fail)|->failm1_Le_Mrk_Er),
         ((waiting_Le_Mim_Er|->M_pass)|->passm1_Le_Mrk_Er),
         ((passm1_Le_Mrk_Er|->M_eot)|->passm2_Le_Mmc_Er),
         ((failm1_Le_Mrk_Er|->M_eot)|->failm2_Le_Mmc_Er),
         ((passm2_Le_Mmc_Er|->E_evacuate)|->passm2_Le_Mmc_Enr),
         ((failm2_Le_Mmc_Er|->L_liftdown)|->failm2_Lir_Mmc_Er),
         ((failm2_Lir_Mmc_Er|->L_eold)|->failm3_Lr_Mmc_Er),
         ((passm2_Le_Mmc_Enr|->E_eoe)|->passm3_Le_Mmc_Er),
         ((failm3_Lr_Mmc_Er|->E_evacuate)|->failm3_Lr_Mmc_Enr),
         ((passm3_Le_Mmc_Er|->L_liftdown)|->passm3_Lir_Mmc_Er),
         ((passm3_Lir_Mmc_Er|->L_eold)|->passm4_Lr_Mmc_Er),
         ((failm3_Lr_Mmc_Enr|->E_eoe)|->failm4_Lr_Mmc_Er),
         ((passm4_Lr_Mmc_Er|->M_reset)|->idle_Lr_Midl_Er),
         ((failm4_Lr_Mmc_Er|->M_reset)|->idle_Lr_Midl_Er)},
     x_0:idle_Lr_Midl_Er)
*/

/*
  ControlLaw(start) = idle
  ControlLaw(L_liftdown) = (failm2 | passm3) & Le
  ControlLaw(L_liftup) = waiting & Lr
  ControlLaw(M_reset) = (failm4 | passm4) & Mmc
  ControlLaw(M_test) = Le & Midl
  ControlLaw(E_evacuate) = (failm3 | passm2) & Er
*/

/*
% Runtime for SOLUTION for SETUP_CONSTANTS: 130 ms (walltime: 130 ms)
% Finished searching for valid initial states 150 ms walltime (140 ms runtime), since start: 1960 ms
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
finished_random_animate(5,[runtime/140,total_runtime/140,walltime/140])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 370 ms walltime (330 ms runtime), since start: 2110 ms

% Finished process_file (errors=0, warnings=0)
*/