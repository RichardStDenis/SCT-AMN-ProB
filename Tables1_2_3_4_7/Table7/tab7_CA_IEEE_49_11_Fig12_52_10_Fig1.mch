// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL tab7_CA_IEEE_49_11_Fig12_52_10_Fig1

SEES
 //---------------------------------------------------------------------
 // Machine for input data
 //---------------------------------------------------------------------
 // conditionally C&P coobservable and conditionally D&A coobservable
 // w.r.t. the general architecture thus conditionally coobservable
 // w.r.t. the general architecture (Sigma_ce = {c} and Sigma_cd = {d}) and
 // Sigma_c_1 = {c, d}, Sigma_c_2 = {c, d} and
 // Sigma_o_1 = {a, ap, c, d}, Sigma_o_2 = {b, bp, c, d}.
 //IEEE_49_11_H_Fig12 /*@file "../../DataExamples/IEEE_49_11_H_Fig12.mch" */
 IEEE_52_10_Ex1_H_Fig1 /*@file "../../DataExamples/IEEE_52_10_Ex1_H_Fig1.mch" */

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 1000000;
 SET_PREF_SMT == FALSE;
 SET_PREF_KODKOD == FALSE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../../Theories/SCT_core.def";
 "../../Theories/SCT_decentralized.def";
 "LibraryIO.def"

CONSTANTS
 L_G, L_H               // Languages for free and legal behaviors

PROPERTIES
 G_fb : Generator_C_O(G_fb'states,G_fb'events) &
 H_lb : FSA(H_lb'states,H_lb'events) &
 L_G = GetStrings(G_fb,bound) &
 L_H = GetStrings(H_lb,bound)

VARIABLES
 S_d_i, S_dc_i, S_e_i, S_ec_i,    // Local decisions
 S_d, S_e                         // Disabling and enabling global decisions

INITIALISATION
 S_d_i := {};  S_dc_i := {};  S_e_i := {};  S_ec_i := {};
 S_d := {};  S_e := {}

INVARIANT
 S_d_i <: def_Sl_d_dc_e_ec(L_G,seq(G_fb'events),G_fb'events) &
 S_dc_i <: def_Sl_d_dc_e_ec(L_G,seq(G_fb'events),G_fb'events) &
 S_e_i <: def_Sl_d_dc_e_ec(L_G,seq(G_fb'events),G_fb'events) &
 S_ec_i <: def_Sl_d_dc_e_ec(L_G,seq(G_fb'events),G_fb'events) &
 S_d <: def_Sg_e_d(L_G,G_fb'events) &
 S_e <: def_Sg_e_d(L_G,G_fb'events)

OPERATIONS
 VerifyGCCoobservability =
  SELECT GCCoobservability(A_d,A_d'Events_ce,A_d'Events_cd,L_G,L_H) THEN
   PRINT("K is conditionally coobservable.");
   PRINTF("E_ce: ~w~n",[A_d'Events_ce]);
   PRINTF("E_cd: ~w~n",[A_d'Events_cd]);
   S_e_i := LocalDecisionsEnbl(A_d,L_G,L_H);
   PRINTF("Local decisions (enabled unconditionally): ~n ~w~n",[S_e_i]);
   S_ec_i := LocalDecisionsEnbl_c(A_d,L_G,L_H);
   PRINTF("Local decisions (enabled conditionally): ~n ~w~n",[S_ec_i]);
   S_d_i := LocalDecisionsDsbl(A_d,L_G,L_H);
   PRINTF("Local decisions (disabled unconditionally): ~n ~w~n",[S_d_i]);
   S_dc_i := LocalDecisionsDsbl_c(A_d,L_G,L_H);
   PRINTF("Local decisions (disabled conditionally): ~n ~w~n",[S_dc_i]);
   S_e := GlobalDecisionDA(A_d,S_e_i,S_d_i,S_ec_i);
   PRINTF("Global decisions (enabled): ~n ~w~n",[S_e]);
   S_d := GlobalDecisionCP(A_d,S_e_i,S_d_i,S_dc_i);
   PRINTF("Global decisions (disabled): ~n ~w~n",[S_d]);
   IF CtrlNonconflicting(S_e,S_d) THEN
      PRINT("The control is nonconflicting.") ELSE
      PRINT("The control is conflicting.") END ELSE
    PRINT("K is not conditionally coobservable.");
    PRINTF("E_ce: ~w~n",[A_d'Events_ce]);
    PRINTF("E_cd: ~w~n",[A_d'Events_cd]) END END

/*
OUTPUT (IEEE_49_11_H_Fig12.mch):
% Runtime for SOLUTION for SETUP_CONSTANTS: 150 ms (walltime: 160 ms)
% Finished searching for valid initial states 180 ms walltime (180 ms runtime), since start: 1360 ms
% Starting random_animate (max_steps=3)
perform_random_step(1,1)
random(2,1)
perform_random_step(1,2)
random(2,1)
"K is conditionally coobservable."
E_ce: {c}
E_cd: {d}
Local decisions (enabled unconditionally): 
 {((({(1|->a),(2|->bp)}|->2)|->{(1|->bp)})|->c),
  ((({(1|->b),(2|->ap)}|->1)|->{(1|->ap)})|->c),
  ((({(1|->c)}|->1)|->{(1|->c)})|->d),
  ((({(1|->c)}|->2)|->{(1|->c)})|->d)}
Local decisions (enabled conditionally): 
 {((({(1|->a),(2|->bp)}|->2)|->{(1|->bp)})|->c), ((({(1|->b),(2|->ap)}|->1)|->{(1|->ap)})|->c),
  ((({(1|->c)}|->1)|->{(1|->c)})|->d), ((({(1|->c)}|->2)|->{(1|->c)})|->d),
  ((([c,d,a]|->1)|->[c,d,a])|->d), ((([c,d,b]|->2)|->[c,d,b])|->d),
  ((([c,d,a,bp]|->1)|->[c,d,a])|->d),((([c,d,a,bp]|->2)|->[c,d,bp])|->d),
  ((([c,d,b,ap]|->1)|->[c,d,ap])|->d),((([c,d,b,ap]|->2)|->[c,d,b])|->d)}
Local decisions (disabled unconditionally): 
 {((([c,d,a,bp]|->2)|->[c,d,bp])|->d),((([c,d,b,ap]|->1)|->[c,d,ap])|->d)}
Local decisions (disabled conditionally): 
 {((({(1|->a)}|->1)|->{(1|->a)})|->c), ((({(1|->a),(2|->bp)}|->1)|->{(1|->a)})|->c),
  ((({(1|->a),(2|->bp)}|->2)|->{(1|->bp)})|->c),((({(1|->b)}|->2)|->{(1|->b)})|->c),
  ((({(1|->b),(2|->ap)}|->1)|->{(1|->ap)})|->c),((({(1|->b),(2|->ap)}|->2)|->{(1|->b)})|->c),
  ((({(1|->c)}|->1)|->{(1|->c)})|->d),((({(1|->c)}|->2)|->{(1|->c)})|->d),
  ((([c,d,a,bp]|->2)|->[c,d,bp])|->d),((([c,d,b,ap]|->1)|->[c,d,ap])|->d)}
Global decisions (enabled): 
 {({(1|->a),(2|->bp)}|->c),({(1|->b),(2|->ap)}|->c),
  ({(1|->c)}|->d),([c,d,a]|->d),([c,d,b]|->d)}
Global decisions (disabled): 
 {({(1|->a)}|->c),({(1|->b)}|->c),([c,d,a,bp]|->d),([c,d,b,ap]|->d)}

ALL OPERATIONS COVERED

perform_random_step(1,3)
random(2,1)
finished_random_animate(3,[runtime/3870,total_runtime/3870,walltime/4260])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 4500 ms walltime (4110 ms runtime), since start: 5620 ms

% Finished process_file (errors=0, warnings=0)
% ProB Finished

Remark: c is enabled by default from the string [], since there is no decision by any local
        supervisor.
        d is disabled by default from the string [c,d], since there is no decision by any local
        supervisor.


-------------

On MacBook Pro with last version of ProB 20.2.2024:
% Finished processing file after 2204 ms walltime (2189 ms runtime), since start: 4290 ms

----------------------------------------

OUTPUT (IEEE_52_10_Ex1_H_Fig1.mch):
% Runtime for SOLUTION for SETUP_CONSTANTS: 150 ms (walltime: 160 ms)
% Finished searching for valid initial states 180 ms walltime (180 ms runtime), since start: 2130 ms
% Starting random_animate (max_steps=3)
perform_random_step(1,1)
random(2,1)
perform_random_step(1,2)
random(2,1)
"K is not conditionally coobservable."
E_ce: {c}
E_cd: {}
or
E_ce: {}
E_cd: {c}
ALL OPERATIONS COVERED

perform_random_step(1,3)
random(2,1)
finished_random_animate(3,[runtime/1980,total_runtime/1980,walltime/2240])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 2490 ms walltime (2210 ms runtime), since start: 4390 ms

% Finished process_file (errors=0, warnings=0)
% ProB Finished

-------------

On MacBook Pro with last version of ProB 20.2.2024:
% Finished processing file after 367 ms walltime (361 ms runtime), since start: 2464 ms
*/