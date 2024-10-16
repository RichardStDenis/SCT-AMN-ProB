// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL tab7_CA_IEEE_49_11_H_Fig17

SEES
 //---------------------------------------------------------------------
 // Machine for input data
 //---------------------------------------------------------------------
 // conditionally C&P coobservable and conditionally D&A coobservable
 // w.r.t. the general architecture thus conditionally coobservable
 // w.r.t. the general architecture (Sigma_ce = {p} and Sigma_cd = {s}).
 IEEE_49_11_H_Fig17 /*@file "../../DataExamples/IEEE_49_11_H_Fig17.mch" */

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 1000000;
 SET_PREF_SMT == TRUE;   // to avoid a warning
 SET_PREF_KODKOD == FALSE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../../Theories/SCT_core.def";
 "../../Theories/SCT_decentralized.def";
 "LibraryIO.def"

CONSTANTS
 T_G, L_G, T_H, L_H     // Languages for free and legal behaviors

PROPERTIES
 G_fb : Generator_C_O(G_fb'states,G_fb'events) &
 H_lb : FSA(H_lb'states,H_lb'events) &
 T_G = GetTracesNc1stNqcSev(G_fb,bound,{p,s}) &
 L_G = Traces2Strings(T_G) &
 T_H = GetTracesNc1stNqcSev(H_lb,bound,{p,s}) &
 L_H = Traces2Strings(T_H)

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
  PRE GCCoobservability(A_d,A_d'Events_ce,A_d'Events_cd,L_G,L_H) THEN
   PRINT("K is conditionally coobservable.");
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
      PRINT("The control is conflicting.") END END END

/*
OUTPUT (IEEE_49_11_H_Fig17):
% Runtime for SOLUTION for SETUP_CONSTANTS: 190 ms (walltime: 190 ms)
% Finished searching for valid initial states 220 ms walltime (210 ms runtime), since start: 1410 ms
% Starting random_animate (max_steps=3)
perform_random_step(1,1)
random(2,1)
perform_random_step(1,2)
random(2,1)
"K is conditionally coobservable."
Local decisions (enabled unconditionally): 
 {((({(1|->i1),(2|->o2)}|->2)|->{(1|->o2)})|->p),((({(1|->i2),(2|->o1)}|->1)|->{(1|->o1)})|->p),
  ((([p,i1,o2]|->2)|->{(1|->p),(2|->o2)})|->p),((([p,i1,s,o2]|->2)|->[p,s,o2])|->p),
  ((([p,i2,o1]|->1)|->{(1|->p),(2|->o1)})|->p),((([p,i2,s,o1]|->1)|->[p,s,o1])|->p)}
Local decisions (enabled conditionally): 
 {((({(1|->i1)}|->1)|->{(1|->i1)})|->s),((({(1|->i1),(2|->o2)}|->1)|->{(1|->i1)})|->s),
  ((({(1|->i1),(2|->o2)}|->2)|->{(1|->o2)})|->s),((({(1|->i1),(2|->o2)}|->2)|->{(1|->o2)})|->p),
  ((({(1|->i1),(2|->s)}|->1)|->{(1|->i1),(2|->s)})|->s),((({(1|->i1),(2|->s)}|->1)|->{(1|->i1),(2|->s)})|->p),
  ((({(1|->i1),(2|->s)}|->2)|->{(1|->s)})|->s),((({(1|->i1),(2|->s)}|->2)|->{(1|->s)})|->p),
  ((({(1|->i2)}|->2)|->{(1|->i2)})|->s),((({(1|->i2),(2|->o1)}|->1)|->{(1|->o1)})|->s),
  ((({(1|->i2),(2|->o1)}|->1)|->{(1|->o1)})|->p),((({(1|->i2),(2|->o1)}|->2)|->{(1|->i2)})|->s),
  ((({(1|->i2),(2|->s)}|->1)|->{(1|->s)})|->s),((({(1|->i2),(2|->s)}|->1)|->{(1|->s)})|->p),
  ((({(1|->i2),(2|->s)}|->2)|->{(1|->i2),(2|->s)})|->s),((({(1|->i2),(2|->s)}|->2)|->{(1|->i2),(2|->s)})|->p),
  ((({(1|->p)}|->1)|->{(1|->p)})|->p),((({(1|->p)}|->2)|->{(1|->p)})|->p),
  ((({(1|->p),(2|->i1)}|->1)|->{(1|->p),(2|->i1)})|->s),((({(1|->p),(2|->i1)}|->1)|->{(1|->p),(2|->i1)})|->p),
  ((({(1|->p),(2|->i1)}|->2)|->{(1|->p)})|->p),((({(1|->p),(2|->i2)}|->1)|->{(1|->p)})|->p),
  ((({(1|->p),(2|->i2)}|->2)|->{(1|->p),(2|->i2)})|->s),((({(1|->p),(2|->i2)}|->2)|->{(1|->p),(2|->i2)})|->p),
  ((([p,i1,o2]|->1)|->{(1|->p),(2|->i1)})|->s),((([p,i1,o2]|->1)|->{(1|->p),(2|->i1)})|->p),
  ((([p,i1,o2]|->2)|->{(1|->p),(2|->o2)})|->s),((([p,i1,o2]|->2)|->{(1|->p),(2|->o2)})|->p),
  ((([p,i1,s]|->1)|->[p,i1,s])|->s),((([p,i1,s]|->1)|->[p,i1,s])|->p),((([p,i1,s]|->2)|->{(1|->p),(2|->s)})|->p),
  ((([p,i1,s,o2]|->1)|->[p,i1,s])|->s),((([p,i1,s,o2]|->1)|->[p,i1,s])|->p),((([p,i1,s,o2]|->2)|->[p,s,o2])|->s),
  ((([p,i1,s,o2]|->2)|->[p,s,o2])|->p),((([p,i2,o1]|->1)|->{(1|->p),(2|->o1)})|->s),
  ((([p,i2,o1]|->1)|->{(1|->p),(2|->o1)})|->p),((([p,i2,o1]|->2)|->{(1|->p),(2|->i2)})|->s),
  ((([p,i2,o1]|->2)|->{(1|->p),(2|->i2)})|->p),((([p,i2,s]|->1)|->{(1|->p),(2|->s)})|->p),
  ((([p,i2,s]|->2)|->[p,i2,s])|->s),((([p,i2,s]|->2)|->[p,i2,s])|->p),
  ((([p,i2,s,o1]|->1)|->[p,s,o1])|->s),((([p,i2,s,o1]|->1)|->[p,s,o1])|->p),
  ((([p,i2,s,o1]|->2)|->[p,i2,s])|->s),((([p,i2,s,o1]|->2)|->[p,i2,s])|->p),
  ((([i1,s,o2]|->1)|->{(1|->i1),(2|->s)})|->s),((([i1,s,o2]|->1)|->{(1|->i1),(2|->s)})|->p),
  ((([i1,s,o2]|->2)|->{(1|->s),(2|->o2)})|->s),((([i1,s,o2]|->2)|->{(1|->s),(2|->o2)})|->p),
  ((([i2,s,o1]|->1)|->{(1|->s),(2|->o1)})|->s),((([i2,s,o1]|->1)|->{(1|->s),(2|->o1)})|->p),
  ((([i2,s,o1]|->2)|->{(1|->i2),(2|->s)})|->s),((([i2,s,o1]|->2)|->{(1|->i2),(2|->s)})|->p)}
Local decisions (disabled unconditionally): 
 {((({(1|->i1),(2|->o2)}|->2)|->{(1|->o2)})|->s),((({(1|->i1),(2|->s)}|->1)|->{(1|->i1),(2|->s)})|->s),
  ((({(1|->i1),(2|->s)}|->2)|->{(1|->s)})|->s),((({(1|->i1),(2|->s)}|->2)|->{(1|->s)})|->p),
  ((({(1|->i2),(2|->o1)}|->1)|->{(1|->o1)})|->s),((({(1|->i2),(2|->s)}|->1)|->{(1|->s)})|->s),
  ((({(1|->i2),(2|->s)}|->1)|->{(1|->s)})|->p),((({(1|->i2),(2|->s)}|->2)|->{(1|->i2),(2|->s)})|->s),
  ((({(1|->p)}|->1)|->{(1|->p)})|->p),((({(1|->p)}|->2)|->{(1|->p)})|->p),
  ((({(1|->p),(2|->i1)}|->2)|->{(1|->p)})|->p),((({(1|->p),(2|->i2)}|->1)|->{(1|->p)})|->p),
  ((([p,i1,o2]|->2)|->{(1|->p),(2|->o2)})|->s),((([p,i1,s]|->1)|->[p,i1,s])|->s),
  ((([p,i1,s]|->2)|->{(1|->p),(2|->s)})|->p),((([p,i1,s,o2]|->1)|->[p,i1,s])|->s),
  ((([p,i1,s,o2]|->2)|->[p,s,o2])|->s),((([p,i2,o1]|->1)|->{(1|->p),(2|->o1)})|->s),
  ((([p,i2,s]|->1)|->{(1|->p),(2|->s)})|->p),((([p,i2,s]|->2)|->[p,i2,s])|->s),
  ((([p,i2,s,o1]|->1)|->[p,s,o1])|->s),((([p,i2,s,o1]|->2)|->[p,i2,s])|->s),
  ((([i1,s,o2]|->1)|->{(1|->i1),(2|->s)})|->s),((([i1,s,o2]|->2)|->{(1|->s),(2|->o2)})|->s),
  ((([i2,s,o1]|->1)|->{(1|->s),(2|->o1)})|->s),((([i2,s,o1]|->2)|->{(1|->i2),(2|->s)})|->s)}
Local decisions (disabled conditionally): 
 {((({(1|->i1)}|->1)|->{(1|->i1)})|->p),((({(1|->i1),(2|->o2)}|->1)|->{(1|->i1)})|->p),
  ((({(1|->i1),(2|->o2)}|->2)|->{(1|->o2)})|->s),((({(1|->i1),(2|->o2)}|->2)|->{(1|->o2)})|->p),
  ((({(1|->i1),(2|->s)}|->1)|->{(1|->i1),(2|->s)})|->s),((({(1|->i1),(2|->s)}|->2)|->{(1|->s)})|->s),
  ((({(1|->i1),(2|->s)}|->2)|->{(1|->s)})|->p),((({(1|->i2)}|->2)|->{(1|->i2)})|->p),
  ((({(1|->i2),(2|->o1)}|->1)|->{(1|->o1)})|->s),((({(1|->i2),(2|->o1)}|->1)|->{(1|->o1)})|->p),
  ((({(1|->i2),(2|->o1)}|->2)|->{(1|->i2)})|->p),((({(1|->i2),(2|->s)}|->1)|->{(1|->s)})|->s),
  ((({(1|->i2),(2|->s)}|->1)|->{(1|->s)})|->p),((({(1|->i2),(2|->s)}|->2)|->{(1|->i2),(2|->s)})|->s),
  ((({(1|->p)}|->1)|->{(1|->p)})|->p),((({(1|->p)}|->2)|->{(1|->p)})|->p),
  ((({(1|->p),(2|->i1)}|->1)|->{(1|->p),(2|->i1)})|->p),((({(1|->p),(2|->i1)}|->2)|->{(1|->p)})|->p),
  ((({(1|->p),(2|->i2)}|->1)|->{(1|->p)})|->p),((({(1|->p),(2|->i2)}|->2)|->{(1|->p),(2|->i2)})|->p),
  ((([p,i1,o2]|->1)|->{(1|->p),(2|->i1)})|->p),((([p,i1,o2]|->2)|->{(1|->p),(2|->o2)})|->s),
  ((([p,i1,o2]|->2)|->{(1|->p),(2|->o2)})|->p),((([p,i1,s]|->1)|->[p,i1,s])|->s),
  ((([p,i1,s]|->1)|->[p,i1,s])|->p),((([p,i1,s]|->2)|->{(1|->p),(2|->s)})|->p),
  ((([p,i1,s,o2]|->1)|->[p,i1,s])|->s),((([p,i1,s,o2]|->1)|->[p,i1,s])|->p),
  ((([p,i1,s,o2]|->2)|->[p,s,o2])|->s),((([p,i1,s,o2]|->2)|->[p,s,o2])|->p),
  ((([p,i2,o1]|->1)|->{(1|->p),(2|->o1)})|->s),((([p,i2,o1]|->1)|->{(1|->p),(2|->o1)})|->p),
  ((([p,i2,o1]|->2)|->{(1|->p),(2|->i2)})|->p),((([p,i2,s]|->1)|->{(1|->p),(2|->s)})|->p),
  ((([p,i2,s]|->2)|->[p,i2,s])|->s),((([p,i2,s]|->2)|->[p,i2,s])|->p),
  ((([p,i2,s,o1]|->1)|->[p,s,o1])|->s),((([p,i2,s,o1]|->1)|->[p,s,o1])|->p),
  ((([p,i2,s,o1]|->2)|->[p,i2,s])|->s),((([p,i2,s,o1]|->2)|->[p,i2,s])|->p),
  ((([i1,s,o2]|->1)|->{(1|->i1),(2|->s)})|->s),((([i1,s,o2]|->2)|->{(1|->s),(2|->o2)})|->s),
  ((([i2,s,o1]|->1)|->{(1|->s),(2|->o1)})|->s),((([i2,s,o1]|->2)|->{(1|->i2),(2|->s)})|->s)}
Global decisions (enabled): 
 {({(1|->i1)}|->s),           ({(1|->i2)}|->s),
 ({(1|->i1),(2|->o2)}|->p),   ({(1|->i2),(2|->o1)}|->p),
 ({(1|->p),(2|->i1)}|->s),    ({(1|->p),(2|->i2)}|->s),
 ([p,i1,o2]|->p),             ([p,i2,o1]|->p),
 ([p,i1,s,o2]|->p),           ([p,i2,s,o1]|->p),
 ([i1,s,o2]|->p),             ([i2,s,o1]|->p)}
Global decisions (disabled): 
 {({(1|->i1)}|->p),                 ({(1|->i2)}|->p),
  ({(1|->i1),(2|->o2)}|->s),        ({(1|->i2),(2|->o1)}|->s),
  ({(1|->i1),(2|->s)}|->s),         ({(1|->i2),(2|->s)}|->s),   due to avoid cycles in the generation of sublanguages
  ({(1|->i1),(2|->s)}|->p),         ({(1|->i2),(2|->s)}|->p),
  ({(1|->p)}|->p),                                              due to avoid cycles in the generation of sublanguages
  ({(1|->p),(2|->i1)}|->p),         ({(1|->p),(2|->i2)}|->p),
  ([p,i1,o2]|->s),                  ([p,i2,o1]|->s),
  ([p,i1,s]|->s),                   ([p,i2,s]|->s),             due to avoid cycles in the generation of sublanguages
  ([p,i1,s]|->p),                   ([p,i2,s]|->p),
  ([p,i1,s,o2]|->s),                ([p,i2,s,o1]|->s),
  ([i1,s,o2]|->s),                  ([i2,s,o1]|->s)}
"The control is nonconflicting."

ALL OPERATIONS COVERED

perform_random_step(1,3)
random(2,1)
finished_random_animate(3,[runtime/25450,total_runtime/25450,walltime/28600])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 28890 ms walltime (25720 ms runtime), since start: 30020 ms

% Finished process_file (errors=0, warnings=0)
% ProB Finished
*/