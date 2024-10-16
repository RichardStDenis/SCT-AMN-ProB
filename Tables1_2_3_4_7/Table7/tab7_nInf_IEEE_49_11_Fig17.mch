// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL tab7_nInf_IEEE_49_11_Fig17

SEES
 //---------------------------------------------------------------------
 // Machine for input data
 //---------------------------------------------------------------------
 IEEE_49_11_H_Fig17/*@file "../../DataExamples/IEEE_49_11_H_Fig17.mch" */

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MININT == 0;
 SET_PREF_MAXINT == 6;
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 1000000;
 SET_PREF_SMT == TRUE;
 SET_PREF_KODKOD == FALSE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../../Theories/SCT_core.def";
 "../../Theories/SCT_decentralized.def";
 "LibraryIO.def"

SETS
 Decisions = {enabled, disabled, unsure}

 ABSTRACT_CONSTANTS GetLangPairs_

CONSTANTS
 T_G, L_G, T_H, L_H,    // Languages for free and legal behaviors
 N                      // Main argument of the observability property

PROPERTIES
 G_fb : Generator_C_O(G_fb'states,G_fb'events) &
 H_lb : FSA(H_lb'states,H_lb'events) &
 T_G = GetTracesNc1stNqcSev(G_fb,bound,{p,s}) &
 L_G = Traces2Strings(T_G) &
 T_H = GetTracesNc1stNqcSev(H_lb,bound,{p,s}) &
 L_H = Traces2Strings(T_H) &
 N = 4 &

 GetLangPairs_ = GetLangPairs__(G_fb,A_d,GetLangPairs_)

VARIABLES
 code
INITIALISATION
 code := 0
INVARIANT
 code >= 0 or code <= 1

OPERATIONS
 VerifyN_InferenceObservability =
  PRE code = 0 THEN
   VAR Sigma_c,              // a subset of the set of controllable events
       e_,                   // a controllable event of Sigma_c
       LP_e,                 // all language pairs (Dk,Ek) for a given event
       n_e, N_e IN
    Sigma_c := G_fb'ctrlEvents;   N_e := 0;
    WHILE Sigma_c /= {} & N_e /= -1 DO   // loop on all controllable events
     e_ :: Sigma_c;          // choice of a controllable event
     LP_e := GetLangPairs_(e_,baseCaseD0E0s(e_,L_G,L_H),N,0);
     //PRINT(LP_e);
     n_e := max(dom(LP_e'D) \/ dom(LP_e'E));
     //PRINT(n_e);
     //PRINT(Dk_1Ek_1(e_,LP_e'E[{n_e}],LP_e'D[{n_e}],A_d));
     //PRINT(Dk_1Ek_1(e_,LP_e'D[{n_e}],LP_e'E[{n_e}],A_d));
     IF Dk_1Ek_1(e_,LP_e'E[{n_e}],LP_e'D[{n_e}],A_d) = {} or
        Dk_1Ek_1(e_,LP_e'D[{n_e}],LP_e'E[{n_e}],A_d) = {} THEN
       N_e := max({N_e,n_e}) ELSE N_e := -1 END;
     Sigma_c := Sigma_c - {e_}
    INVARIANT Sigma_c <: G_fb'ctrlEvents VARIANT card(Sigma_c) END;
    IF N_e >= 0 & N_e <= N THEN
     code := 1 || PRINTF("K is ~w-inference observable\n",[N_e]) ELSE
     PRINTF("K is not ~w-inference observable\n",[N]) END END END;

 GetDecentralizedSupervisor = 
  PRE code = 1 THEN
   VAR Sigma_c,      // a subset of the set of controllable events
       e_,           // a controllable event of Sigma_c
       allPLs,       // all language pairs (Dk,Ek) for a given event
       L,            // a subset of L(G)
       s_,           // a string of L
       i_th,         // index of the ith supervisor
       ndi_s_sigma,  // ambiguity level of the disablement decision of a local supervisor
       nei_s_sigma,  // ambiguity level of the enablement decision of a local supervisor
       ni_s_sigma,   // ambiguity level of a local supervisor
       ci_s_sigma,   // control decision of a local supervisors
       nI_s_sigma,   // ambiguity levels of all local supervisors
       cI_s_sigma,   // control decisions of all local supervisors
       n_s_sigma,    // minimum ambiguity level of the decentralized supervisor
       S_s_sigma IN  // control decision of the decentralized supervisor
    Sigma_c := G_fb'ctrlEvents;
    WHILE Sigma_c /= {} DO   // for all controllable events
     e_ :: Sigma_c;
     //PRINT("Dk and Ek for:"); PRINT(e_);
     allPLs := GetLangPairs_(e_,baseCaseD0E0s(e_,L_G,L_H),N,0);
     //PRINT(allPLs);
     L := L_G;
     WHILE L /= {} DO       // for all strings of L(G)
      s_ :: L;
      PRINT(s_);
      nI_s_sigma := {};   cI_s_sigma:= {};
      i_th := 1;
      WHILE i_th <= A_d'NoSupervisors DO  // for all local supervisors
       IF i_th : In(e_,A_d) THEN
        PRINT(i_th);   PRINT(Mi(i_th,s_,A_d));
        ndi_s_sigma := nde_i(i_th,s_,allPLs'E,N,A_d);
        PRINTF("| ~w ",[ndi_s_sigma]);
        nei_s_sigma := nde_i(i_th,s_,allPLs'D,N,A_d);
        PRINTF(" | ~w ",[nei_s_sigma]);
        c_i(ci_s_sigma, ndi_s_sigma,nei_s_sigma);
        PRINTF(" | ~w ",[ci_s_sigma]);
        ni_s_sigma := n_i(ndi_s_sigma,nei_s_sigma);
        PRINTF(" | ~w |\n",[ni_s_sigma]);
        nI_s_sigma := nI_s_sigma \/ {i_th|->ni_s_sigma};
        cI_s_sigma := cI_s_sigma \/ {i_th|->ci_s_sigma} END; // if
       i_th := i_th + 1
      INVARIANT i_th : 1..A_d'NoSupervisors+1 VARIANT A_d'NoSupervisors-i_th END; // while i_th
      n_s_sigma := n_(nI_s_sigma);
      S_i_I(S_s_sigma,nI_s_sigma,cI_s_sigma,n_s_sigma,e_,A_d);
      PRINTF("global decision for ~w", [s_]);   PRINTF(" and ~w = ",[e_]);
      PRINTF("(~w, ",[n_s_sigma]);   PRINTF("~w)\n",[S_s_sigma]);
      L := L - {s_}
     INVARIANT L <: L_G VARIANT card(L) END; // while L
     Sigma_c := Sigma_c - {e_}
    INVARIANT Sigma_c <: G_fb'ctrlEvents VARIANT card(Sigma_c) END END END END

/*
OUTPUT (IEEE_49_11_H_Fig12):
% Runtime for SOLUTION for SETUP_CONSTANTS: 200 ms (walltime: 210 ms)
% Finished searching for valid initial states 240 ms walltime (230 ms runtime), since start: 2230 ms
% Starting random_animate (max_steps=4)
perform_random_step(1,1)
random(2,1)
perform_random_step(1,2)
random(2,1)
K is 2-inference observable
perform_random_step(1,3)
random(2,1)
{}
1
{}
| 2  | 3  | 0  | 2 |
2
{}
| 2  | 3  | 0  | 2 |
global decision for {} and s = (2, disabled)
{(1|->i1)}
1
{(1|->i1)}
| 2  | 1  | 1  | 1 |
2
{}
| 2  | 3  | 0  | 2 |
global decision for {(1|->i1)} and s = (1, enabled)
{(1|->i1),(2|->o2)}
1
{(1|->i1)}
| 2  | 1  | 1  | 1 |
2
{(1|->o2)}
| 0  | 1  | 0  | 0 |
global decision for {(1|->i1),(2|->o2)} and s = (0, disabled)
{(1|->i1),(2|->s)}
1
{(1|->i1),(2|->s)}
| 0  | 1  | 0  | 0 |
2
{(1|->s)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->i1),(2|->s)} and s = (0, unsure)
{(1|->i1),(2|->p)}
1
{(1|->i1),(2|->p)}
| 0  | 0  | -1  | 0 |
2
{(1|->p)}
| 2  | 3  | 0  | 2 |
global decision for {(1|->i1),(2|->p)} and s = (0, unsure)
{(1|->i2)}
1
{}
| 2  | 3  | 0  | 2 |
2
{(1|->i2)}
| 2  | 1  | 1  | 1 |
global decision for {(1|->i2)} and s = (1, enabled)
{(1|->i2),(2|->o1)}
1
{(1|->o1)}
| 0  | 1  | 0  | 0 |
2
{(1|->i2)}
| 2  | 1  | 1  | 1 |
global decision for {(1|->i2),(2|->o1)} and s = (0, disabled)
{(1|->i2),(2|->s)}
1
{(1|->s)}
| 0  | 0  | -1  | 0 |
2
{(1|->i2),(2|->s)}
| 0  | 1  | 0  | 0 |
global decision for {(1|->i2),(2|->s)} and s = (0, unsure)
{(1|->i2),(2|->p)}
1
{(1|->p)}
| 2  | 3  | 0  | 2 |
2
{(1|->i2),(2|->p)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->i2),(2|->p)} and s = (0, unsure)
{(1|->s)}
1
{(1|->s)}
| 0  | 0  | -1  | 0 |
2
{(1|->s)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->s)} and s = (0, unsure)
{(1|->s),(2|->i1)}
1
{(1|->s),(2|->i1)}
| 0  | 0  | -1  | 0 |
2
{(1|->s)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->s),(2|->i1)} and s = (0, unsure)
{(1|->s),(2|->i2)}
1
{(1|->s)}
| 0  | 0  | -1  | 0 |
2
{(1|->s),(2|->i2)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->s),(2|->i2)} and s = (0, unsure)
{(1|->s),(2|->p)}
1
{(1|->s),(2|->p)}
| 0  | 0  | -1  | 0 |
2
{(1|->s),(2|->p)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->s),(2|->p)} and s = (0, unsure)
{(1|->p)}
1
{(1|->p)}
| 2  | 3  | 0  | 2 |
2
{(1|->p)}
| 2  | 3  | 0  | 2 |
global decision for {(1|->p)} and s = (2, disabled)
{(1|->p),(2|->i1)}
1
{(1|->p),(2|->i1)}
| 2  | 1  | 1  | 1 |
2
{(1|->p)}
| 2  | 3  | 0  | 2 |
global decision for {(1|->p),(2|->i1)} and s = (1, enabled)
{(1|->p),(2|->i2)}
1
{(1|->p)}
| 2  | 3  | 0  | 2 |
2
{(1|->p),(2|->i2)}
| 2  | 1  | 1  | 1 |
global decision for {(1|->p),(2|->i2)} and s = (1, enabled)
{(1|->p),(2|->s)}
1
{(1|->p),(2|->s)}
| 0  | 0  | -1  | 0 |
2
{(1|->p),(2|->s)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->p),(2|->s)} and s = (0, unsure)
[s,i1,o2]
1
{(1|->s),(2|->i1)}
| 0  | 0  | -1  | 0 |
2
{(1|->s),(2|->o2)}
| 0  | 1  | 0  | 0 |
global decision for [s,i1,o2] and s = (0, unsure)
[s,i1,s]
1
[s,i1,s]
| 0  | 0  | -1  | 0 |
2
{(1|->s),(2|->s)}
| 0  | 0  | -1  | 0 |
global decision for [s,i1,s] and s = (0, unsure)
[s,i1,p]
1
[s,i1,p]
| 0  | 0  | -1  | 0 |
2
{(1|->s),(2|->p)}
| 0  | 0  | -1  | 0 |
global decision for [s,i1,p] and s = (0, unsure)
[p,i1,o2]
1
{(1|->p),(2|->i1)}
| 2  | 1  | 1  | 1 |
2
{(1|->p),(2|->o2)}
| 0  | 1  | 0  | 0 |
global decision for [p,i1,o2] and s = (0, disabled)
[p,i1,s]
1
[p,i1,s]
| 0  | 1  | 0  | 0 |
2
{(1|->p),(2|->s)}
| 0  | 0  | -1  | 0 |
global decision for [p,i1,s] and s = (0, unsure)
[p,i1,p]
1
[p,i1,p]
| 0  | 0  | -1  | 0 |
2
{(1|->p),(2|->p)}
| 0  | 0  | -1  | 0 |
global decision for [p,i1,p] and s = (0, unsure)
[s,i1,o2,s]
1
[s,i1,s]
| 0  | 0  | -1  | 0 |
2
[s,o2,s]
| 0  | 0  | -1  | 0 |
global decision for [s,i1,o2,s] and s = (0, unsure)
[s,i1,o2,p]
1
[s,i1,p]
| 0  | 0  | -1  | 0 |
2
[s,o2,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i1,o2,p] and s = (0, unsure)
[s,i1,s,o2]
1
[s,i1,s]
| 0  | 0  | -1  | 0 |
2
[s,s,o2]
| 0  | 0  | -1  | 0 |
global decision for [s,i1,s,o2] and s = (0, unsure)
[s,i1,s,p]
1
[s,i1,s,p]
| 0  | 0  | -1  | 0 |
2
[s,s,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i1,s,p] and s = (0, unsure)
[s,i1,p,i2]
1
[s,i1,p]
| 0  | 0  | -1  | 0 |
2
[s,p,i2]
| 0  | 0  | -1  | 0 |
global decision for [s,i1,p,i2] and s = (0, unsure)
[p,i1,o2,s]
1
[p,i1,s]
| 0  | 1  | 0  | 0 |
2
[p,o2,s]
| 0  | 0  | -1  | 0 |
global decision for [p,i1,o2,s] and s = (0, unsure)
[p,i1,o2,p]
1
[p,i1,p]
| 0  | 0  | -1  | 0 |
2
[p,o2,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i1,o2,p] and s = (0, unsure)
[p,i1,s,o2]
1
[p,i1,s]
| 0  | 1  | 0  | 0 |
2
[p,s,o2]
| 0  | 1  | 0  | 0 |
global decision for [p,i1,s,o2] and s = (0, disabled)
[p,i1,s,p]
1
[p,i1,s,p]
| 0  | 0  | -1  | 0 |
2
[p,s,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i1,s,p] and s = (0, unsure)
[p,i1,p,i2]
1
[p,i1,p]
| 0  | 0  | -1  | 0 |
2
[p,p,i2]
| 0  | 0  | -1  | 0 |
global decision for [p,i1,p,i2] and s = (0, unsure)
[s,i2,o1]
1
{(1|->s),(2|->o1)}
| 0  | 1  | 0  | 0 |
2
{(1|->s),(2|->i2)}
| 0  | 0  | -1  | 0 |
global decision for [s,i2,o1] and s = (0, unsure)
[s,i2,s]
1
{(1|->s),(2|->s)}
| 0  | 0  | -1  | 0 |
2
[s,i2,s]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,s] and s = (0, unsure)
[s,i2,p]
1
{(1|->s),(2|->p)}
| 0  | 0  | -1  | 0 |
2
[s,i2,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,p] and s = (0, unsure)
[p,i2,o1]
1
{(1|->p),(2|->o1)}
| 0  | 1  | 0  | 0 |
2
{(1|->p),(2|->i2)}
| 2  | 1  | 1  | 1 |
global decision for [p,i2,o1] and s = (0, disabled)
[p,i2,s]
1
{(1|->p),(2|->s)}
| 0  | 0  | -1  | 0 |
2
[p,i2,s]
| 0  | 1  | 0  | 0 |
global decision for [p,i2,s] and s = (0, unsure)
[p,i2,p]
1
{(1|->p),(2|->p)}
| 0  | 0  | -1  | 0 |
2
[p,i2,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i2,p] and s = (0, unsure)
[s,i2,o1,s]
1
[s,o1,s]
| 0  | 0  | -1  | 0 |
2
[s,i2,s]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,o1,s] and s = (0, unsure)
[s,i2,o1,p]
1
[s,o1,p]
| 0  | 0  | -1  | 0 |
2
[s,i2,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,o1,p] and s = (0, unsure)
[s,i2,s,o1]
1
[s,s,o1]
| 0  | 0  | -1  | 0 |
2
[s,i2,s]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,s,o1] and s = (0, unsure)
[s,i2,s,p]
1
[s,s,p]
| 0  | 0  | -1  | 0 |
2
[s,i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,s,p] and s = (0, unsure)
[s,i2,p,i1]
1
[s,p,i1]
| 0  | 0  | -1  | 0 |
2
[s,i2,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,p,i1] and s = (0, unsure)
[p,i2,o1,s]
1
[p,o1,s]
| 0  | 0  | -1  | 0 |
2
[p,i2,s]
| 0  | 1  | 0  | 0 |
global decision for [p,i2,o1,s] and s = (0, unsure)
[p,i2,o1,p]
1
[p,o1,p]
| 0  | 0  | -1  | 0 |
2
[p,i2,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i2,o1,p] and s = (0, unsure)
[p,i2,s,o1]
1
[p,s,o1]
| 0  | 1  | 0  | 0 |
2
[p,i2,s]
| 0  | 1  | 0  | 0 |
global decision for [p,i2,s,o1] and s = (0, disabled)
[p,i2,s,p]
1
[p,s,p]
| 0  | 0  | -1  | 0 |
2
[p,i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i2,s,p] and s = (0, unsure)
[p,i2,p,i1]
1
[p,p,i1]
| 0  | 0  | -1  | 0 |
2
[p,i2,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i2,p,i1] and s = (0, unsure)
[i2,o1,s]
1
{(1|->o1),(2|->s)}
| 0  | 0  | -1  | 0 |
2
{(1|->i2),(2|->s)}
| 0  | 1  | 0  | 0 |
global decision for [i2,o1,s] and s = (0, unsure)
[i2,o1,p]
1
{(1|->o1),(2|->p)}
| 0  | 0  | -1  | 0 |
2
{(1|->i2),(2|->p)}
| 0  | 0  | -1  | 0 |
global decision for [i2,o1,p] and s = (0, unsure)
[i2,o1,s,p]
1
[o1,s,p]
| 0  | 0  | -1  | 0 |
2
[i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [i2,o1,s,p] and s = (0, unsure)
[i1,o2,s]
1
{(1|->i1),(2|->s)}
| 0  | 1  | 0  | 0 |
2
{(1|->o2),(2|->s)}
| 0  | 0  | -1  | 0 |
global decision for [i1,o2,s] and s = (0, unsure)
[i1,o2,p]
1
{(1|->i1),(2|->p)}
| 0  | 0  | -1  | 0 |
2
{(1|->o2),(2|->p)}
| 0  | 0  | -1  | 0 |
global decision for [i1,o2,p] and s = (0, unsure)
[i1,o2,s,p]
1
[i1,s,p]
| 0  | 0  | -1  | 0 |
2
[o2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [i1,o2,s,p] and s = (0, unsure)
[i1,s,o2]
1
{(1|->i1),(2|->s)}
| 0  | 1  | 0  | 0 |
2
{(1|->s),(2|->o2)}
| 0  | 1  | 0  | 0 |
global decision for [i1,s,o2] and s = (0, disabled)
[i1,s,p]
1
[i1,s,p]
| 0  | 0  | -1  | 0 |
2
{(1|->s),(2|->p)}
| 0  | 0  | -1  | 0 |
global decision for [i1,s,p] and s = (0, unsure)
[i2,s,o1]
1
{(1|->s),(2|->o1)}
| 0  | 1  | 0  | 0 |
2
{(1|->i2),(2|->s)}
| 0  | 1  | 0  | 0 |
global decision for [i2,s,o1] and s = (0, disabled)
[i2,s,p]
1
{(1|->s),(2|->p)}
| 0  | 0  | -1  | 0 |
2
[i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [i2,s,p] and s = (0, unsure)
[i1,s,o2,s]
1
[i1,s,s]
| 0  | 0  | -1  | 0 |
2
[s,o2,s]
| 0  | 0  | -1  | 0 |
global decision for [i1,s,o2,s] and s = (0, unsure)
[i1,s,o2,p]
1
[i1,s,p]
| 0  | 0  | -1  | 0 |
2
[s,o2,p]
| 0  | 0  | -1  | 0 |
global decision for [i1,s,o2,p] and s = (0, unsure)
[i1,s,p,i2]
1
[i1,s,p]
| 0  | 0  | -1  | 0 |
2
[s,p,i2]
| 0  | 0  | -1  | 0 |
global decision for [i1,s,p,i2] and s = (0, unsure)
[i2,s,o1,s]
1
[s,o1,s]
| 0  | 0  | -1  | 0 |
2
[i2,s,s]
| 0  | 0  | -1  | 0 |
global decision for [i2,s,o1,s] and s = (0, unsure)
[i2,s,o1,p]
1
[s,o1,p]
| 0  | 0  | -1  | 0 |
2
[i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [i2,s,o1,p] and s = (0, unsure)
[i2,s,p,i1]
1
[s,p,i1]
| 0  | 0  | -1  | 0 |
2
[i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [i2,s,p,i1] and s = (0, unsure)
[i1,p,i2]
1
{(1|->i1),(2|->p)}
| 0  | 0  | -1  | 0 |
2
{(1|->p),(2|->i2)}
| 2  | 1  | 1  | 1 |
global decision for [i1,p,i2] and s = (0, unsure)
[i2,p,i1]
1
{(1|->p),(2|->i1)}
| 2  | 1  | 1  | 1 |
2
{(1|->i2),(2|->p)}
| 0  | 0  | -1  | 0 |
global decision for [i2,p,i1] and s = (0, unsure)
[i2,s,o1,s,p]
1
[s,o1,s,p]
| 0  | 0  | -1  | 0 |
2
[i2,s,s,p]
| 0  | 0  | -1  | 0 |
global decision for [i2,s,o1,s,p] and s = (0, unsure)
[s,i2,o1,s,p]
1
[s,o1,s,p]
| 0  | 0  | -1  | 0 |
2
[s,i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,o1,s,p] and s = (0, unsure)
[p,i2,o1,s,p]
1
[p,o1,s,p]
| 0  | 0  | -1  | 0 |
2
[p,i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i2,o1,s,p] and s = (0, unsure)
[i1,s,o2,s,p]
1
[i1,s,s,p]
| 0  | 0  | -1  | 0 |
2
[s,o2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [i1,s,o2,s,p] and s = (0, unsure)
[s,i1,o2,s,p]
1
[s,i1,s,p]
| 0  | 0  | -1  | 0 |
2
[s,o2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i1,o2,s,p] and s = (0, unsure)
[p,i1,o2,s,p]
1
[p,i1,s,p]
| 0  | 0  | -1  | 0 |
2
[p,o2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i1,o2,s,p] and s = (0, unsure)
[s,i1,s,o2,s]
1
[s,i1,s,s]
| 0  | 0  | -1  | 0 |
2
[s,s,o2,s]
| 0  | 0  | -1  | 0 |
global decision for [s,i1,s,o2,s] and s = (0, unsure)
[s,i1,s,o2,p]
1
[s,i1,s,p]
| 0  | 0  | -1  | 0 |
2
[s,s,o2,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i1,s,o2,p] and s = (0, unsure)
[s,i1,s,p,i2]
1
[s,i1,s,p]
| 0  | 0  | -1  | 0 |
2
[s,s,p,i2]
| 0  | 0  | -1  | 0 |
global decision for [s,i1,s,p,i2] and s = (0, unsure)
[s,i1,s,o2,s,p]
1
[s,i1,s,s,p]
| 0  | 0  | -1  | 0 |
2
[s,s,o2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i1,s,o2,s,p] and s = (0, unsure)
[s,i2,s,o1,s]
1
[s,s,o1,s]
| 0  | 0  | -1  | 0 |
2
[s,i2,s,s]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,s,o1,s] and s = (0, unsure)
[s,i2,s,o1,p]
1
[s,s,o1,p]
| 0  | 0  | -1  | 0 |
2
[s,i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,s,o1,p] and s = (0, unsure)
[s,i2,s,p,i1]
1
[s,s,p,i1]
| 0  | 0  | -1  | 0 |
2
[s,i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,s,p,i1] and s = (0, unsure)
[s,i2,s,o1,s,p]
1
[s,s,o1,s,p]
| 0  | 0  | -1  | 0 |
2
[s,i2,s,s,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,s,o1,s,p] and s = (0, unsure)
[p,i1,s,o2,s]
1
[p,i1,s,s]
| 0  | 0  | -1  | 0 |
2
[p,s,o2,s]
| 0  | 0  | -1  | 0 |
global decision for [p,i1,s,o2,s] and s = (0, unsure)
[p,i1,s,o2,p]
1
[p,i1,s,p]
| 0  | 0  | -1  | 0 |
2
[p,s,o2,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i1,s,o2,p] and s = (0, unsure)
[p,i1,s,p,i2]
1
[p,i1,s,p]
| 0  | 0  | -1  | 0 |
2
[p,s,p,i2]
| 0  | 0  | -1  | 0 |
global decision for [p,i1,s,p,i2] and s = (0, unsure)
[p,i1,s,o2,s,p]
1
[p,i1,s,s,p]
| 0  | 0  | -1  | 0 |
2
[p,s,o2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i1,s,o2,s,p] and s = (0, unsure)
[p,i2,s,o1,s]
1
[p,s,o1,s]
| 0  | 0  | -1  | 0 |
2
[p,i2,s,s]
| 0  | 0  | -1  | 0 |
global decision for [p,i2,s,o1,s] and s = (0, unsure)
[p,i2,s,o1,p]
1
[p,s,o1,p]
| 0  | 0  | -1  | 0 |
2
[p,i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i2,s,o1,p] and s = (0, unsure)
[p,i2,s,p,i1]
1
[p,s,p,i1]
| 0  | 0  | -1  | 0 |
2
[p,i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i2,s,p,i1] and s = (0, unsure)
[p,i2,s,o1,s,p]
1
[p,s,o1,s,p]
| 0  | 0  | -1  | 0 |
2
[p,i2,s,s,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i2,s,o1,s,p] and s = (0, unsure)
{}
1
{}
| 3  | 2  | 1  | 2 |
2
{}
| 3  | 2  | 1  | 2 |
global decision for {} and p = (2, enabled)
{(1|->i1)}
1
{(1|->i1)}
| 1  | 2  | 0  | 1 |
2
{}
| 3  | 2  | 1  | 2 |
global decision for {(1|->i1)} and p = (1, disabled)
{(1|->i1),(2|->o2)}
1
{(1|->i1)}
| 1  | 2  | 0  | 1 |
2
{(1|->o2)}
| 1  | 0  | 1  | 0 |
global decision for {(1|->i1),(2|->o2)} and p = (0, enabled)
{(1|->i1),(2|->s)}
1
{(1|->i1),(2|->s)}
| 1  | 1  | -1  | 1 |
2
{(1|->s)}
| 0  | 1  | 0  | 0 |
global decision for {(1|->i1),(2|->s)} and p = (0, disabled)
{(1|->i1),(2|->p)}
1
{(1|->i1),(2|->p)}
| 0  | 0  | -1  | 0 |
2
{(1|->p)}
| 0  | 1  | 0  | 0 |
global decision for {(1|->i1),(2|->p)} and p = (0, unsure)
{(1|->i2)}
1
{}
| 3  | 2  | 1  | 2 |
2
{(1|->i2)}
| 1  | 2  | 0  | 1 |
global decision for {(1|->i2)} and p = (1, disabled)
{(1|->i2),(2|->o1)}
1
{(1|->o1)}
| 1  | 0  | 1  | 0 |
2
{(1|->i2)}
| 1  | 2  | 0  | 1 |
global decision for {(1|->i2),(2|->o1)} and p = (0, enabled)
{(1|->i2),(2|->s)}
1
{(1|->s)}
| 0  | 1  | 0  | 0 |
2
{(1|->i2),(2|->s)}
| 1  | 1  | -1  | 1 |
global decision for {(1|->i2),(2|->s)} and p = (0, disabled)
{(1|->i2),(2|->p)}
1
{(1|->p)}
| 0  | 1  | 0  | 0 |
2
{(1|->i2),(2|->p)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->i2),(2|->p)} and p = (0, unsure)
{(1|->s)}
1
{(1|->s)}
| 0  | 1  | 0  | 0 |
2
{(1|->s)}
| 0  | 1  | 0  | 0 |
global decision for {(1|->s)} and p = (0, disabled)
{(1|->s),(2|->i1)}
1
{(1|->s),(2|->i1)}
| 0  | 0  | -1  | 0 |
2
{(1|->s)}
| 0  | 1  | 0  | 0 |
global decision for {(1|->s),(2|->i1)} and p = (0, unsure)
{(1|->s),(2|->i2)}
1
{(1|->s)}
| 0  | 1  | 0  | 0 |
2
{(1|->s),(2|->i2)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->s),(2|->i2)} and p = (0, unsure)
{(1|->s),(2|->p)}
1
{(1|->s),(2|->p)}
| 0  | 0  | -1  | 0 |
2
{(1|->s),(2|->p)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->s),(2|->p)} and p = (0, unsure)
{(1|->p)}
1
{(1|->p)}
| 0  | 1  | 0  | 0 |
2
{(1|->p)}
| 0  | 1  | 0  | 0 |
global decision for {(1|->p)} and p = (0, disabled)
{(1|->p),(2|->i1)}
1
{(1|->p),(2|->i1)}
| 1  | 1  | -1  | 1 |
2
{(1|->p)}
| 0  | 1  | 0  | 0 |
global decision for {(1|->p),(2|->i1)} and p = (0, disabled)
{(1|->p),(2|->i2)}
1
{(1|->p)}
| 0  | 1  | 0  | 0 |
2
{(1|->p),(2|->i2)}
| 1  | 1  | -1  | 1 |
global decision for {(1|->p),(2|->i2)} and p = (0, disabled)
{(1|->p),(2|->s)}
1
{(1|->p),(2|->s)}
| 0  | 1  | 0  | 0 |
2
{(1|->p),(2|->s)}
| 0  | 1  | 0  | 0 |
global decision for {(1|->p),(2|->s)} and p = (0, disabled)
[s,i1,o2]
1
{(1|->s),(2|->i1)}
| 0  | 0  | -1  | 0 |
2
{(1|->s),(2|->o2)}
| 1  | 0  | 1  | 0 |
global decision for [s,i1,o2] and p = (0, unsure)
[s,i1,s]
1
[s,i1,s]
| 0  | 0  | -1  | 0 |
2
{(1|->s),(2|->s)}
| 0  | 0  | -1  | 0 |
global decision for [s,i1,s] and p = (0, unsure)
[s,i1,p]
1
[s,i1,p]
| 0  | 0  | -1  | 0 |
2
{(1|->s),(2|->p)}
| 0  | 0  | -1  | 0 |
global decision for [s,i1,p] and p = (0, unsure)
[p,i1,o2]
1
{(1|->p),(2|->i1)}
| 1  | 1  | -1  | 1 |
2
{(1|->p),(2|->o2)}
| 1  | 0  | 1  | 0 |
global decision for [p,i1,o2] and p = (0, enabled)
[p,i1,s]
1
[p,i1,s]
| 1  | 1  | -1  | 1 |
2
{(1|->p),(2|->s)}
| 0  | 1  | 0  | 0 |
global decision for [p,i1,s] and p = (0, disabled)
[p,i1,p]
1
[p,i1,p]
| 0  | 0  | -1  | 0 |
2
{(1|->p),(2|->p)}
| 0  | 0  | -1  | 0 |
global decision for [p,i1,p] and p = (0, unsure)
[s,i1,o2,s]
1
[s,i1,s]
| 0  | 0  | -1  | 0 |
2
[s,o2,s]
| 0  | 0  | -1  | 0 |
global decision for [s,i1,o2,s] and p = (0, unsure)
[s,i1,o2,p]
1
[s,i1,p]
| 0  | 0  | -1  | 0 |
2
[s,o2,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i1,o2,p] and p = (0, unsure)
[s,i1,s,o2]
1
[s,i1,s]
| 0  | 0  | -1  | 0 |
2
[s,s,o2]
| 0  | 0  | -1  | 0 |
global decision for [s,i1,s,o2] and p = (0, unsure)
[s,i1,s,p]
1
[s,i1,s,p]
| 0  | 0  | -1  | 0 |
2
[s,s,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i1,s,p] and p = (0, unsure)
[s,i1,p,i2]
1
[s,i1,p]
| 0  | 0  | -1  | 0 |
2
[s,p,i2]
| 0  | 0  | -1  | 0 |
global decision for [s,i1,p,i2] and p = (0, unsure)
[p,i1,o2,s]
1
[p,i1,s]
| 1  | 1  | -1  | 1 |
2
[p,o2,s]
| 0  | 0  | -1  | 0 |
global decision for [p,i1,o2,s] and p = (0, unsure)
[p,i1,o2,p]
1
[p,i1,p]
| 0  | 0  | -1  | 0 |
2
[p,o2,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i1,o2,p] and p = (0, unsure)
[p,i1,s,o2]
1
[p,i1,s]
| 1  | 1  | -1  | 1 |
2
[p,s,o2]
| 1  | 0  | 1  | 0 |
global decision for [p,i1,s,o2] and p = (0, enabled)
[p,i1,s,p]
1
[p,i1,s,p]
| 0  | 0  | -1  | 0 |
2
[p,s,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i1,s,p] and p = (0, unsure)
[p,i1,p,i2]
1
[p,i1,p]
| 0  | 0  | -1  | 0 |
2
[p,p,i2]
| 0  | 0  | -1  | 0 |
global decision for [p,i1,p,i2] and p = (0, unsure)
[s,i2,o1]
1
{(1|->s),(2|->o1)}
| 1  | 0  | 1  | 0 |
2
{(1|->s),(2|->i2)}
| 0  | 0  | -1  | 0 |
global decision for [s,i2,o1] and p = (0, unsure)
[s,i2,s]
1
{(1|->s),(2|->s)}
| 0  | 0  | -1  | 0 |
2
[s,i2,s]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,s] and p = (0, unsure)
[s,i2,p]
1
{(1|->s),(2|->p)}
| 0  | 0  | -1  | 0 |
2
[s,i2,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,p] and p = (0, unsure)
[p,i2,o1]
1
{(1|->p),(2|->o1)}
| 1  | 0  | 1  | 0 |
2
{(1|->p),(2|->i2)}
| 1  | 1  | -1  | 1 |
global decision for [p,i2,o1] and p = (0, enabled)
[p,i2,s]
1
{(1|->p),(2|->s)}
| 0  | 1  | 0  | 0 |
2
[p,i2,s]
| 1  | 1  | -1  | 1 |
global decision for [p,i2,s] and p = (0, disabled)
[p,i2,p]
1
{(1|->p),(2|->p)}
| 0  | 0  | -1  | 0 |
2
[p,i2,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i2,p] and p = (0, unsure)
[s,i2,o1,s]
1
[s,o1,s]
| 0  | 0  | -1  | 0 |
2
[s,i2,s]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,o1,s] and p = (0, unsure)
[s,i2,o1,p]
1
[s,o1,p]
| 0  | 0  | -1  | 0 |
2
[s,i2,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,o1,p] and p = (0, unsure)
[s,i2,s,o1]
1
[s,s,o1]
| 0  | 0  | -1  | 0 |
2
[s,i2,s]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,s,o1] and p = (0, unsure)
[s,i2,s,p]
1
[s,s,p]
| 0  | 0  | -1  | 0 |
2
[s,i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,s,p] and p = (0, unsure)
[s,i2,p,i1]
1
[s,p,i1]
| 0  | 0  | -1  | 0 |
2
[s,i2,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,p,i1] and p = (0, unsure)
[p,i2,o1,s]
1
[p,o1,s]
| 0  | 0  | -1  | 0 |
2
[p,i2,s]
| 1  | 1  | -1  | 1 |
global decision for [p,i2,o1,s] and p = (0, unsure)
[p,i2,o1,p]
1
[p,o1,p]
| 0  | 0  | -1  | 0 |
2
[p,i2,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i2,o1,p] and p = (0, unsure)
[p,i2,s,o1]
1
[p,s,o1]
| 1  | 0  | 1  | 0 |
2
[p,i2,s]
| 1  | 1  | -1  | 1 |
global decision for [p,i2,s,o1] and p = (0, enabled)
[p,i2,s,p]
1
[p,s,p]
| 0  | 0  | -1  | 0 |
2
[p,i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i2,s,p] and p = (0, unsure)
[p,i2,p,i1]
1
[p,p,i1]
| 0  | 0  | -1  | 0 |
2
[p,i2,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i2,p,i1] and p = (0, unsure)
[i2,o1,s]
1
{(1|->o1),(2|->s)}
| 0  | 0  | -1  | 0 |
2
{(1|->i2),(2|->s)}
| 1  | 1  | -1  | 1 |
global decision for [i2,o1,s] and p = (0, unsure)
[i2,o1,p]
1
{(1|->o1),(2|->p)}
| 0  | 0  | -1  | 0 |
2
{(1|->i2),(2|->p)}
| 0  | 0  | -1  | 0 |
global decision for [i2,o1,p] and p = (0, unsure)
[i2,o1,s,p]
1
[o1,s,p]
| 0  | 0  | -1  | 0 |
2
[i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [i2,o1,s,p] and p = (0, unsure)
[i1,o2,s]
1
{(1|->i1),(2|->s)}
| 1  | 1  | -1  | 1 |
2
{(1|->o2),(2|->s)}
| 0  | 0  | -1  | 0 |
global decision for [i1,o2,s] and p = (0, unsure)
[i1,o2,p]
1
{(1|->i1),(2|->p)}
| 0  | 0  | -1  | 0 |
2
{(1|->o2),(2|->p)}
| 0  | 0  | -1  | 0 |
global decision for [i1,o2,p] and p = (0, unsure)
[i1,o2,s,p]
1
[i1,s,p]
| 0  | 0  | -1  | 0 |
2
[o2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [i1,o2,s,p] and p = (0, unsure)
[i1,s,o2]
1
{(1|->i1),(2|->s)}
| 1  | 1  | -1  | 1 |
2
{(1|->s),(2|->o2)}
| 1  | 0  | 1  | 0 |
global decision for [i1,s,o2] and p = (0, enabled)
[i1,s,p]
1
[i1,s,p]
| 0  | 0  | -1  | 0 |
2
{(1|->s),(2|->p)}
| 0  | 0  | -1  | 0 |
global decision for [i1,s,p] and p = (0, unsure)
[i2,s,o1]
1
{(1|->s),(2|->o1)}
| 1  | 0  | 1  | 0 |
2
{(1|->i2),(2|->s)}
| 1  | 1  | -1  | 1 |
global decision for [i2,s,o1] and p = (0, enabled)
[i2,s,p]
1
{(1|->s),(2|->p)}
| 0  | 0  | -1  | 0 |
2
[i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [i2,s,p] and p = (0, unsure)
[i1,s,o2,s]
1
[i1,s,s]
| 0  | 0  | -1  | 0 |
2
[s,o2,s]
| 0  | 0  | -1  | 0 |
global decision for [i1,s,o2,s] and p = (0, unsure)
[i1,s,o2,p]
1
[i1,s,p]
| 0  | 0  | -1  | 0 |
2
[s,o2,p]
| 0  | 0  | -1  | 0 |
global decision for [i1,s,o2,p] and p = (0, unsure)
[i1,s,p,i2]
1
[i1,s,p]
| 0  | 0  | -1  | 0 |
2
[s,p,i2]
| 0  | 0  | -1  | 0 |
global decision for [i1,s,p,i2] and p = (0, unsure)
[i2,s,o1,s]
1
[s,o1,s]
| 0  | 0  | -1  | 0 |
2
[i2,s,s]
| 0  | 0  | -1  | 0 |
global decision for [i2,s,o1,s] and p = (0, unsure)
[i2,s,o1,p]
1
[s,o1,p]
| 0  | 0  | -1  | 0 |
2
[i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [i2,s,o1,p] and p = (0, unsure)
[i2,s,p,i1]
1
[s,p,i1]
| 0  | 0  | -1  | 0 |
2
[i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [i2,s,p,i1] and p = (0, unsure)
[i1,p,i2]
1
{(1|->i1),(2|->p)}
| 0  | 0  | -1  | 0 |
2
{(1|->p),(2|->i2)}
| 1  | 1  | -1  | 1 |
global decision for [i1,p,i2] and p = (0, unsure)
[i2,p,i1]
1
{(1|->p),(2|->i1)}
| 1  | 1  | -1  | 1 |
2
{(1|->i2),(2|->p)}
| 0  | 0  | -1  | 0 |
global decision for [i2,p,i1] and p = (0, unsure)
[i2,s,o1,s,p]
1
[s,o1,s,p]
| 0  | 0  | -1  | 0 |
2
[i2,s,s,p]
| 0  | 0  | -1  | 0 |
global decision for [i2,s,o1,s,p] and p = (0, unsure)
[s,i2,o1,s,p]
1
[s,o1,s,p]
| 0  | 0  | -1  | 0 |
2
[s,i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,o1,s,p] and p = (0, unsure)
[p,i2,o1,s,p]
1
[p,o1,s,p]
| 0  | 0  | -1  | 0 |
2
[p,i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i2,o1,s,p] and p = (0, unsure)
[i1,s,o2,s,p]
1
[i1,s,s,p]
| 0  | 0  | -1  | 0 |
2
[s,o2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [i1,s,o2,s,p] and p = (0, unsure)
[s,i1,o2,s,p]
1
[s,i1,s,p]
| 0  | 0  | -1  | 0 |
2
[s,o2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i1,o2,s,p] and p = (0, unsure)
[p,i1,o2,s,p]
1
[p,i1,s,p]
| 0  | 0  | -1  | 0 |
2
[p,o2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i1,o2,s,p] and p = (0, unsure)
[s,i1,s,o2,s]
1
[s,i1,s,s]
| 0  | 0  | -1  | 0 |
2
[s,s,o2,s]
| 0  | 0  | -1  | 0 |
global decision for [s,i1,s,o2,s] and p = (0, unsure)
[s,i1,s,o2,p]
1
[s,i1,s,p]
| 0  | 0  | -1  | 0 |
2
[s,s,o2,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i1,s,o2,p] and p = (0, unsure)
[s,i1,s,p,i2]
1
[s,i1,s,p]
| 0  | 0  | -1  | 0 |
2
[s,s,p,i2]
| 0  | 0  | -1  | 0 |
global decision for [s,i1,s,p,i2] and p = (0, unsure)
[s,i1,s,o2,s,p]
1
[s,i1,s,s,p]
| 0  | 0  | -1  | 0 |
2
[s,s,o2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i1,s,o2,s,p] and p = (0, unsure)
[s,i2,s,o1,s]
1
[s,s,o1,s]
| 0  | 0  | -1  | 0 |
2
[s,i2,s,s]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,s,o1,s] and p = (0, unsure)
[s,i2,s,o1,p]
1
[s,s,o1,p]
| 0  | 0  | -1  | 0 |
2
[s,i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,s,o1,p] and p = (0, unsure)
[s,i2,s,p,i1]
1
[s,s,p,i1]
| 0  | 0  | -1  | 0 |
2
[s,i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,s,p,i1] and p = (0, unsure)
[s,i2,s,o1,s,p]
1
[s,s,o1,s,p]
| 0  | 0  | -1  | 0 |
2
[s,i2,s,s,p]
| 0  | 0  | -1  | 0 |
global decision for [s,i2,s,o1,s,p] and p = (0, unsure)
[p,i1,s,o2,s]
1
[p,i1,s,s]
| 0  | 0  | -1  | 0 |
2
[p,s,o2,s]
| 0  | 0  | -1  | 0 |
global decision for [p,i1,s,o2,s] and p = (0, unsure)
[p,i1,s,o2,p]
1
[p,i1,s,p]
| 0  | 0  | -1  | 0 |
2
[p,s,o2,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i1,s,o2,p] and p = (0, unsure)
[p,i1,s,p,i2]
1
[p,i1,s,p]
| 0  | 0  | -1  | 0 |
2
[p,s,p,i2]
| 0  | 0  | -1  | 0 |
global decision for [p,i1,s,p,i2] and p = (0, unsure)
[p,i1,s,o2,s,p]
1
[p,i1,s,s,p]
| 0  | 0  | -1  | 0 |
2
[p,s,o2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i1,s,o2,s,p] and p = (0, unsure)
[p,i2,s,o1,s]
1
[p,s,o1,s]
| 0  | 0  | -1  | 0 |
2
[p,i2,s,s]
| 0  | 0  | -1  | 0 |
global decision for [p,i2,s,o1,s] and p = (0, unsure)
[p,i2,s,o1,p]
1
[p,s,o1,p]
| 0  | 0  | -1  | 0 |
2
[p,i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i2,s,o1,p] and p = (0, unsure)
[p,i2,s,p,i1]
1
[p,s,p,i1]
| 0  | 0  | -1  | 0 |
2
[p,i2,s,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i2,s,p,i1] and p = (0, unsure)
[p,i2,s,o1,s,p]
1
[p,s,o1,s,p]
| 0  | 0  | -1  | 0 |
2
[p,i2,s,s,p]
| 0  | 0  | -1  | 0 |
global decision for [p,i2,s,o1,s,p] and p = (0, unsure)

ALL OPERATIONS COVERED

perform_random_step(1,4)
random(2,1)
finished_random_animate(4,[runtime/4920,total_runtime/4970,walltime/5540])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 5840 ms walltime (5210 ms runtime), since start: 7770 ms

! *** warning occurred ***
! b_interpreter
! *** Occurences of this warning: 
! 2
% Finished process_file (errors=0, warnings=1)
% ProB Finished
! Total Errors: 0, Warnings:1
*/