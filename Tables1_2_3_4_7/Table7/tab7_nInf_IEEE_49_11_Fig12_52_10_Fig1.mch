// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL tab7_nInf_IEEE_49_11_Fig12_52_10_Fig1

SEES
 //---------------------------------------------------------------------
 // Machine for input data
 //---------------------------------------------------------------------
   IEEE_49_11_H_Fig12/*@file "../../DataExamples/IEEE_49_11_H_Fig12.mch" */
 //IEEE_52_10_Ex1_H_Fig1 /*@file "../../DataExamples/IEEE_52_10_Ex1_H_Fig1.mch" */

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
 T_G = GetTraces(G_fb,bound) & L_G = GetStrings(G_fb,bound) &
 T_H = GetTraces(H_lb,bound) & L_H = GetStrings(H_lb,bound) &
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
OUTPUT (IEEE_49_11_H_Fig12.mch):
% Runtime for SOLUTION for SETUP_CONSTANTS: 190 ms (walltime: 190 ms)
% Finished searching for valid initial states 220 ms walltime (210 ms runtime), since start: 1410 ms
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
| 3  | 2  | 1  | 2 |
2
{}
| 3  | 2  | 1  | 2 |
global decision for {} and c = (2, enabled)
{(1|->a)}
1
{(1|->a)}
| 1  | 2  | 0  | 1 |
2
{}
| 3  | 2  | 1  | 2 |
global decision for {(1|->a)} and c = (1, disabled)
{(1|->a),(2|->bp)}
1
{(1|->a)}
| 1  | 2  | 0  | 1 |
2
{(1|->bp)}
| 1  | 0  | 1  | 0 |
global decision for {(1|->a),(2|->bp)} and c = (0, enabled)
{(1|->a),(2|->c)}
1
{(1|->a),(2|->c)}
| 0  | 0  | -1  | 0 |
2
{(1|->c)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->a),(2|->c)} and c = (0, unsure)
{(1|->b)}
1
{}
| 3  | 2  | 1  | 2 |
2
{(1|->b)}
| 1  | 2  | 0  | 1 |
global decision for {(1|->b)} and c = (1, disabled)
{(1|->b),(2|->ap)}
1
{(1|->ap)}
| 1  | 0  | 1  | 0 |
2
{(1|->b)}
| 1  | 2  | 0  | 1 |
global decision for {(1|->b),(2|->ap)} and c = (0, enabled)
{(1|->b),(2|->c)}
1
{(1|->c)}
| 0  | 0  | -1  | 0 |
2
{(1|->b),(2|->c)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->b),(2|->c)} and c = (0, unsure)
{(1|->c)}
1
{(1|->c)}
| 0  | 0  | -1  | 0 |
2
{(1|->c)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->c)} and c = (0, unsure)
{(1|->c),(2|->d)}
1
{(1|->c),(2|->d)}
| 0  | 0  | -1  | 0 |
2
{(1|->c),(2|->d)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->c),(2|->d)} and c = (0, unsure)
[b,ap,c]
1
{(1|->ap),(2|->c)}
| 0  | 0  | -1  | 0 |
2
{(1|->b),(2|->c)}
| 0  | 0  | -1  | 0 |
global decision for [b,ap,c] and c = (0, unsure)
[a,bp,c]
1
{(1|->a),(2|->c)}
| 0  | 0  | -1  | 0 |
2
{(1|->bp),(2|->c)}
| 0  | 0  | -1  | 0 |
global decision for [a,bp,c] and c = (0, unsure)
[c,d,a]
1
[c,d,a]
| 0  | 0  | -1  | 0 |
2
{(1|->c),(2|->d)}
| 0  | 0  | -1  | 0 |
global decision for [c,d,a] and c = (0, unsure)
[c,d,b]
1
{(1|->c),(2|->d)}
| 0  | 0  | -1  | 0 |
2
[c,d,b]
| 0  | 0  | -1  | 0 |
global decision for [c,d,b] and c = (0, unsure)
[c,d,d]
1
[c,d,d]
| 0  | 0  | -1  | 0 |
2
[c,d,d]
| 0  | 0  | -1  | 0 |
global decision for [c,d,d] and c = (0, unsure)
[c,d,a,bp]
1
[c,d,a]
| 0  | 0  | -1  | 0 |
2
[c,d,bp]
| 0  | 0  | -1  | 0 |
global decision for [c,d,a,bp] and c = (0, unsure)
[c,d,a,d]
1
[c,d,a,d]
| 0  | 0  | -1  | 0 |
2
[c,d,d]
| 0  | 0  | -1  | 0 |
global decision for [c,d,a,d] and c = (0, unsure)
[c,d,b,ap]
1
[c,d,ap]
| 0  | 0  | -1  | 0 |
2
[c,d,b]
| 0  | 0  | -1  | 0 |
global decision for [c,d,b,ap] and c = (0, unsure)
[c,d,b,d]
1
[c,d,d]
| 0  | 0  | -1  | 0 |
2
[c,d,b,d]
| 0  | 0  | -1  | 0 |
global decision for [c,d,b,d] and c = (0, unsure)
[c,d,a,bp,d]
1
[c,d,a,d]
| 0  | 0  | -1  | 0 |
2
[c,d,bp,d]
| 0  | 0  | -1  | 0 |
global decision for [c,d,a,bp,d] and c = (0, unsure)
[c,d,b,ap,d]
1
[c,d,ap,d]
| 0  | 0  | -1  | 0 |
2
[c,d,b,d]
| 0  | 0  | -1  | 0 |
global decision for [c,d,b,ap,d] and c = (0, unsure)
{}
1
{}
| 0  | 0  | -1  | 0 |
2
{}
| 0  | 0  | -1  | 0 |
global decision for {} and d = (0, unsure)
{(1|->a)}
1
{(1|->a)}
| 0  | 0  | -1  | 0 |
2
{}
| 0  | 0  | -1  | 0 |
global decision for {(1|->a)} and d = (0, unsure)
{(1|->a),(2|->bp)}
1
{(1|->a)}
| 0  | 0  | -1  | 0 |
2
{(1|->bp)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->a),(2|->bp)} and d = (0, unsure)
{(1|->a),(2|->c)}
1
{(1|->a),(2|->c)}
| 0  | 0  | -1  | 0 |
2
{(1|->c)}
| 1  | 0  | 1  | 0 |
global decision for {(1|->a),(2|->c)} and d = (0, unsure)
{(1|->b)}
1
{}
| 0  | 0  | -1  | 0 |
2
{(1|->b)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->b)} and d = (0, unsure)
{(1|->b),(2|->ap)}
1
{(1|->ap)}
| 0  | 0  | -1  | 0 |
2
{(1|->b)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->b),(2|->ap)} and d = (0, unsure)
{(1|->b),(2|->c)}
1
{(1|->c)}
| 1  | 0  | 1  | 0 |
2
{(1|->b),(2|->c)}
| 0  | 0  | -1  | 0 |
global decision for {(1|->b),(2|->c)} and d = (0, unsure)
{(1|->c)}
1
{(1|->c)}
| 1  | 0  | 1  | 0 |
2
{(1|->c)}
| 1  | 0  | 1  | 0 |
global decision for {(1|->c)} and d = (0, enabled)
{(1|->c),(2|->d)}
1
{(1|->c),(2|->d)}
| 2  | 3  | 0  | 2 |
2
{(1|->c),(2|->d)}
| 2  | 3  | 0  | 2 |
global decision for {(1|->c),(2|->d)} and d = (2, disabled)
[b,ap,c]
1
{(1|->ap),(2|->c)}
| 0  | 0  | -1  | 0 |
2
{(1|->b),(2|->c)}
| 0  | 0  | -1  | 0 |
global decision for [b,ap,c] and d = (0, unsure)
[a,bp,c]
1
{(1|->a),(2|->c)}
| 0  | 0  | -1  | 0 |
2
{(1|->bp),(2|->c)}
| 0  | 0  | -1  | 0 |
global decision for [a,bp,c] and d = (0, unsure)
[c,d,a]
1
[c,d,a]
| 2  | 1  | 1  | 1 |
2
{(1|->c),(2|->d)}
| 2  | 3  | 0  | 2 |
global decision for [c,d,a] and d = (1, enabled)
[c,d,b]
1
{(1|->c),(2|->d)}
| 2  | 3  | 0  | 2 |
2
[c,d,b]
| 2  | 1  | 1  | 1 |
global decision for [c,d,b] and d = (1, enabled)
[c,d,d]
1
[c,d,d]
| 0  | 0  | -1  | 0 |
2
[c,d,d]
| 0  | 0  | -1  | 0 |
global decision for [c,d,d] and d = (0, unsure)
[c,d,a,bp]
1
[c,d,a]
| 2  | 1  | 1  | 1 |
2
[c,d,bp]
| 0  | 1  | 0  | 0 |
global decision for [c,d,a,bp] and d = (0, disabled)
[c,d,a,d]
1
[c,d,a,d]
| 0  | 0  | -1  | 0 |
2
[c,d,d]
| 0  | 0  | -1  | 0 |
global decision for [c,d,a,d] and d = (0, unsure)
[c,d,b,ap]
1
[c,d,ap]
| 0  | 1  | 0  | 0 |
2
[c,d,b]
| 2  | 1  | 1  | 1 |
global decision for [c,d,b,ap] and d = (0, disabled)
[c,d,b,d]
1
[c,d,d]
| 0  | 0  | -1  | 0 |
2
[c,d,b,d]
| 0  | 0  | -1  | 0 |
global decision for [c,d,b,d] and d = (0, unsure)
[c,d,a,bp,d]
1
[c,d,a,d]
| 0  | 0  | -1  | 0 |
2
[c,d,bp,d]
| 0  | 0  | -1  | 0 |
global decision for [c,d,a,bp,d] and d = (0, unsure)
[c,d,b,ap,d]
1
[c,d,ap,d]
| 0  | 0  | -1  | 0 |
2
[c,d,b,d]
| 0  | 0  | -1  | 0 |
global decision for [c,d,b,ap,d] and d = (0, unsure)

ALL OPERATIONS COVERED

perform_random_step(1,4)
random(2,1)
finished_random_animate(4,[runtime/840,total_runtime/840,walltime/910])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 1190 ms walltime (1110 ms runtime), since start: 2330 ms

! *** warning occurred ***
! b_interpreter
! *** Occurences of this warning: 
! 2
% Finished process_file (errors=0, warnings=1)
% ProB Finished
! Total Errors: 0, Warnings:1


----------------------------------------


OUTPUT (IEEE_52_10_Ex1_H_Fig1.mch):
% Runtime for SOLUTION for SETUP_CONSTANTS: 190 ms (walltime: 190 ms)
% Finished searching for valid initial states 210 ms walltime (210 ms runtime), since start: 2170 ms
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
| 3  | 2  | 1  | 2 |
2
{}
| 3  | 2  | 1  | 2 |
global decision for {} and c = (2, enabled)
{(1|->a)}
1
{(1|->a)}
| 1  | 2  | 0  | 1 |
2
{}
| 3  | 2  | 1  | 2 |
global decision for {(1|->a)} and c = (1, disabled)
{(1|->a),(2|->c)}
1
{(1|->a)}
| 1  | 2  | 0  | 1 |
2
{}
| 3  | 2  | 1  | 2 |
global decision for {(1|->a),(2|->c)} and c = (1, disabled)
{(1|->a),(2|->bp)}
1
{(1|->a)}
| 1  | 2  | 0  | 1 |
2
{(1|->bp)}
| 1  | 0  | 1  | 0 |
global decision for {(1|->a),(2|->bp)} and c = (0, enabled)
{(1|->b)}
1
{}
| 3  | 2  | 1  | 2 |
2
{(1|->b)}
| 1  | 2  | 0  | 1 |
global decision for {(1|->b)} and c = (1, disabled)
{(1|->b),(2|->c)}
1
{}
| 3  | 2  | 1  | 2 |
2
{(1|->b)}
| 1  | 2  | 0  | 1 |
global decision for {(1|->b),(2|->c)} and c = (1, disabled)
{(1|->b),(2|->ap)}
1
{(1|->ap)}
| 1  | 0  | 1  | 0 |
2
{(1|->b)}
| 1  | 2  | 0  | 1 |
global decision for {(1|->b),(2|->ap)} and c = (0, enabled)
{(1|->c)}
1
{}
| 3  | 2  | 1  | 2 |
2
{}
| 3  | 2  | 1  | 2 |
global decision for {(1|->c)} and c = (2, enabled)
{(1|->d)}
1
{(1|->d)}
| 2  | 3  | 0  | 2 |
2
{(1|->d)}
| 2  | 3  | 0  | 2 |
global decision for {(1|->d)} and c = (2, disabled)
{(1|->d),(2|->a)}
1
{(1|->d),(2|->a)}
| 2  | 1  | 1  | 1 |
2
{(1|->d)}
| 2  | 3  | 0  | 2 |
global decision for {(1|->d),(2|->a)} and c = (1, enabled)
{(1|->d),(2|->b)}
1
{(1|->d)}
| 2  | 3  | 0  | 2 |
2
{(1|->d),(2|->b)}
| 2  | 1  | 1  | 1 |
global decision for {(1|->d),(2|->b)} and c = (1, enabled)
{(1|->d),(2|->c)}
1
{(1|->d)}
| 2  | 3  | 0  | 2 |
2
{(1|->d)}
| 2  | 3  | 0  | 2 |
global decision for {(1|->d),(2|->c)} and c = (2, disabled)
[d,a,c]
1
{(1|->d),(2|->a)}
| 2  | 1  | 1  | 1 |
2
{(1|->d)}
| 2  | 3  | 0  | 2 |
global decision for [d,a,c] and c = (1, enabled)
[d,a,bp]
1
{(1|->d),(2|->a)}
| 2  | 1  | 1  | 1 |
2
{(1|->d),(2|->bp)}
| 0  | 1  | 0  | 0 |
global decision for [d,a,bp] and c = (0, disabled)
[d,a,bp,c]
1
{(1|->d),(2|->a)}
| 2  | 1  | 1  | 1 |
2
{(1|->d),(2|->bp)}
| 0  | 1  | 0  | 0 |
global decision for [d,a,bp,c] and c = (0, disabled)
[d,b,c]
1
{(1|->d)}
| 2  | 3  | 0  | 2 |
2
{(1|->d),(2|->b)}
| 2  | 1  | 1  | 1 |
global decision for [d,b,c] and c = (1, enabled)
[d,b,ap]
1
{(1|->d),(2|->ap)}
| 0  | 1  | 0  | 0 |
2
{(1|->d),(2|->b)}
| 2  | 1  | 1  | 1 |
global decision for [d,b,ap] and c = (0, disabled)
[d,b,ap,c]
1
{(1|->d),(2|->ap)}
| 0  | 1  | 0  | 0 |
2
{(1|->d),(2|->b)}
| 2  | 1  | 1  | 1 |
global decision for [d,b,ap,c] and c = (0, disabled)
[b,ap,c]
1
{(1|->ap)}
| 1  | 0  | 1  | 0 |
2
{(1|->b)}
| 1  | 2  | 0  | 1 |
global decision for [b,ap,c] and c = (0, enabled)
[a,bp,c]
1
{(1|->a)}
| 1  | 2  | 0  | 1 |
2
{(1|->bp)}
| 1  | 0  | 1  | 0 |
global decision for [a,bp,c] and c = (0, enabled)

ALL OPERATIONS COVERED

perform_random_step(1,4)
random(2,1)
finished_random_animate(4,[runtime/460,total_runtime/460,walltime/490])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 780 ms walltime (740 ms runtime), since start: 2680 ms

! *** warning occurred ***
! b_interpreter
! *** Occurences of this warning: 
! 2
% Finished process_file (errors=0, warnings=1)
% ProB Finished
! Total Errors: 0, Warnings:1
*/