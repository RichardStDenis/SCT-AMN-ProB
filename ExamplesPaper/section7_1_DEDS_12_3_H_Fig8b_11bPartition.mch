// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL section7_1_DEDS_12_3_H_Figs8b_11bPartition

SEES
 //---------------------------------------------------------------------
 // Machine for input data
 //---------------------------------------------------------------------
 // There is a partition such that K is coobservable w.r.t. the general
 // architecture, i.e., E_ce = {delta} et E_cd = {gamma}.
 // Local decisions, first supervisor:
 //  (({(1|->alpha)}|->gamma)|->"or") the 2nd supervisor does not observe alpha
 //  (({(1|->gamma)}|->delta)|->"and") the 1st supervisor does not observe beta and
 //    1) the 2nd supervisor observe [beta,gamma] and enables delta (four lines below) or
 //    2) the 2nd supervisor observe [beta,gamma,beta] and disables delta
 // Local decisions, second supervisor:
 //  (({(1|->beta)}|->gamma)|->"or") the 1st supervisor does not observe beta
 //  (({(1|->beta),(2|->gamma)}|->delta)|->"and")
 //    1) the 1st supervisor observe [gamma] and enables delta or
 //    2) the 1st supervisor observe [gamma,alpha] and disables delta
   DEDS_12_3_H_Fig8b /*@file "../DataExamples/DEDS_12_3_H_Fig8b.mch" */
 // No partition such that K is coobservable w.r.t. the general architecture.
 //DEDS_12_3_H_Fig11b /*@file "../DataExamples/DEDS_12_3_H_Fig11b.mch" */

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
 "../Theories/SCT_core.def";
 "../Theories/SCT_decentralized.def";
 "LibraryIO.def"

CONSTANTS
L_G, L_H, T_G, T_H, Lm_G, Lm_H    // Languages for free and legal behaviors

PROPERTIES
 G_fb : Generator_C_O(G_fb'states,G_fb'events) &
 H_lb : FSA(H_lb'states,H_lb'events) &
 T_G = GetTraces(G_fb, bound) &
 Lm_G = Traces2Tasks(T_G,G_fb'Xm) &
 L_G = pr(Lm_G) &
 T_H = GetTraces(H_lb,bound) &
 Lm_H = Traces2Tasks(T_H,H_lb'Xm) &
 L_H = pr(Lm_H)

VARIABLES
 i
INITIALISATION
 i := 1
INVARIANT
 i : 1..A_d'NoSupervisors+1

OPERATIONS
 FindPartitionCoobservability =
  ANY Ece, Ecd WHERE
   Ece /\ Ecd = {} & Ece \/ Ecd = G_fb'ctrlEvents &
   Coobservability(A_d,Ece,Ecd,L_G,L_H) THEN
    PRINT("K is coobservable.") || PRINT(Ece) || PRINT(Ecd) END;

 FindPartitionCoobservability_ =
  LET Eu BE Eu = G_fb'events-G_fb'ctrlEvents IN
   PRE Lm_closed_(L_H,Lm_H,Lm_G) &
       Controllability_(L_H,L_G,Eu) THEN
    PRINT("K is Lm(G)-closed."); PRINT("K is controllable.");
    ANY Ece, Ecd WHERE Ece /\ Ecd = {} & Ece \/ Ecd = G_fb'ctrlEvents &
     Coobservability_(A_d,Ece,Ecd,L_G,L_H) THEN
      PRINT("K is coobservable."); PRINT(Ece); PRINT(Ecd);
      LET S_P_i BE S_P_i = LocalDecisionsG(A_d,Eu,L_G,L_H) IN
       PRINTF("Local decisions for all supervisors: ~n ~w~n",[S_P_i]);
       PRINTF("Global decisions: ~n ~w~n",
        [GlobalDecisionsG(A_d,Eu,L_H,S_P_i)]) END END END END END