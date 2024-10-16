// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL tab7_mObs_IEEE_49_11_Fig12_52_10_Fig1

SEES
 //---------------------------------------------------------------------
 // Machine for input data
 //---------------------------------------------------------------------
   IEEE_49_11_H_Fig12 /*@file "../../DataExamples/IEEE_49_11_H_Fig12.mch" */
 //IEEE_52_10_Ex1_H_Fig1 /*@file "../../DataExamples/IEEE_52_10_Ex1_H_Fig1.mch" */

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 10000000;
 SET_PREF_SMT == FALSE;
 SET_PREF_KODKOD == FALSE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../../Theories/SCT_core.def";
 "../../Theories/SCT_decentralized.def";
 "LibraryIO.def"

CONSTANTS
 L_G, L_H,    // Languages for free and legal behaviors
 max_m        // Parameter of the m-co-observability property

PROPERTIES
 G_fb : Generator_C_O(G_fb'states,G_fb'events) &
 H_lb : FSA(H_lb'states,H_lb'events) &
 L_G = GetStrings(G_fb,bound) &
 L_H = GetStrings(H_lb,bound) &
 max_m = 3

VARIABLES
 code,
 allPE_ce, allPE_cd,
 m,
 Sup_j_i, Sup_j, Sup_G

INITIALISATION
 code := 0;
 allPE_ce := {};  allPE_cd := {};
 m := 0;
 Sup_j_i := {}; Sup_j := {}; Sup_G := {}

INVARIANT
 (code = 0 or code = 1 or code = 2) &
 allPE_ce <: A_d'Events_ce * POW(POW(L_H)) &
 allPE_cd <: A_d'Events_cd * POW(POW(L_H)) &
 m >=0 & m <= max_m &
 Sup_j_i : def_Sup_j_i(seq(G_fb'events),G_fb'events) &
 Sup_j : def_S_j(L_H,G_fb'events) &
 Sup_G: def_Sup_G(L_H,G_fb'events)

OPERATIONS
 Verify_mCoobservability  = PRE code = 0 THEN
  VAR Ece, Ecd,
      e_ IN
   Ece := A_d'Events_ce;
   WHILE Ece /= {} DO
    e_ :: Ece;
    ANY PE WHERE
     PE <: POW(E_(L_H,e_)) &
     card(PE) >= 1 & card(PE) <= max_m &
     !(S1,S2).(S1 : PE & S2 : PE & S1 /= S2 => S1 /\ S2 = {}) &
     UNION(S_).(S_ : PE | S_) = E_(L_H,e_) &
     CPmCoobservability_e(A_d,e_,L_H,D_(L_H,L_G,e_),PE) THEN
      PRINT("K is C&P3-coobservable.");
      allPE_ce :=  allPE_ce \/ {e_|->(PE-{{}})} END;
    Ece := Ece - {e_}
   INVARIANT Ece <: A_d'Events_ce VARIANT card(Ece) END;

   Ecd := A_d'Events_cd;
   WHILE Ecd /= {} DO
    e_ :: Ecd;
    ANY PE WHERE
     PE <: POW(D_(L_H,L_G,e_)) &
     card(PE) >= 1 & card(PE) <= max_m &
     !(S1,S2).(S1 : PE & S2 : PE & S1 /= S2 => S1 /\ S2 = {}) &
     UNION(S_).(S_ : PE | S_) = D_(L_H,L_G,e_) &
     DAmCoobservability_e(A_d,e_,L_H,E_(L_H,e_),PE) THEN
      PRINT("K is D&A3-coobservable.");
      allPE_cd :=  allPE_cd \/ {e_|->(PE-{{}})} END;
    Ecd := Ecd - {e_}
   INVARIANT Ecd <: A_d'Events_cd VARIANT card(Ecd) END END;
  m := max(UNION(e_).(e_ : A_d'Events_ce | {card(allPE_ce(e_))}) \/
           UNION(e_).(e_ : A_d'Events_cd | {card(allPE_cd(e_))}));
  PRINTF("K is ~w-coobservable.~n ",[m]);
  code := 1 END;

 ComputeDecisions = PRE code = 1 THEN
  VAR Ece, Ecd,
      e_,
      E_e, D_e,         // Partitions for event e
      E_e_j, D_e_j,     // j-th element of partitions for event e
      j IN 
   Ece := A_d'Events_ce;
   WHILE Ece /= {} DO
    e_ :: Ece;
    E_e := allPE_ce(e_);
    j := 1;
    WHILE j <= card(allPE_ce(e_)) DO
     E_e_j :: E_e;
     Sup_j_i := Sup_j_i \/ MicroDecisionsCP(e_,j,A_d,L_H,L_G,E_e_j);
     E_e := E_e - {E_e_j};
     j := j+1
    INVARIANT j > 0 VARIANT m-j END;
    Ece := Ece - {e_}
   INVARIANT Ece <: A_d'Events_ce VARIANT card(Ece) END;

   Ecd := A_d'Events_cd;
   WHILE Ecd /= {} DO
    e_ :: Ecd;
    D_e := allPE_cd(e_);
    j := 1;
    WHILE j <= card(allPE_cd(e_)) DO
     D_e_j :: D_e;
     Sup_j_i := Sup_j_i \/ MicroDecisionsDA(e_,j,A_d,L_H,L_G,D_e_j);
     D_e := D_e - {D_e_j};
     j := j+1
    INVARIANT j > 0 VARIANT m-j END;
    Ecd := Ecd - {e_}
   INVARIANT Ecd <: A_d'Events_cd VARIANT card(Ecd) END END;
  Sup_j := IntermediateDecisions(A_d,L_H,L_G,A_d'Events_ce,A_d'Events_cd,A_d'NoSupervisors,Sup_j_i);
  Sup_G := FinalDecisions(L_H,A_d'Events_ce,A_d'Events_cd,m,Sup_j);
  PRINTF("Global decisions: ~w~n",[Sup_G]);
  code := 2 END END

/*
OUTPUT (IEEE_49_11_H_Fig12.mch):
% Finished searching for valid initial states 170 ms walltime (160 ms runtime), since start: 1360 ms
% Starting random_animate (max_steps=4)
perform_random_step(1,1)
random(2,1)
perform_random_step(1,2)
random(2,1)
"K is C&P3-coobservable."
"K is D&A3-coobservable."
K is 2-coobservable.
 perform_random_step(1,3)
random(2,1)
Global decisions: {
(({}|->c)|->TRUE),
(({(1|->a)}|->c)|->FALSE),
(({(1|->a),(2|->bp)}|->c)|->TRUE),
(({(1|->b)}|->c)|->FALSE),
(({(1|->b),(2|->ap)}|->c)|->TRUE),
(({(1|->c)}|->d)|->TRUE),
(({(1|->c),(2|->d)}|->d)|->FALSE),
(([c,d,a]|->d)|->TRUE),
(([c,d,b]|->d)|->TRUE),
(([c,d,a,bp]|->d)|->FALSE),
(([c,d,b,ap]|->d)|->FALSE) }

ALL OPERATIONS COVERED

perform_random_step(1,4)
random(2,1)
finished_random_animate(4,[runtime/1270,total_runtime/1270,walltime/1400])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 1630 ms walltime (1490 ms runtime), since start: 2760 ms

% Finished process_file (errors=0, warnings=0)
% ProB Finished


----------------------------------------

OUTPUT (IEEE_52_10_Ex1_H_Fig1.mch):
% Runtime for SOLUTION for SETUP_CONSTANTS: 150 ms (walltime: 160 ms)
% Finished searching for valid initial states 170 ms walltime (170 ms runtime), since start: 2160 ms
% Starting random_animate (max_steps=4)
perform_random_step(1,1)
random(2,1)
perform_random_step(1,2)
random(2,1)
"K is C&P3-coobservable."
K is 2-coobservable.
 perform_random_step(1,3)
random(2,1)
Global decisions: {
(({}|->c)|->TRUE),(({(1|->a)}|->c)|->FALSE),
(({(1|->a),(2|->bp)}|->c)|->TRUE),(({(1|->b)}|->c)|->FALSE),
(({(1|->b),(2|->ap)}|->c)|->TRUE),(({(1|->d)}|->c)|->FALSE),
(({(1|->d),(2|->a)}|->c)|->TRUE),(({(1|->d),(2|->b)}|->c)|->TRUE),
(([d,a,bp]|->c)|->FALSE),(([d,b,ap]|->c)|->FALSE)}

ALL OPERATIONS COVERED

perform_random_step(1,4)
random(2,1)
finished_random_animate(4,[runtime/760,total_runtime/760,walltime/820])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 1050 ms walltime (990 ms runtime), since start: 2980 ms

% Finished process_file (errors=0, warnings=0)
% ProB Finished
*/