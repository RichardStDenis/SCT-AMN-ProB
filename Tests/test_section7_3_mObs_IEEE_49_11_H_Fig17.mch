// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL test_section7_3_mObs_IEEE_49_11_H_Fig17

SEES
 //---------------------------------------------------------------------
 // Machine for input data
 //---------------------------------------------------------------------
 IEEE_49_11_H_Fig17 /*@file "../DataExamples/IEEE_49_11_H_Fig17.mch" */

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
 "../Theories/SCT_core.def";
 "../Theories/SCT_decentralized.def";
 "LibraryIO.def"

CONSTANTS
 T_G, L_G, T_H, L_H,    // Languages for free and legal behaviors
 max_m                  // Parameter of the m-co-observability property

PROPERTIES
 G_fb : Generator_C_O(G_fb'states,G_fb'events) &
 H_lb : FSA(H_lb'states,H_lb'events) &
 T_G = GetTracesNc1stNqcSev(G_fb,bound,{p,s}) &
 L_G = Traces2Strings(T_G) &
 T_H = GetTracesNc1stNqcSev(H_lb,bound,{p,s}) &
 L_H = Traces2Strings(T_H) &
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
    PRINTF("Event: ~w~n",[e_]);
    ANY PE WHERE
     PE <: POW(E_(L_H,e_)) &
     card(PE) >= 1 & card(PE) <= max_m &
     !(S1,S2).(S1 : PE & S2 : PE & S1 /= S2 => S1 /\ S2 = {}) &
     UNION(S_).(S_ : PE | S_) = E_(L_H,e_) &
     CPmCoobservability_e(A_d,e_,L_H,D_(L_H,L_G,e_),PE) THEN
      PRINT("K is C&P3-coobservable.");
      PRINT(p |-> PE);
      allPE_ce :=  allPE_ce \/ {e_|->(PE-{{}})} END;
    Ece := Ece - {e_}
   INVARIANT Ece <: A_d'Events_ce VARIANT card(Ece) END;

   Ecd := A_d'Events_cd;
   WHILE Ecd /= {} DO
    e_ :: Ecd;
    PRINTF("Event: ~w~n",[e_]);
    ANY PE WHERE
     PE <: POW(D_(L_H,L_G,e_)) &
     card(PE) >= 1 & card(PE) <= max_m &
     !(S1,S2).(S1 : PE & S2 : PE & S1 /= S2 => S1 /\ S2 = {}) &
     UNION(S_).(S_ : PE | S_) = D_(L_H,L_G,e_) &
     DAmCoobservability_e(A_d,e_,L_H,E_(L_H,e_),PE) THEN
      PRINT("K is D&A3-coobservable.");
      PRINT(s |-> PE);
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
      E_e, D_e,        // Partitions for event e
      E_e_j, D_e_j,    // j-th element of partitions for event e
      j IN 
   Ece := A_d'Events_ce;
   WHILE Ece /= {} DO
    e_ :: Ece;
    PRINTF("Event: ~w~n",[e_]);
    E_e := allPE_ce(e_);
    PRINTF("Partition: ~w~n",[E_e]);
    j := 1;
    WHILE j <= card(allPE_ce(e_)) DO
     PRINTF("E~w_e: ",[j]);
     E_e_j :: E_e;
     PRINTF("~w~n",[E_e_j]);
     Sup_j_i := Sup_j_i \/ MicroDecisionsCP(e_,j,A_d,L_H,L_G,E_e_j);
     //PRINTF("~w~n",[Sup_j_i]);
     E_e := E_e - {E_e_j};
     j := j+1
    INVARIANT j > 0 VARIANT m-j END;
    Ece := Ece - {e_}
   INVARIANT Ece <: A_d'Events_ce VARIANT card(Ece) END;

   Ecd := A_d'Events_cd;
   WHILE Ecd /= {} DO
    e_ :: Ecd;
    PRINTF("Event: ~w~n",[e_]);
    D_e := allPE_cd(e_);
    PRINTF("Partition: ~w~n",[D_e]);
    j := 1;
    WHILE j <= card(allPE_cd(e_)) DO
     PRINTF("D~w_e: ",[j]);
     D_e_j :: D_e;
     PRINTF("~w~n",[D_e_j]);
     Sup_j_i := Sup_j_i \/ MicroDecisionsDA(e_,j,A_d,L_H,L_G,D_e_j);
     //PRINTF("~w~n",[Sup_j_i]);
     D_e := D_e - {D_e_j};
     j := j+1
    INVARIANT j > 0 VARIANT m-j END;
    Ecd := Ecd - {e_}
   INVARIANT Ecd <: A_d'Events_cd VARIANT card(Ecd) END END;
  Sup_j := IntermediateDecisions(A_d,L_H,L_G,A_d'Events_ce,A_d'Events_cd,A_d'NoSupervisors,Sup_j_i);
  Sup_G := FinalDecisions(L_H,A_d'Events_ce,A_d'Events_cd,m,Sup_j);
  PRINTF("Global decisions:~w~n",[Sup_G]);
  code := 2 END END

/*
OUTPUT:
Event: p
Partition: {{{}},
            {{(1|->i1),(2|->o2)},{(1|->i2),(2|->o1)},[p,i1,o2],[p,i1,s,o2],[p,i2,o1],
             [p,i2,s,o1],[i1,s,o2],[i2,s,o1]}}
E1_e: {{}}
E2_e: {{(1|->i1),(2|->o2)},{(1|->i2),(2|->o1)},[p,i1,o2],[p,i1,s,o2],[p,i2,o1],
       [p,i2,s,o1],[i1,s,o2],[i2,s,o1]}
Event: s
Partition: {{{},{(1|->p)}},
            {{(1|->i1),(2|->o2)},{(1|->i2),(2|->o1)},[p,i1,o2],[p,i1,s,o2],[p,i2,o1],
             [p,i2,s,o1],[i1,s,o2],[i2,s,o1]}}
D1_e: {{},{(1|->p)}}
D2_e: {{(1|->i1),(2|->o2)},{(1|->i2),(2|->o1)},[p,i1,o2],[p,i1,s,o2],[p,i2,o1],
       [p,i2,s,o1],[i1,s,o2],[i2,s,o1]}
Global decisions:{
(({}|->s)|->FALSE),
(({}|->p)|->TRUE),
(({(1|->i1)}|->s)|->TRUE),
(({(1|->i1)}|->p)|->FALSE),
(({(1|->i1),(2|->o2)}|->s)|->FALSE),
(({(1|->i1),(2|->o2)}|->p)|->TRUE),
(({(1|->i1),(2|->s)}|->p)|->FALSE),
(({(1|->i2)}|->s)|->TRUE),
(({(1|->i2)}|->p)|->FALSE),
(({(1|->i2),(2|->o1)}|->s)|->FALSE),
(({(1|->i2),(2|->o1)}|->p)|->TRUE),
(({(1|->i2),(2|->s)}|->p)|->FALSE),
(({(1|->p)}|->s)|->FALSE),
(({(1|->p),(2|->i1)}|->s)|->TRUE),
(({(1|->p),(2|->i1)}|->p)|->FALSE),
(({(1|->p),(2|->i2)}|->s)|->TRUE),
(({(1|->p),(2|->i2)}|->p)|->FALSE),
(([p,i1,o2]|->s)|->FALSE),
(([p,i1,o2]|->p)|->TRUE),
(([p,i1,s]|->p)|->FALSE),
(([p,i1,s,o2]|->s)|->FALSE),
(([p,i1,s,o2]|->p)|->TRUE),
(([p,i2,o1]|->s)|->FALSE),
(([p,i2,o1]|->p)|->TRUE),
(([p,i2,s]|->p)|->FALSE),
(([p,i2,s,o1]|->s)|->FALSE),
(([p,i2,s,o1]|->p)|->TRUE),
(([i1,s,o2]|->s)|->FALSE),
(([i1,s,o2]|->p)|->TRUE),
(([i2,s,o1]|->s)|->FALSE),
(([i2,s,o1]|->p)|->TRUE)
}
*/