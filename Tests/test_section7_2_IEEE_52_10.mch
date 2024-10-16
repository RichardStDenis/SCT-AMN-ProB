// Richard St-Denis, Universite de Sherbrooke, 2024.
MACHINE test_section7_2_IEEE_52_10

SEES
//---------------------------------------------------------------------
// Machine for input data
//---------------------------------------------------------------------
  IEEE_52_10_Ex1_H_Fig1 /*@file "../DataExamples/IEEE_52_10_Ex1_H_Fig1.mch" */

// N-inference observability.

DEFINITIONS
//---------------------------------------------------------------------
// ProB settings
//---------------------------------------------------------------------
  SET_PREF_MININT == 0;
  SET_PREF_MAXINT == 12;
  SET_PREF_MAX_INITIALISATIONS == 1;
  SET_PREF_MAX_OPERATIONS == 1;
  SET_PREF_TIME_OUT == 1000000;
  SET_PREF_SOLVER_STRENGTH == 1000;
  SET_PREF_SMT == TRUE;
  SET_PREF_KODKOD == FALSE;
  SET_PREF_ALLOW_INCOMPLETE_SETUP_CONSTANTS == TRUE;
  SET_TRACE_INFO == FALSE;
//---------------------------------------------------------------------
// Import the theory
//---------------------------------------------------------------------
 "../Theories/SCT_core.def";
 "../Theories/SCT_decentralized.def";
 "LibraryIO.def"

ABSTRACT_CONSTANTS Mi_, GetLangPair_, GetLangPairs_

CONSTANTS
  N,                         // Main parameter for N-inferring
  T_G, T_H, L_G, L_H,        // Languages for free and legal behaviors
  M1L_G, M2L_G,              // Observed languages
  D_0, D_1, D_2, D_3, D_4, E_0, E_1, E_2, E_3, E_4,
  allLangPairs

PROPERTIES
 G_fb : Generator_C_O(G_fb'states,G_fb'events) &
 H_lb : FSA(H_lb'states,H_lb'events) &
 T_G = GetTraces(G_fb,bound) & L_G = GetStrings(G_fb,bound) &
 T_H = GetTraces(H_lb,bound) & L_H = GetStrings(H_lb,bound) &
 N = 3 &

  Mi_ = Mi__(A_d,Mi_) &
  M1L_G = MiL(1,L_G,A_d) & M2L_G = MiL(2,L_G,A_d) &

  D_0 = D0(c,L_G,L_H) &
  E_0 = E0(c,L_H) &
  D_1 = Dk_1Ek_1(c,E_0,D_0, A_d) &
  E_1 = Dk_1Ek_1(c,D_0,E_0, A_d) &
  D_2 = Dk_1Ek_1(c,E_1,D_1, A_d) &
  E_2 = Dk_1Ek_1(c,D_1,E_1, A_d) &
  D_3 = Dk_1Ek_1(c,E_2,D_2, A_d) &
  E_3 = Dk_1Ek_1(c,D_2,E_2, A_d) &
  D_4 = Dk_1Ek_1(c,E_3,D_3, A_d) &
  E_4 = Dk_1Ek_1(c,D_3,E_3, A_d) &

  GetLangPair_ = GetLangPair__(G_fb,A_d,GetLangPair_) &
  GetLangPairs_ = GetLangPairs__(G_fb,A_d,GetLangPairs_) &
  allLangPairs = GetLangPairs_(c,baseCaseD0E0s(c,L_G,L_H),4,0)

VARIABLES
  code
INITIALISATION
  code := 0
INVARIANT
  code : 0..2

OPERATIONS
 PrintResults = PRE code = 0 THEN
  code := 1;
  PRINTF("M2([]) = ~w\n",[Mi(2,[],A_d)]);
  PRINTF("M2([d,a,bp,c]) = ~w\n",[Mi(2,[d,a,bp,c],A_d)]);
  PRINTF("M1([a,a,a,c,a,a,c,a,c]) = ~w\n",[Mi(1,[a,a,a,c,a,a,c,a,c],A_d)]);
  PRINTF("M2([]) = ~w\n",[Mi_(2,[],[])]);
  PRINTF("M2([d,a,bp,c]) = ~w\n",[Mi_(2,[d,a,bp,c],[])]);
  PRINTF("M1([a,d,a,c,a,a,c,a,c]) = ~w\n",[Mi_(1,[a,d,a,c,a,a,c,a,c],[])]);
  PRINTF("L(G) = ~w\n",[L_G]);
  PRINTF("M1(L(G)) = ~w\n",[M1L_G]);
  PRINTF("M2(L(G)) = ~w\n",[M2L_G]);
  PRINTF("M1_1Mi(...) = ~w\n",[Mi_1Mi(1,{[],[a]},{[],[c]},A_d)]);
  PRINTF("M1_1Mi(...) = ~w\n",[Mi_1Mi(1,{[d,b,ap,c]},{[d,b,ap]},A_d)]);
  PRINTF("prK = ~w\n",[L_H]);
  PRINTF("D0(c) = ~w\n",[D_0]);
  PRINTF("M1(D0) = ~w\n",[MiL(1,D_0,A_d)]);
  PRINTF("M2(D0) = ~w\n",[MiL(2,D_0,A_d)]);
  PRINTF("E0(c) = ~w\n",[E_0]);
  PRINTF("M1(E0) = ~w\n",[MiL(1,E_0,A_d)]);
  PRINTF("M2(E0) = ~w\n",[MiL(2,E_0,A_d)]);
  PRINTF("Base case D0E0(c) = ~w\n",[baseCaseD0E0(c,L_G,L_H)]);
  PRINTF("D1(c) = ~w\n",[D_1]);
  PRINTF("E1(c) = ~w\n",[E_1]);
  PRINTF("M1(D1) = ~w\n",[MiL(1,D_1,A_d)]);
  PRINTF("M2(D1) = ~w\n",[MiL(2,D_1,A_d)]);
  PRINTF("M1(E1) = ~w\n",[MiL(1,E_1,A_d)]);
  PRINTF("M2(E1) = ~w\n",[MiL(2,E_1,A_d)]);
  PRINTF("D2(c) = ~w\n",[D_2]);
  PRINTF("E2(c) = ~w\n",[E_2]);
  PRINTF("M1(D2) = ~w\n",[MiL(1,D_2,A_d)]);
  PRINTF("M2(D2) = ~w\n",[MiL(2,D_2,A_d)]);
  PRINTF("M1(E2) = ~w\n",[MiL(1,E_2,A_d)]);
  PRINTF("M2(E2) = ~w\n",[MiL(2,E_2,A_d)]);
  PRINTF("D3(c) = ~w\n",[D_3]);
  PRINTF("E3(c) = ~w\n",[E_3]);
  PRINTF("M1(D3) = ~w\n",[MiL(1,D_3,A_d)]);
  PRINTF("M2(D3) = ~w\n",[MiL(2,D_3,A_d)]);
  PRINTF("M1(E3) = ~w\n",[MiL(1,E_3,A_d)]);
  PRINTF("M2(E3) = ~w\n",[MiL(2,E_3,A_d)]);
  PRINTF("D4(c) = ~w\n",[D_4]);
  PRINTF("E4(c) = ~w\n",[E_4]);
  PRINTF("Pair 0 = ~w\n",[GetLangPair_(c,baseCaseD0E0(c,L_G,L_H),0)]);
  PRINTF("Pair 1 = ~w\n",[GetLangPair_(c,baseCaseD0E0(c,L_G,L_H),1)]);
  PRINTF("Pair 2 = ~w\n",[GetLangPair_(c,baseCaseD0E0(c,L_G,L_H),2)]);
  PRINTF("Pair 3 = ~w\n",[GetLangPair_(c,baseCaseD0E0(c,L_G,L_H),3)]);
  PRINTF("Pair 4 = ~w\n",[GetLangPair_(c,baseCaseD0E0(c,L_G,L_H),4)]);
  PRINTF("All pairs = ~w\n",[GetLangPairs_(c,baseCaseD0E0s(c,L_G,L_H),4,0)]);
  PRINTF("All masked pairs (1) = ~w\n",[GetMaskedLangPairs(allLangPairs,1,4,A_d)]);
  PRINTF("All masked pairs (2) = ~w\n",[GetMaskedLangPairs(allLangPairs,2,4,A_d)]);

  PRINTF("ndi = ~w\n",[nde_i(1,[d,a],allLangPairs'E,N,A_d)]);
  PRINTF("nei = ~w\n",[nde_i(1,[d,a],allLangPairs'D,N,A_d)]);
  PRINTF("ni = ~w\n",[n_i(nde_i(2,[a,bp,c],allLangPairs'E,N,A_d),
                          nde_i(2,[a,bp,c],allLangPairs'D,N,A_d) )]);

  VAR ci IN
   c_i(ci,nde_i(1,[d,a],allLangPairs'E,N,A_d),
          nde_i(1,[d,a],allLangPairs'D,N,A_d) );
   PRINTF("ci = ~w\n",[ci]);
   c_i(ci,nde_i(2,[a,bp,c],allLangPairs'E,N,A_d),
          nde_i(2,[a,bp,c],allLangPairs'D,N,A_d) );
   PRINTF("ci = ~w\n",[ci])
  END;

  VAR k IN
   k := 0;
   WHILE k <= N DO
    PRINTF("M1(D~w) = ",[k]);
    PRINTF("~w\n",[MiL(1,allLangPairs'D[{k}],A_d)]);
    PRINTF("M2(D~w) = ",[k]);
    PRINTF("~w\n",[MiL(2,allLangPairs'D[{k}],A_d)]);
    PRINTF("M1(E~w) = ",[k]);
    PRINTF("~w\n",[MiL(1,allLangPairs'E[{k}],A_d)]);
    PRINTF("M2(E~w) = ", [k]);
    PRINTF("~w\n",[MiL(2,allLangPairs'E[{k}],A_d)]);
    k := k + 1
   INVARIANT k : 0..N+1 VARIANT N-k END END END;

 VerifyN_InferenceObservability = // only for the example of Kumar and Takai
  PRE code = 1 THEN
   code := 2;
   VAR n IN
    n := max(dom(allLangPairs'D) \/ dom(allLangPairs'E));
    IF n <= N THEN PRINTF("K is ~w-inference observable\n",[n])
              ELSE PRINTF("K is not ~w-inference observable\n",[N]) END END END END

/*
OUTPUT:
M2([]) = {}
M2([d,a,bp,c]) = {(1|->d),(2|->bp)}
M1([a,a,a,c,a,a,c,a,c]) = [a,a,a,a,a,a]
M2([]) = {}
M2([d,a,bp,c]) = {(1|->d),(2|->bp)}
M1([a,d,a,c,a,a,c,a,c]) = [a,d,a,a,a,a]
L(G) = {
 {},
 {(1|->a)},
 {(1|->a),(2|->c)},
 {(1|->a),(2|->bp)},
 {(1|->b)},
 {(1|->b),(2|->c)},
 {(1|->b),(2|->ap)},
 {(1|->c)},
 {(1|->d)},
 {(1|->d),(2|->a)},
 {(1|->d),(2|->b)},
 {(1|->d),(2|->c)},
 [d,a,c],[d,a,bp],[d,a,bp,c],[d,b,c],[d,b,ap],[d,b,ap,c],[b,ap,c],[a,bp,c]}
M1(L(G)) = {{},{(1|->a)},{(1|->d)},{(1|->d),(2|->a)},{(1|->d),(2|->ap)},{(1|->ap)}}
M2(L(G)) = {{},{(1|->b)},{(1|->d)},{(1|->d),(2|->b)},{(1|->d),(2|->bp)},{(1|->bp)}}
M1_1Mi(...) = {{},{(1|->c)}}
M1_1Mi(...) = {[d,b,ap]}
prK = {
 {},
 {(1|->a)},
 {(1|->a),(2|->bp)},
 {(1|->b)},
 {(1|->b),(2|->ap)},
 {(1|->c)},
 {(1|->d)},
 {(1|->d),(2|->a)},
 {(1|->d),(2|->b)},
 [d,a,c],[d,a,bp],[d,b,c],[d,b,ap],[b,ap,c],[a,bp,c]}
D0(c) = {{(1|->a)},{(1|->b)},{(1|->d)},[d,a,bp],[d,b,ap]}
M1(D0) = {{},{(1|->a)},{(1|->d)},{(1|->d),(2|->a)},{(1|->d),(2|->ap)}}
M2(D0) = {{},{(1|->b)},{(1|->d)},{(1|->d),(2|->b)},{(1|->d),(2|->bp)}}
E0(c) = {{},{(1|->a),(2|->bp)},{(1|->b),(2|->ap)},{(1|->d),(2|->a)},{(1|->d),(2|->b)}}
M1(E0) = {{},{(1|->a)},{(1|->d)},{(1|->d),(2|->a)},{(1|->ap)}}
M2(E0) = {{},{(1|->b)},{(1|->d)},{(1|->d),(2|->b)},{(1|->bp)}}
Base case D0E0(c) = 
 rec(D:{{(1|->a)},{(1|->b)},{(1|->d)},[d,a,bp],[d,b,ap]},
     E:{{},{(1|->a),(2|->bp)},{(1|->b),(2|->ap)},{(1|->d),(2|->a)},{(1|->d),(2|->b)}})
D1(c) = {{(1|->a)},{(1|->b)},{(1|->d)}}
E1(c) = {{},{(1|->d),(2|->a)},{(1|->d),(2|->b)}}
M1(D1) = {{},{(1|->a)},{(1|->d)}}
M2(D1) = {{},{(1|->b)},{(1|->d)}}
M1(E1) = {{},{(1|->d)},{(1|->d),(2|->a)}}
M2(E1) = {{},{(1|->d)},{(1|->d),(2|->b)}}
D2(c) = {{(1|->d)}}
E2(c) = {{}}
M1(D2) = {{(1|->d)}}
M2(D2) = {{(1|->d)}}
M1(E2) = {{}}
M2(E2) = {{}}
D3(c) = {}
E3(c) = {}
M1(D3) = {}
M2(D3) = {}
M1(E3) = {}
M2(E3) = {}
D4(c) = {}
E4(c) = {}
Pair 0 = rec(D:{{(1|->a)},{(1|->b)},{(1|->d)},[d,a,bp],[d,b,ap]},
             E:{{},{(1|->a),(2|->bp)},{(1|->b),(2|->ap)},{(1|->d),(2|->a)},{(1|->d),(2|->b)}})
Pair 1 = rec(D:{{(1|->a)},{(1|->b)},{(1|->d)}},
             E:{{},{(1|->d),(2|->a)},{(1|->d),(2|->b)}})
Pair 2 = rec(D:{{(1|->d)}},E:{{}})
Pair 3 = rec(D:{},E:{})
Pair 4 = rec(D:{},E:{})
All pairs = 
 rec(D:{(0|->{(1|->a)}),(0|->{(1|->b)}),(0|->{(1|->d)}),(0|->[d,a,bp]),(0|->[d,b,ap]),
        (1|->{(1|->a)}),(1|->{(1|->b)}),(1|->{(1|->d)}),
        (2|->{(1|->d)})},
     E:{(0|->{}),(0|->{(1|->a),(2|->bp)}),(0|->{(1|->b),(2|->ap)}),(0|->{(1|->d),(2|->a)}),(0|->{(1|->d),(2|->b)}),
        (1|->{}),(1|->{(1|->d),(2|->a)}),(1|->{(1|->d),(2|->b)}),
        (2|->{})})
All masked pairs (1) = 
 rec(D:{(0|->{}),(0|->{(1|->a)}),(0|->{(1|->d)}),(0|->{(1|->d),(2|->a)}),(0|->{(1|->d),(2|->ap)}),
        (1|->{}),(1|->{(1|->a)}),(1|->{(1|->d)}),
        (2|->{(1|->d)})},
     E:{(0|->{}),(0|->{(1|->a)}),(0|->{(1|->d)}),(0|->{(1|->d),(2|->a)}),(0|->{(1|->ap)}),
        (1|->{}),(1|->{(1|->d)}),(1|->{(1|->d),(2|->a)}),
        (2|->{})})
All masked pairs (2) = 
 rec(D:{(0|->{}),(0|->{(1|->b)}),(0|->{(1|->d)}),(0|->{(1|->d),(2|->b)}),(0|->{(1|->d),(2|->bp)}),
        (1|->{}),(1|->{(1|->b)}),(1|->{(1|->d)}),
        (2|->{(1|->d)})},
     E:{(0|->{}),(0|->{(1|->b)}),(0|->{(1|->d)}),(0|->{(1|->d),(2|->b)}),(0|->{(1|->bp)}),
        (1|->{}),(1|->{(1|->d)}),(1|->{(1|->d),(2|->b)}),
        (2|->{})})
ndi = 2
nei = 1
ni = 0
ci = 1
ci = 1
M1(D0) = {{},{(1|->a)},{(1|->d)},{(1|->d),(2|->a)},{(1|->d),(2|->ap)}}
M2(D0) = {{},{(1|->b)},{(1|->d)},{(1|->d),(2|->b)},{(1|->d),(2|->bp)}}
M1(E0) = {{},{(1|->a)},{(1|->d)},{(1|->d),(2|->a)},{(1|->ap)}}
M2(E0) = {{},{(1|->b)},{(1|->d)},{(1|->d),(2|->b)},{(1|->bp)}}
M1(D1) = {{},{(1|->a)},{(1|->d)}}
M2(D1) = {{},{(1|->b)},{(1|->d)}}
M1(E1) = {{},{(1|->d)},{(1|->d),(2|->a)}}
M2(E1) = {{},{(1|->d)},{(1|->d),(2|->b)}}
M1(D2) = {{(1|->d)}}
M2(D2) = {{(1|->d)}}
M1(E2) = {{}}
M2(E2) = {{}}
M1(D3) = {}
M2(D3) = {}
M1(E3) = {}
M2(E3) = {}
cover(PrintResults)
K is 2-inference observable
cover(VerifyN_InferenceObservability)
*/