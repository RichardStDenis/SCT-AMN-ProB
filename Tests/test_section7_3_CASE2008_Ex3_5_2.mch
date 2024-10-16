// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL test_section7_3_CASE2008_Ex3_5_2

SEES
 //---------------------------------------------------------------------
 // Machine for input data
 //---------------------------------------------------------------------
 CASE2008_Ex3_5_2 /*@file "../DataExamples/CASE2008_Ex3_5_2.mch" */

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
 L_G, L_H,    // Languages for free and legal behaviors
 m            // Parameter of the m-co-observability property

PROPERTIES
 G_fb : Generator_C_O(G_fb'states,G_fb'events) &
 H_lb : FSA(H_lb'states,H_lb'events) &
 L_G = GetStrings(G_fb,bound) &
 L_H = GetStrings(H_lb,bound) &
 m = 3

VARIABLES
 code
INITIALISATION
 code := 0
INVARIANT
 (code = 0 or code = 1)

OPERATIONS
 VerifymCoobservability =
  ANY Ece, Ecd WHERE
   Ece /\ Ecd = {} & Ece \/ Ecd = G_fb'ctrlEvents THEN // & Ece = {tau} THEN
    PRINT(Ece); PRINT(Ecd);
    VAR Ece_, e_ IN
     Ece_ := Ece;
     WHILE Ece_ /= {} DO
      e_ :: Ece_;
      ANY PE WHERE
       PE <: POW(E_(L_H,e_)) & card(PE) >= 1 & card(PE) <= m &
       !(S1,S2).(S1 : PE & S2 : PE & S1 /= S2 => S1 /\ S2 = {}) &
       UNION(S_).(S_ : PE | S_) = E_(L_H,e_) &
       CPmCoobservability_e(A_d,e_,L_H,D_(L_H,L_G,e_),PE) THEN
        PRINT(e_ |-> PE) END;
      Ece_ := Ece_ - {e_}
     INVARIANT Ece_ <: Ece VARIANT card(Ece_) END END;
    PRINT("K is C&P3-coobservable.");

    VAR Ecd_, e_ IN
     Ecd_ := Ecd;
     WHILE Ecd_ /= {} DO
      e_ :: Ecd_;
      ANY PD WHERE
       PD <: POW(D_(L_H,L_G,e_)) & card(PD) >= 1 & card(PD) <= m &
       !(S1,S2).(S1 : PD & S2 : PD & S1 /= S2 => S1 /\ S2 = {}) &
       UNION(S_).(S_ : PD | S_) = D_(L_H,L_G,e_) &
       DAmCoobservability_e(A_d,e_,L_H,E_(L_H,e_),PD) THEN
        PRINT(e_ |-> PD) END;
      Ecd_ := Ecd_ - {e_}
     INVARIANT Ecd_ <: Ecd VARIANT card(Ecd_) END;
    PRINT("K is D&A3-coobservable.") END END
END

/*
OUTPUT:
With a constraint Ece = {tau}), about one minute:
{tau}
{sigma}
(tau|->{{{(1|->b1),(2|->a2)}},{[c2,a1,a2]},{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}})
"K is C&P3-coobservable."
(sigma|->{{{(1|->b1),(2|->a2)}},{[c2,a1,a2]},{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}})
"K is D&A3-coobservable."
cover(VerifymCoobservability)

With no constraint, about two minutes:
{sigma,tau}
{}
(sigma|->{{},{{(1|->a1),(2|->a2)},{(1|->a1),(2|->b2)},[c2,a1,b2],[c2,a1,b2,c1],[d2,b1,a2,c1],[a1,a2,c1]},
          {{(1|->b1),(2|->b2)},[d2,b1,a2],[d2,b1,b2],[b1,b2,c1]}})
(tau|->{{{(1|->b1),(2|->a2)}},{[c2,a1,a2]},{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}})
"K is C&P3-coobservable."
"K is D&A3-coobservable."
cover(VerifymCoobservability)
*/