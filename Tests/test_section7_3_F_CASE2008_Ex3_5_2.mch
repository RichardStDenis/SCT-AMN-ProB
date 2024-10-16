// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL test_section7_3_F_CASE2008_Ex3_5_2

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
 L_G, L_H    // Languages for free and legal behaviors

PROPERTIES
 G_fb : Generator_C_O(G_fb'states,G_fb'events) &
 H_lb : FSA(H_lb'states,H_lb'events) &
 L_G = GetStrings(G_fb,bound) &
 L_H = GetStrings(H_lb,bound)

VARIABLES
 code
INITIALISATION
 code := 0
INVARIANT
 code = 0 or code = 1

OPERATIONS
 VerifyCP3Coobservability0 =
  SELECT CPmCoobservability(A_d,{},L_G,L_H,{}) THEN
   code := 1 || PRINT("K is C&P3-coobservable.") ELSE
   code := 0 || PRINT("K is not C&P3-coobservable.") END;

 VerifyCP3Coobservability1 =
  SELECT CPmCoobservability(A_d,A_d'Events_ce,L_G,L_H,
    {(tau,{{[b1,a2]},{[c2,a1,a2]},{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}})} ) THEN
   code := 1 || PRINT("K is C&P3-coobservable.") ELSE
   code := 0 || PRINT("K is not C&P3-coobservable.") END;

 VerifyCP3Coobservability2 =
  SELECT CPmCoobservability_e(A_d,tau,L_H,D_(L_H,L_G,tau),
    {{[b1,a2]},{[c2,a1,a2]},{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}} ) THEN
   code := 1 || PRINT("K is C&P3-coobservable.") ELSE
   code := 0 || PRINT("K is not C&P3-coobservable.") END;

 VerifyCP3Coobservability3 =
  VAR Ece, PEs IN
   Ece := {tau};   PEs := {(tau,{{[b1,a2]},{[c2,a1,a2]},{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}})};
   IF CPmCoobservability(A_d,Ece,L_G,L_H,PEs) THEN
    code := 1 || PRINT("K is C&P3-coobservable.") ELSE
    code := 0 || PRINT("K is not C&P3-coobservable.") END END;

 VerifyDA3Coobservability0 =
  SELECT DAmCoobservability(A_d,{},L_H,{}) THEN
   code := 1 || PRINT("K is D&A3-coobservable.") ELSE
   code := 0 || PRINT("K is not D&A3-coobservable.") END;

 VerifyDA3Coobservability1 =
  SELECT DAmCoobservability(A_d,A_d'Events_cd,L_H,
    {(sigma,{{[b1,a2]},{[c2,a1,a2]},{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}})} ) THEN
   code := 1 || PRINT("K is D&A3-coobservable.") ELSE
   code := 0 || PRINT("K is not D&A3-coobservable.") END;

 VerifyDA3Coobservability2 =
  SELECT DAmCoobservability_e(A_d,sigma,L_H,E_(L_H, sigma),
    {{[b1,a2]},{[c2,a1,a2]},{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}} ) THEN
   code := 1 || PRINT("K is D&A3-coobservable.") ELSE
   code := 0 || PRINT("K is not D&A3-coobservable.") END;

 VerifyDA3Coobservability3 =
  VAR Ecd, PDs IN
   Ecd := {sigma};   PDs := {(sigma,{{[b1,a2]},{[c2,a1,a2]},{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}})};
   IF DAmCoobservability(A_d,Ecd,L_H,PDs) THEN
    code := 1 || PRINT("K is D&A3-coobservable.") ELSE
    code := 0 || PRINT("K is not D&A3-coobservable.") END END;

 VerifymCoobservability1 =
  SELECT mCoobservability(A_d,A_d'Events_ce,A_d'Events_cd,L_G,L_H,
    {(tau,{{[b1,a2]},{[c2,a1,a2]},{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}})},
    {(sigma,{{[b1,a2]},{[c2,a1,a2]},{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}})} ) THEN
   code := 1 || PRINT("K is 3-coobservable.") ELSE
   code := 0 || PRINT("K is not 3-coobservable.") END;

 VerifymCoobservability2 =
  ANY Ece, Ecd WHERE
   Ece /\ Ecd = {} & Ece \/ Ecd = G_fb'ctrlEvents THEN
    LET PEs, PDs BE
     PEs = {e_, P_ | e_ : Ece & P_ = {{[b1,a2]},{[c2,a1,a2]},{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}}} &
     PDs = {e_, P_ | e_ : Ecd & P_ = {{[b1,a2]},{[c2,a1,a2]},{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}}} IN
      IF mCoobservability(A_d,Ece,Ecd,L_G,L_H,PEs,PDs) THEN
       code := 1 || PRINT("K is 3-coobservable.") || PRINT(Ece) || PRINT(Ecd) END END END;

 FindPartitionForCP3Coobservability =
  ANY S1, S2, S3 WHERE
   S1 /\ S2 = {} & S1 /\ S3 = {} & S2 /\ S3 = {} &
   S1 \/ S2 \/ S3 = E_(L_H, tau) &
   CPmCoobservability(A_d,{tau},L_G,L_H,{(tau,{S1, S2, S3})}) THEN
    code := 1 || PRINT("K is C&P3-coobservable.") ||
                 PRINT(S1) || PRINT(S2) || PRINT(S3) END;

 FindPartitionForDA3Coobservability =
  ANY S1, S2, S3 WHERE
   S1 /\ S2 = {} & S1 /\ S3 = {} & S2 /\ S3 = {} &
   S1 \/ S2 \/ S3 = D_(L_H, L_G, sigma) &
   DAmCoobservability(A_d,{sigma},L_H,{(sigma,{S1, S2, S3})}) THEN
    code := 1 || PRINT("K is D&A3-coobservable.") ||
                 PRINT(S1) || PRINT(S2) || PRINT(S3) END;

 VerifymCP3Coobservability4 =
  ANY Ece, Ecd WHERE
    Ece /\ Ecd = {} & Ece \/ Ecd = G_fb'ctrlEvents THEN
   VAR Ece_, PEs IN
    Ece_ := Ece;   PEs := {};
    WHILE Ece_ /= {} DO
     ANY e_, S1, S2, S3 WHERE e_ : Ece_ &
       S1 /\ S2 = {} & S1 /\ S3 = {} & S2 /\ S3 = {} & 
       S1 \/ S2 \/ S3 = E_(L_H, e_) &
       CPmCoobservability_e(A_d,e_,L_H, D_(L_H,L_G,e_),
                            {S1, S2, S3}) THEN
      PEs := PEs \/ {e_ |-> {S1, S2, S3}};
      Ece_ := Ece_ - {e_} END
    INVARIANT Ece_ <: Ece VARIANT card(Ece_) END;
    PRINT("E:"); PRINT(PEs);
    IF CPmCoobservability(A_d,Ece,L_G,L_H,PEs) THEN
     code := 1; PRINT("K is C&P3-coobservable."); PRINT(Ece) END END END;

 VerifymDA3Coobservability4 =
  ANY Ece, Ecd WHERE
    Ece /\ Ecd = {} & Ece \/ Ecd = G_fb'ctrlEvents THEN
   VAR Ecd_, PDs IN
    Ecd_ := Ece;   PDs := {};  // Not use Ecd because Ecd = {} (trivial solution)
    WHILE Ecd_ /= {} DO
     ANY e_, S1, S2, S3 WHERE  e_ : Ecd_ &
       S1 /\ S2 = {} & S1 /\ S3 = {} & S2 /\ S3 = {} &
       S1 \/ S2 \/ S3 = D_(L_H, L_G, e_) &
       DAmCoobservability_e(A_d,e_,L_H, E_(L_H, e_),
                           {S1, S2, S3}) THEN
      PDs := PDs \/ {e_ |-> {S1, S2, S3}};
      Ecd_ := Ecd_ - {e_} END
    INVARIANT Ecd_ <: Ece VARIANT card(Ecd_) END;
    PRINT("D:"); PRINT(PDs);
    IF DAmCoobservability(A_d,Ece,L_H,PDs) THEN
     code := 1 || PRINT("K is D&A3-coobservable.") || PRINT(Ece) END END END;

 VerifymCoobservability =
  ANY Ece, Ecd WHERE
    Ece /\ Ecd = {} & Ece \/ Ecd = G_fb'ctrlEvents & card(Ece) = card(Ecd) THEN
   VAR Ece_, PEs IN
    Ece_ := Ece;   PEs := {};
    WHILE Ece_ /= {} DO
     ANY e_, S1, S2, S3 WHERE e_ : Ece_ &
       S1 /\ S2 = {} & S1 /\ S3 = {} & S2 /\ S3 = {} &
       S1 \/ S2 \/ S3 = E_(L_H, e_) &
       CPmCoobservability_e(A_d,e_,L_H, D_(L_H,L_G,e_),
                            {S1, S2, S3} ) THEN
      PEs := PEs \/ {e_ |-> {S1, S2, S3}};
      Ece_ := Ece_ - {e_} END
    INVARIANT Ece_ <: Ece VARIANT card(Ece_) END;
    PRINT("E:"); PRINT(PEs);
    IF CPmCoobservability(A_d,Ece,L_G,L_H,PEs) THEN
     code := 1; PRINT("K is C&P3-coobservable."); PRINT(Ece) END END;

   VAR Ecd_, PDs IN
    Ecd_ := Ecd;   PDs := {};
    WHILE Ecd_ /= {} DO
     ANY e_, S1, S2, S3 WHERE  e_ : Ecd_ &
       S1 /\ S2 = {} & S1 /\ S3 = {} & S2 /\ S3 = {} &
       S1 \/ S2 \/ S3 = D_(L_H, L_G, e_) &
       DAmCoobservability_e(A_d,e_,L_H, E_(L_H,e_),
                            {S1, S2, S3} ) THEN
      PDs := PDs \/ {e_ |-> {S1, S2, S3}};
      Ecd_ := Ecd_ - {e_} END
    INVARIANT Ecd_ <: Ecd VARIANT card(Ecd_) END;
    PRINT("D:"); PRINT(PDs);
    IF DAmCoobservability(A_d,Ecd,L_H,PDs) THEN
     code := 1; PRINT("K is D&A3-coobservable."); PRINT(Ecd) END END END END

/*
OUTPUT:
"K is C&P3-coobservable."
cover(VerifyCP3Coobservability0)
"K is C&P3-coobservable."
cover(VerifyCP3Coobservability1)
"K is C&P3-coobservable."
cover(VerifyCP3Coobservability2)
"K is C&P3-coobservable."
cover(VerifyCP3Coobservability3)
"K is D&A3-coobservable."
cover(VerifyDA3Coobservability0)
"K is D&A3-coobservable."
cover(VerifyDA3Coobservability1)
"K is D&A3-coobservable."
cover(VerifyDA3Coobservability2)
"K is D&A3-coobservable."
cover(VerifyDA3Coobservability3)
"K is 3-coobservable."
cover(VerifymCoobservability1)
"K is 3-coobservable."
{tau}
{sigma}
cover(VerifymCoobservability2)
"K is C&P3-coobservable."
{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}
{[c2,a1,a2]}
{{(1|->b1),(2|->a2)}}
cover(FindPartitionForCP3Coobservability)
"K is D&A3-coobservable."
{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}
{[c2,a1,a2]}
{{(1|->b1),(2|->a2)}}
cover(FindPartitionForDA3Coobservability)
"E:"
{(tau|->{{{(1|->b1),(2|->a2)}},{[c2,a1,a2]},{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}}),
  (sigma|->{{},{{(1|->a1),(2|->a2)},{(1|->a1),(2|->b2)},[c2,a1,b2],[c2,a1,b2,c1],[d2,b1,a2,c1],[a1,a2,c1]},
            {{(1|->b1),(2|->b2)},[d2,b1,a2],[d2,b1,b2],[b1,b2,c1]}})}
"K is C&P3-coobservable."
{sigma,tau}
cover(VerifymCP3Coobservability4)
"D:"
{(tau|->{{},{{(1|->a1),(2|->a2)},{(1|->a1),(2|->b2)},[c2,a1,b2],[c2,a1,b2,c1],[d2,b1,a2,c1],[a1,a2,c1]},
         {{(1|->b1),(2|->b2)},[d2,b1,a2],[d2,b1,b2],[b1,b2,c1]}}),
(sigma|->{{{(1|->b1),(2|->a2)}},{[c2,a1,a2]},{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}})}
"K is D&A3-coobservable."
{sigma,tau}
cover(VerifymDA3Coobservability4)
"E:"
{(tau|->{{{(1|->b1),(2|->a2)}},{[c2,a1,a2]},{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}})}
"K is C&P3-coobservable."
{tau}
"D:"
{(sigma|->{{{(1|->b1),(2|->a2)}},{[c2,a1,a2]},{[c2,a1,b2,d1],[d2,b1,a2,d1],[a1,a2,d1],[b1,b2,d1]}})}
"K is D&A3-coobservable."
{sigma}
cover(VerifymCoobservability)
*/