// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL section5_4_find_ex6_tunnels

SEES
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------
 SBtrains_6_2 /*@file "../Trains/SBtrains_6_2.mch" */

SETS T; Y

CONSTANTS
 Q_,                         // set of good states
 S,                          // set of sections
 dist,                       // distance between two sections
 prj_pos_1, prj_pos_2        // projections of a global state to local states
 
ABSTRACT_CONSTANTS
 GetsupCCP         // Lambda function to compute the supremal
                   // coreachable controllable predicate

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 50000000;
 SET_PREF_SMT == FALSE;
 SET_PREF_KODKOD == FALSE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../Theories/SBCDES.def";
 "LibraryIO.def"

PROPERTIES
 card(T) = 3 &
 card(Y) = 9 &
 Q_ = Qpred1 &
 S = LocalStates &           // rename the set of local states
 prj_pos_1 = t61 &
 prj_pos_2 = t62 &

 dist = {(s1,s1,0), (s1,s2,1), (s1,s3,2), (s1,s4,3), (s1,s5,2), (s1,s6,1),
         (s2,s1,1), (s2,s2,0), (s2,s3,1), (s2,s4,2), (s2,s5,3), (s2,s6,2),
         (s3,s1,2), (s3,s2,1), (s3,s3,0), (s3,s4,1), (s3,s5,2), (s3,s6,3),
         (s4,s1,3), (s4,s2,2), (s4,s3,1), (s4,s4,0), (s4,s5,1), (s4,s6,2),
         (s5,s1,2), (s5,s2,3), (s5,s3,2), (s5,s4,1), (s5,s5,0), (s5,s6,1),
         (s6,s1,1), (s6,s2,2), (s6,s3,3), (s6,s4,2), (s6,s5,1), (s6,s6,0) } &

 G_FB : Generator_C(States,Events) &
 GetsupCCP = lambdaGetsupCCP(G_FB,GetsupCCP)

VARIABLES
 supCCP       // supremal controllable and coreachable predicate stronger than Q

INITIALISATION
 supCCP := GetsupCCP(Q_)

INVARIANT
 supCCP <: Q_

OPERATIONS
 findMask = ANY f_, Tr_, m_ WHERE
  f_ : S -->> T &            // A total surjective function
  Tr_ : T*T >->> Y &         // A total bijection
  m_ : G_FB'states -->> Y &  // A total surjective function

  not(#t_.(t_ : T & card(f_|>{t_}) = 1)) &
  !(s1_,s2_).(s1_ : S & s2_ : S & s1_ /= s2_ & f_(s1_) = f_(s2_) =>
   dist(s1_,s2_) <= card(f_|>{f_(s1_)})-1 ) &

  !x_.(x_ : G_FB'states => 
   m_(x_) = Tr_(f_(prj_pos_1(x_)),f_(prj_pos_2(x_))) ) &

  supCCP = R_breve(G_FB,m_,CtrlField(G_FB,Q_)) THEN
   PRINT(f_); PRINT(Tr_); PRINT(m_);
   PRINT(R_breve(G_FB,m_,CtrlField(G_FB,Q_))); PRINT(supCCP) END END

/*
OUTPUT:
{(s1|->T1),(s2|->T1),(s3|->T2),(s4|->T2),(s5|->T3),(s6|->T3)}
{((T1|->T1)|->Y1),((T1|->T2)|->Y2),((T1|->T3)|->Y6),
 ((T2|->T1)|->Y3),((T2|->T2)|->Y5),((T2|->T3)|->Y8),
 ((T3|->T1)|->Y4),((T3|->T2)|->Y7),((T3|->T3)|->Y9)}

{(s1_s1|->Y1),(s1_s2|->Y1),(s2_s1|->Y1),(s2_s2|->Y1),
 (s3_s3|->Y5),(s3_s4|->Y5),(s4_s3|->Y5),(s4_s4|->Y5),
 (s5_s5|->Y9),(s5_s6|->Y9),(s6_s5|->Y9),(s6_s6|->Y9),
 (s1_s3|->Y2),(s1_s4|->Y2),(s2_s3|->Y2),(s2_s4|->Y2),
 (s3_s1|->Y3),(s3_s2|->Y3),(s4_s1|->Y3),(s4_s2|->Y3),
 (s5_s1|->Y4),(s5_s2|->Y4),(s6_s1|->Y4),(s6_s2|->Y4),
 (s1_s5|->Y6),(s1_s6|->Y6),(s2_s5|->Y6),(s2_s6|->Y6),
 (s5_s3|->Y7),(s5_s4|->Y7),(s6_s3|->Y7),(s6_s4|->Y7),
 (s3_s5|->Y8),(s3_s6|->Y8),(s4_s5|->Y8),(s4_s6|->Y8)}
{s3_s1,s1_s3,s4_s1,s3_s2,s2_s3,s1_s4,s5_s1,s4_s2,s2_s4,s1_s5,s6_s1,s5_s2,
 s2_s5,s1_s6,s6_s2,s5_s3,s3_s5,s2_s6,s6_s3,s5_s4,s4_s5,s3_s6,s6_s4,s4_s6}
{s3_s1,s1_s3,s4_s1,s3_s2,s2_s3,s1_s4,s5_s1,s4_s2,s2_s4,s1_s5,s6_s1,s5_s2,
 s2_s5,s1_s6,s6_s2,s5_s3,s3_s5,s2_s6,s6_s3,s5_s4,s4_s5,s3_s6,s6_s4,s4_s6}
cover(findMask)
% Runtime: 2070 ms (total: 2070 ms, walltime: 2080 ms)
% Finished execute (errors=0, warnings=0)
% Finished processing file after 2140 ms walltime (2120 ms runtime), since start: 3790 ms
*/