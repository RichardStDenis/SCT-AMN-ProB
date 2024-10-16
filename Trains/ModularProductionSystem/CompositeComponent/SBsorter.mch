// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL SBsorter

SETS
 States = {w_Cnm_Dp3, p1_Cnm_Dp3, w_Cnm_Dp3p1,
           w_Cnm_Dp3p2, w_Cim_Dp3, p2_Cnm_Dp3,
           p3_Cnm_Dp3, p1_Cim_Dp3, p1_Cnm_Dp3p1,
           p1_Cnm_Dp3p2, w_Cim_Dp3p1, p2_Cnm_Dp3p1,
           p3_Cnm_Dp3p1, w_Cnm_Dp1, w_Cim_Dp3p2,
           p2_Cnm_Dp3p2, p3_Cnm_Dp3p2, w_Cnm_Dp2,
           p2_Cim_Dp3, p3_Cim_Dp3, p1_Cim_Dp3p1,
           p1_Cim_Dp3p2, p1_Cnm_Dp1, p1_Cnm_Dp2,
           p2_Cim_Dp3p1, p3_Cim_Dp3p1, w_Cim_Dp1,
           p2_Cnm_Dp1, p3_Cnm_Dp1, w_Cnm_Dpxp3,
           p2_Cim_Dp3p2, p3_Cim_Dp3p2, w_Cim_Dp2,
           p2_Cnm_Dp2, p3_Cnm_Dp2, p1_Cim_Dp1,
           p1_Cim_Dp2, p1_Cnm_Dpxp3, p2_Cim_Dp1,
           p3_Cim_Dp1, w_Cim_Dpxp3, p2_Cnm_Dpxp3,
           p3_Cnm_Dpxp3, p2_Cim_Dp2, p3_Cim_Dp2,
           p1_Cim_Dpxp3, p2_Cim_Dpxp3, p3_Cim_Dpxp3 };
 Events = {cp1, cp2, cp3, Cmso, Dp1s, Dp2s, Dp3s, Cstart, Dselect1, Dselect2, Dselect3};
 LocalStates = {p1, p2, p3, w, Cnm, Cim, Dp1, Dp2, Dp3, Dp3p1, Dp3p2, Dpxp3}

CONSTANTS
 G_FB,
 seqS, conveyor, dispatcher,
 Qpred, BadStates

PROPERTIES
 G_FB =rec(   // Automaton modeling the free behavior
      states: States,
      events: Events,
      tf: {(w_Cnm_Dp3,cp1,p1_Cnm_Dp3), (w_Cnm_Dp3,Dselect1,w_Cnm_Dp3p1), (w_Cnm_Dp3,Dselect2,w_Cnm_Dp3p2),
           (w_Cnm_Dp3,Cstart,w_Cim_Dp3), (w_Cnm_Dp3,cp2,p2_Cnm_Dp3), (w_Cnm_Dp3,cp3,p3_Cnm_Dp3),
           (p1_Cnm_Dp3,Cstart,p1_Cim_Dp3), (p1_Cnm_Dp3,Dselect1,p1_Cnm_Dp3p1), (p1_Cnm_Dp3,Dselect2,p1_Cnm_Dp3p2),
           (w_Cnm_Dp3p1,cp1,p1_Cnm_Dp3p1), (w_Cnm_Dp3p1,Cstart,w_Cim_Dp3p1), (w_Cnm_Dp3p1,cp2,p2_Cnm_Dp3p1),
           (w_Cnm_Dp3p1,cp3,p3_Cnm_Dp3p1), (w_Cnm_Dp3p1,Dp1s,w_Cnm_Dp1), (w_Cnm_Dp3p2,cp1,p1_Cnm_Dp3p2),
           (w_Cnm_Dp3p2,Cstart,w_Cim_Dp3p2), (w_Cnm_Dp3p2,cp2,p2_Cnm_Dp3p2), (w_Cnm_Dp3p2,cp3,p3_Cnm_Dp3p2),
           (w_Cnm_Dp3p2,Dp2s,w_Cnm_Dp2), (w_Cim_Dp3,cp1,p1_Cim_Dp3), (w_Cim_Dp3,Dselect1,w_Cim_Dp3p1),
           (w_Cim_Dp3,Dselect2,w_Cim_Dp3p2), (w_Cim_Dp3,cp2,p2_Cim_Dp3), (w_Cim_Dp3,cp3,p3_Cim_Dp3),
           (w_Cim_Dp3,Cmso,w_Cnm_Dp3), (p2_Cnm_Dp3,Cstart,p2_Cim_Dp3), (p2_Cnm_Dp3,Dselect1,p2_Cnm_Dp3p1),
           (p2_Cnm_Dp3,Dselect2,p2_Cnm_Dp3p2), (p3_Cnm_Dp3,Cstart,p3_Cim_Dp3), (p3_Cnm_Dp3,Dselect1,p3_Cnm_Dp3p1),
           (p3_Cnm_Dp3,Dselect2,p3_Cnm_Dp3p2), (p1_Cim_Dp3,Cmso,w_Cnm_Dp3), (p1_Cim_Dp3,Dselect1,p1_Cim_Dp3p1),
           (p1_Cim_Dp3,Dselect2,p1_Cim_Dp3p2), (p1_Cnm_Dp3p1,Cstart,p1_Cim_Dp3p1), (p1_Cnm_Dp3p1,Dp1s,p1_Cnm_Dp1),
           (p1_Cnm_Dp3p2,Cstart,p1_Cim_Dp3p2), (p1_Cnm_Dp3p2,Dp2s,p1_Cnm_Dp2), (w_Cim_Dp3p1,cp1,p1_Cim_Dp3p1),
           (w_Cim_Dp3p1,cp2,p2_Cim_Dp3p1), (w_Cim_Dp3p1,cp3,p3_Cim_Dp3p1), (w_Cim_Dp3p1,Cmso,w_Cnm_Dp3p1),
           (w_Cim_Dp3p1,Dp1s,w_Cim_Dp1), (p2_Cnm_Dp3p1,Cstart,p2_Cim_Dp3p1), (p2_Cnm_Dp3p1,Dp1s,p2_Cnm_Dp1),
           (p3_Cnm_Dp3p1,Cstart,p3_Cim_Dp3p1), (p3_Cnm_Dp3p1,Dp1s,p3_Cnm_Dp1), (w_Cnm_Dp1,cp1,p1_Cnm_Dp1),
           (w_Cnm_Dp1,Dselect2,w_Cnm_Dp3p2), (w_Cnm_Dp1,Dselect3,w_Cnm_Dpxp3), (w_Cnm_Dp1,Cstart,w_Cim_Dp1),
           (w_Cnm_Dp1,cp2,p2_Cnm_Dp1), (w_Cnm_Dp1,cp3,p3_Cnm_Dp1), (w_Cim_Dp3p2,cp1,p1_Cim_Dp3p2),
           (w_Cim_Dp3p2,cp2,p2_Cim_Dp3p2), (w_Cim_Dp3p2,cp3,p3_Cim_Dp3p2), (w_Cim_Dp3p2,Cmso,w_Cnm_Dp3p2),
           (w_Cim_Dp3p2,Dp2s,w_Cim_Dp2), (p2_Cnm_Dp3p2,Cstart,p2_Cim_Dp3p2), (p2_Cnm_Dp3p2,Dp2s,p2_Cnm_Dp2),
           (p3_Cnm_Dp3p2,Cstart,p3_Cim_Dp3p2), (p3_Cnm_Dp3p2,Dp2s,p3_Cnm_Dp2), (w_Cnm_Dp2,cp1,p1_Cnm_Dp2),
           (w_Cnm_Dp2,Dselect1,w_Cnm_Dp3p1), (w_Cnm_Dp2,Dselect3,w_Cnm_Dpxp3), (w_Cnm_Dp2,Cstart,w_Cim_Dp2),
           (w_Cnm_Dp2,cp2,p2_Cnm_Dp2), (w_Cnm_Dp2,cp3,p3_Cnm_Dp2), (p2_Cim_Dp3,Cmso,w_Cnm_Dp3),
           (p2_Cim_Dp3,Dselect1,p2_Cim_Dp3p1), (p2_Cim_Dp3,Dselect2,p2_Cim_Dp3p2), (p3_Cim_Dp3,Cmso,w_Cnm_Dp3),
           (p3_Cim_Dp3,Dselect1,p3_Cim_Dp3p1), (p3_Cim_Dp3,Dselect2,p3_Cim_Dp3p2), (p1_Cim_Dp3p1,Cmso,w_Cnm_Dp3p1),
           (p1_Cim_Dp3p1,Dp1s,p1_Cim_Dp1), (p1_Cim_Dp3p2,Cmso,w_Cnm_Dp3p2), (p1_Cim_Dp3p2,Dp2s,p1_Cim_Dp2),
           (p1_Cnm_Dp1,Cstart,p1_Cim_Dp1), (p1_Cnm_Dp1,Dselect2,p1_Cnm_Dp3p2), (p1_Cnm_Dp1,Dselect3,p1_Cnm_Dpxp3),
           (p1_Cnm_Dp2,Cstart,p1_Cim_Dp2), (p1_Cnm_Dp2,Dselect1,p1_Cnm_Dp3p1), (p1_Cnm_Dp2,Dselect3,p1_Cnm_Dpxp3),
           (p2_Cim_Dp3p1,Cmso,w_Cnm_Dp3p1), (p2_Cim_Dp3p1,Dp1s,p2_Cim_Dp1), (p3_Cim_Dp3p1,Cmso,w_Cnm_Dp3p1),
           (p3_Cim_Dp3p1,Dp1s,p3_Cim_Dp1), (w_Cim_Dp1,cp1,p1_Cim_Dp1), (w_Cim_Dp1,Dselect2,w_Cim_Dp3p2),
           (w_Cim_Dp1,Dselect3,w_Cim_Dpxp3), (w_Cim_Dp1,cp2,p2_Cim_Dp1), (w_Cim_Dp1,cp3,p3_Cim_Dp1),
           (w_Cim_Dp1,Cmso,w_Cnm_Dp1), (p2_Cnm_Dp1,Cstart,p2_Cim_Dp1), (p2_Cnm_Dp1,Dselect2,p2_Cnm_Dp3p2),
           (p2_Cnm_Dp1,Dselect3,p2_Cnm_Dpxp3), (p3_Cnm_Dp1,Cstart,p3_Cim_Dp1), (p3_Cnm_Dp1,Dselect2,p3_Cnm_Dp3p2),
           (p3_Cnm_Dp1,Dselect3,p3_Cnm_Dpxp3), (w_Cnm_Dpxp3,cp1,p1_Cnm_Dpxp3), (w_Cnm_Dpxp3,Cstart,w_Cim_Dpxp3),
           (w_Cnm_Dpxp3,cp2,p2_Cnm_Dpxp3), (w_Cnm_Dpxp3,cp3,p3_Cnm_Dpxp3), (w_Cnm_Dpxp3,Dp3s,w_Cnm_Dp3),
           (p2_Cim_Dp3p2,Cmso,w_Cnm_Dp3p2), (p2_Cim_Dp3p2,Dp2s,p2_Cim_Dp2), (p3_Cim_Dp3p2,Cmso,w_Cnm_Dp3p2),
           (p3_Cim_Dp3p2,Dp2s,p3_Cim_Dp2), (w_Cim_Dp2,cp1,p1_Cim_Dp2), (w_Cim_Dp2,Dselect1,w_Cim_Dp3p1),
           (w_Cim_Dp2,Dselect3,w_Cim_Dpxp3), (w_Cim_Dp2,cp2,p2_Cim_Dp2), (w_Cim_Dp2,cp3,p3_Cim_Dp2),
           (w_Cim_Dp2,Cmso,w_Cnm_Dp2), (p2_Cnm_Dp2,Cstart,p2_Cim_Dp2), (p2_Cnm_Dp2,Dselect1,p2_Cnm_Dp3p1),
           (p2_Cnm_Dp2,Dselect3,p2_Cnm_Dpxp3), (p3_Cnm_Dp2,Cstart,p3_Cim_Dp2), (p3_Cnm_Dp2,Dselect1,p3_Cnm_Dp3p1),
           (p3_Cnm_Dp2,Dselect3,p3_Cnm_Dpxp3), (p1_Cim_Dp1,Cmso,w_Cnm_Dp1), (p1_Cim_Dp1,Dselect2,p1_Cim_Dp3p2),
           (p1_Cim_Dp1,Dselect3,p1_Cim_Dpxp3), (p1_Cim_Dp2,Cmso,w_Cnm_Dp2), (p1_Cim_Dp2,Dselect1,p1_Cim_Dp3p1),
           (p1_Cim_Dp2,Dselect3,p1_Cim_Dpxp3), (p1_Cnm_Dpxp3,Cstart,p1_Cim_Dpxp3), (p1_Cnm_Dpxp3,Dp3s,p1_Cnm_Dp3),
           (p2_Cim_Dp1,Cmso,w_Cnm_Dp1), (p2_Cim_Dp1,Dselect2,p2_Cim_Dp3p2), (p2_Cim_Dp1,Dselect3,p2_Cim_Dpxp3),
           (p3_Cim_Dp1,Cmso,w_Cnm_Dp1), (p3_Cim_Dp1,Dselect2,p3_Cim_Dp3p2), (p3_Cim_Dp1,Dselect3,p3_Cim_Dpxp3),
           (w_Cim_Dpxp3,cp1,p1_Cim_Dpxp3), (w_Cim_Dpxp3,cp2,p2_Cim_Dpxp3), (w_Cim_Dpxp3,cp3,p3_Cim_Dpxp3),
           (w_Cim_Dpxp3,Cmso,w_Cnm_Dpxp3), (w_Cim_Dpxp3,Dp3s,w_Cim_Dp3), (p2_Cnm_Dpxp3,Cstart,p2_Cim_Dpxp3),
           (p2_Cnm_Dpxp3,Dp3s,p2_Cnm_Dp3), (p3_Cnm_Dpxp3,Cstart,p3_Cim_Dpxp3), (p3_Cnm_Dpxp3,Dp3s,p3_Cnm_Dp3),
           (p2_Cim_Dp2,Cmso,w_Cnm_Dp2), (p2_Cim_Dp2,Dselect1,p2_Cim_Dp3p1), (p2_Cim_Dp2,Dselect3,p2_Cim_Dpxp3),
           (p3_Cim_Dp2,Cmso,w_Cnm_Dp2), (p3_Cim_Dp2,Dselect1,p3_Cim_Dp3p1), (p3_Cim_Dp2,Dselect3,p3_Cim_Dpxp3),
           (p1_Cim_Dpxp3,Cmso,w_Cnm_Dpxp3), (p1_Cim_Dpxp3,Dp3s,p1_Cim_Dp3), (p2_Cim_Dpxp3,Cmso,w_Cnm_Dpxp3),
           (p2_Cim_Dpxp3,Dp3s,p2_Cim_Dp3), (p3_Cim_Dpxp3,Cmso,w_Cnm_Dpxp3), (p3_Cim_Dpxp3,Dp3s,p3_Cim_Dp3) },
      x_0: w_Cnm_Dp3,
      Xm: {w_Cnm_Dp1, w_Cnm_Dp2, w_Cnm_Dp3},
      ctrlEvents: {cp1, cp2, cp3, Cstart, Dselect1, Dselect2, Dselect3} )
 &
 seqS = {
      (w_Cnm_Dp3,w),(p1_Cnm_Dp3,p1),(w_Cnm_Dp3p1,w),
      (w_Cnm_Dp3p2,w),(w_Cim_Dp3,w),(p2_Cnm_Dp3,p2),
      (p3_Cnm_Dp3,p3),(p1_Cim_Dp3,p1),(p1_Cnm_Dp3p1,p1),
      (p1_Cnm_Dp3p2,p1),(w_Cim_Dp3p1,w),(p2_Cnm_Dp3p1,p2),
      (p3_Cnm_Dp3p1,p3),(w_Cnm_Dp1,w),(w_Cim_Dp3p2,w),
      (p2_Cnm_Dp3p2,p2),(p3_Cnm_Dp3p2,p3),(w_Cnm_Dp2,w),
      (p2_Cim_Dp3,p2),(p3_Cim_Dp3,p3),(p1_Cim_Dp3p1,p1),
      (p1_Cim_Dp3p2,p1),(p1_Cnm_Dp1,p1),(p1_Cnm_Dp2,p1),
      (p2_Cim_Dp3p1,p2),(p3_Cim_Dp3p1,p3),(w_Cim_Dp1,w),
      (p2_Cnm_Dp1,p2),(p3_Cnm_Dp1,p3),(w_Cnm_Dpxp3,w),
      (p2_Cim_Dp3p2,p2),(p3_Cim_Dp3p2,p3),(w_Cim_Dp2,w),
      (p2_Cnm_Dp2,p2),(p3_Cnm_Dp2,p3),(p1_Cim_Dp1,p1),
      (p1_Cim_Dp2,p1),(p1_Cnm_Dpxp3,p1),(p2_Cim_Dp1,p2),
      (p3_Cim_Dp1,p3),(w_Cim_Dpxp3,w),(p2_Cnm_Dpxp3,p2),
      (p3_Cnm_Dpxp3,p3),(p2_Cim_Dp2,p2),(p3_Cim_Dp2,p3),
      (p1_Cim_Dpxp3,p1),(p2_Cim_Dpxp3,p2),(p3_Cim_Dpxp3,p3) }
 &
 conveyor = {
      (w_Cnm_Dp3,Cnm),(p1_Cnm_Dp3,Cnm),(w_Cnm_Dp3p1,Cnm),
      (w_Cnm_Dp3p2,Cnm),(w_Cim_Dp3,Cim),(p2_Cnm_Dp3,Cnm),
      (p3_Cnm_Dp3,Cnm),(p1_Cim_Dp3,Cim),(p1_Cnm_Dp3p1,Cnm),
      (p1_Cnm_Dp3p2,Cnm),(w_Cim_Dp3p1,Cim),(p2_Cnm_Dp3p1,Cnm),
      (p3_Cnm_Dp3p1,Cnm),(w_Cnm_Dp1,Cnm),(w_Cim_Dp3p2,Cim),
      (p2_Cnm_Dp3p2,Cnm),(p3_Cnm_Dp3p2,Cnm),(w_Cnm_Dp2,Cnm),
      (p2_Cim_Dp3,Cim),(p3_Cim_Dp3,Cim),(p1_Cim_Dp3p1,Cim),
      (p1_Cim_Dp3p2,Cim),(p1_Cnm_Dp1,Cnm),(p1_Cnm_Dp2,Cnm),
      (p2_Cim_Dp3p1,Cim),(p3_Cim_Dp3p1,Cim),(w_Cim_Dp1,Cim),
      (p2_Cnm_Dp1,Cnm),(p3_Cnm_Dp1,Cnm),(w_Cnm_Dpxp3,Cnm),
      (p2_Cim_Dp3p2,Cim),(p3_Cim_Dp3p2,Cim),(w_Cim_Dp2,Cim),
      (p2_Cnm_Dp2,Cnm),(p3_Cnm_Dp2,Cnm),(p1_Cim_Dp1,Cim),
      (p1_Cim_Dp2,Cim),(p1_Cnm_Dpxp3,Cnm),(p2_Cim_Dp1,Cim),
      (p3_Cim_Dp1,Cim),(w_Cim_Dpxp3,Cim),(p2_Cnm_Dpxp3,Cnm),
      (p3_Cnm_Dpxp3,Cnm),(p2_Cim_Dp2,Cim),(p3_Cim_Dp2,Cim),
      (p1_Cim_Dpxp3,Cim),(p2_Cim_Dpxp3,Cim),(p3_Cim_Dpxp3,Cim) }
 &
 dispatcher = {
      (w_Cnm_Dp3,Dp3),(p1_Cnm_Dp3,Dp3),(w_Cnm_Dp3p1,Dp3p1),
      (w_Cnm_Dp3p2,Dp3p2),(w_Cim_Dp3,Dp3),(p2_Cnm_Dp3,Dp3),
      (p3_Cnm_Dp3,Dp3),(p1_Cim_Dp3,Dp3),(p1_Cnm_Dp3p1,Dp3p1),
      (p1_Cnm_Dp3p2,Dp3p2),(w_Cim_Dp3p1,Dp3p1),(p2_Cnm_Dp3p1,Dp3p1),
      (p3_Cnm_Dp3p1,Dp3p1),(w_Cnm_Dp1,Dp1),(w_Cim_Dp3p2,Dp3p2),
      (p2_Cnm_Dp3p2,Dp3p2),(p3_Cnm_Dp3p2,Dp3p2),(w_Cnm_Dp2,Dp2),
      (p2_Cim_Dp3,Dp3),(p3_Cim_Dp3,Dp3),(p1_Cim_Dp3p1,Dp3p1),
      (p1_Cim_Dp3p2,Dp3p2),(p1_Cnm_Dp1,Dp1),(p1_Cnm_Dp2,Dp2),
      (p2_Cim_Dp3p1,Dp3p1),(p3_Cim_Dp3p1,Dp3p1),(w_Cim_Dp1,Dp1),
      (p2_Cnm_Dp1,Dp1),(p3_Cnm_Dp1,Dp1),(w_Cnm_Dpxp3,Dpxp3),
      (p2_Cim_Dp3p2,Dp3p2),(p3_Cim_Dp3p2,Dp3p2),(w_Cim_Dp2,Dp2),
      (p2_Cnm_Dp2,Dp2),(p3_Cnm_Dp2,Dp2),(p1_Cim_Dp1,Dp1),
      (p1_Cim_Dp2,Dp2),(p1_Cnm_Dpxp3,Dpxp3),(p2_Cim_Dp1,Dp1),
      (p3_Cim_Dp1,Dp1),(w_Cim_Dpxp3,Dpxp3),(p2_Cnm_Dpxp3,Dpxp3),
      (p3_Cnm_Dpxp3,Dpxp3),(p2_Cim_Dp2,Dp2),(p3_Cim_Dp2,Dp2),
      (p1_Cim_Dpxp3,Dpxp3),(p2_Cim_Dpxp3,Dpxp3),(p3_Cim_Dpxp3,Dpxp3) }
 &
/*
  Standard constraints:
    1. The dispatcher cannot select the second or third position when the sorter must
       carry a piece to the first position.
    2. The dspatcher cannot select the first or third position when the sorter must
       carry a piece to the second position.
    3. The dispatcher cannot select the first or second position when the sorter must
       carry a piece to the third position.
    4. The conveyor and dispatcher cannot proceed when the sorter is at rest.
  Implicit constraint:
    5. The conveyor cannot attempt to carry a piece when the dispatcher has not
       completely selected a position.
    6. The conveyor cannot attempt to carry a piece as long as the configuration of
       the dispatcher does not comply with the choice of the sorter.
*/
 BadStates =
// The sorter carry a piece to the first position and the dispatcher has already
// selected the second or third position:
    {x | x : States & seqS(x) = p1 & dispatcher(x) : {Dp3p2,Dpxp3}} \/
// The sorter carry a piece to the second position and the dispatcher has already
// selected the first or third position:
    {x | x : States & seqS(x) = p2 & dispatcher(x) : {Dp3p1,Dpxp3}} \/
// The sorter carry a piece to the third position and the dispatcher has already
// selected the first or second position:
    {x | x : States & seqS(x) = p3 & dispatcher(x) : {Dp3p1,Dp3p2}} \/
// The sorter is waiting a command from another device to carry a piece and
// the conveyor is in motion or the dispatcher has already selected a position:
    {x | x : States & seqS(x) = w &
                      (conveyor(x) = Cim or dispatcher(x) : {Dp3p1,Dp3p2,Dpxp3}) } \/
// The conveyor is in motion and the dispatcher is not in a state where the gates are
// completely closed or opened:
    {x | x : States & conveyor(x) = Cim & dispatcher(x) : {Dp3p1,Dp3p2,Dpxp3}} \/
// The conveyor is in motion and the choice of the sorter is inconsistent with the one
// the dispatcher:
    {x | x : States & conveyor(x) = Cim & 
                      ((seqS(x) = p1 & dispatcher(x) : {Dp2, Dp3}) or
                       (seqS(x) = p2 & dispatcher(x) : {Dp1, Dp3}) or
                       (seqS(x) = p3 & dispatcher(x) : {Dp1, Dp2}) ) }
 &
 Qpred = States - BadStates
END

/*
OUTPUT:
The transition structure of the agent
 (i.e.,the solution of the control problem)
Number of states of the exhaustive model: 48
Number of transitions of the exhaustive model: 156
Cardinality of the predicate: 18
Supremal coreachable and controllable predicates (18):
 {w_Cnm_Dp3,p1_Cnm_Dp3,p2_Cnm_Dp3,p3_Cnm_Dp3,
  p1_Cnm_Dp3p1,w_Cnm_Dp1,p2_Cnm_Dp3p2,w_Cnm_Dp2,
  p3_Cim_Dp3,p1_Cnm_Dp1,p1_Cnm_Dp2,p2_Cnm_Dp1,
  p3_Cnm_Dp1,p2_Cnm_Dp2,p3_Cnm_Dp2,p1_Cim_Dp1,
  p3_Cnm_Dpxp3,p2_Cim_Dp2}
Disabled events:
 {(w_Cnm_Dp3|->{Cstart,Dselect1,Dselect2}),(p1_Cnm_Dp3|->{Cstart,Dselect2}),
  (p2_Cnm_Dp3|->{Cstart,Dselect1}),(p3_Cnm_Dp3|->{Dselect1,Dselect2}),
  (p1_Cnm_Dp3p1|->{Cstart}),(w_Cnm_Dp1|->{Cstart,Dselect2,Dselect3}),
  (p2_Cnm_Dp3p2|->{Cstart}),(w_Cnm_Dp2|->{Cstart,Dselect1,Dselect3}),
  (p3_Cim_Dp3|->{Dselect1,Dselect2}),(p1_Cnm_Dp1|->{Dselect2,Dselect3}),
  (p1_Cnm_Dp2|->{Cstart,Dselect3}),(p2_Cnm_Dp1|->{Cstart,Dselect3}),
  (p3_Cnm_Dp1|->{Cstart,Dselect2}),(p2_Cnm_Dp2|->{Dselect1,Dselect3}),
  (p3_Cnm_Dp2|->{Cstart,Dselect1}),(p1_Cim_Dp1|->{Dselect2,Dselect3}),
  (p3_Cnm_Dpxp3|->{Cstart}),(p2_Cim_Dp2|->{Dselect1,Dselect3})}
Enabled events:
 {(w_Cnm_Dp3|->{cp1,cp2,cp3}),(p1_Cnm_Dp3|->{Dselect1}),
  (p2_Cnm_Dp3|->{Dselect2}),(p3_Cnm_Dp3|->{Cstart}),
  (p1_Cnm_Dp3p1|->{Dp1s}),(w_Cnm_Dp1|->{cp1,cp2,cp3}),
  (p2_Cnm_Dp3p2|->{Dp2s}),(w_Cnm_Dp2|->{cp1,cp2,cp3}),
  (p3_Cim_Dp3|->{Cmso}),(p1_Cnm_Dp1|->{Cstart}),
  (p1_Cnm_Dp2|->{Dselect1}),(p2_Cnm_Dp1|->{Dselect2}),
  (p3_Cnm_Dp1|->{Dselect3}),(p2_Cnm_Dp2|->{Cstart}),
  (p3_Cnm_Dp2|->{Dselect3}),(p1_Cim_Dp1|->{Cmso}),
  (p3_Cnm_Dpxp3|->{Dp3s}),(p2_Cim_Dp2|->{Cmso})}
Closed-loop system:
 rec(Xm:{w_Cnm_Dp3,w_Cnm_Dp1,w_Cnm_Dp2},
     ctrlEvents:{cp1,cp2,cp3,Cstart,Dselect1,Dselect2,Dselect3},
     events:{cp1,cp2,cp3,Cmso,Dp1s,Dp2s,Dp3s,Cstart,Dselect1,Dselect2,Dselect3},
     states:{w_Cnm_Dp3,p1_Cnm_Dp3,p2_Cnm_Dp3,p3_Cnm_Dp3,
             p1_Cnm_Dp3p1,w_Cnm_Dp1,p2_Cnm_Dp3p2,w_Cnm_Dp2,
             p3_Cim_Dp3,p1_Cnm_Dp1,p1_Cnm_Dp2,p2_Cnm_Dp1,
             p3_Cnm_Dp1,p2_Cnm_Dp2,p3_Cnm_Dp2,p1_Cim_Dp1,
             p3_Cnm_Dpxp3,p2_Cim_Dp2},
     tf:{((w_Cnm_Dp3|->cp1)|->p1_Cnm_Dp3),
         ((w_Cnm_Dp3|->cp2)|->p2_Cnm_Dp3),
         ((w_Cnm_Dp3|->cp3)|->p3_Cnm_Dp3),
         ((p1_Cnm_Dp3|->Dselect1)|->p1_Cnm_Dp3p1),
         ((p2_Cnm_Dp3|->Dselect2)|->p2_Cnm_Dp3p2),
         ((p3_Cnm_Dp3|->Cstart)|->p3_Cim_Dp3),
         ((p1_Cnm_Dp3p1|->Dp1s)|->p1_Cnm_Dp1),
         ((w_Cnm_Dp1|->cp1)|->p1_Cnm_Dp1),
         ((w_Cnm_Dp1|->cp2)|->p2_Cnm_Dp1),
         ((w_Cnm_Dp1|->cp3)|->p3_Cnm_Dp1),
         ((p2_Cnm_Dp3p2|->Dp2s)|->p2_Cnm_Dp2),
         ((w_Cnm_Dp2|->cp1)|->p1_Cnm_Dp2),
         ((w_Cnm_Dp2|->cp2)|->p2_Cnm_Dp2),
         ((w_Cnm_Dp2|->cp3)|->p3_Cnm_Dp2),
         ((p3_Cim_Dp3|->Cmso)|->w_Cnm_Dp3),
         ((p1_Cnm_Dp1|->Cstart)|->p1_Cim_Dp1),
         ((p1_Cnm_Dp2|->Dselect1)|->p1_Cnm_Dp3p1),
         ((p2_Cnm_Dp1|->Dselect2)|->p2_Cnm_Dp3p2),
         ((p3_Cnm_Dp1|->Dselect3)|->p3_Cnm_Dpxp3),
         ((p2_Cnm_Dp2|->Cstart)|->p2_Cim_Dp2),
         ((p3_Cnm_Dp2|->Dselect3)|->p3_Cnm_Dpxp3),
         ((p1_Cim_Dp1|->Cmso)|->w_Cnm_Dp1),
         ((p3_Cnm_Dpxp3|->Dp3s)|->p3_Cnm_Dp3),
         ((p2_Cim_Dp2|->Cmso)|->w_Cnm_Dp2)},
     x_0:w_Cnm_Dp3)
*/

/*
  ControlLaw(cp1) = w
  ControlLaw(cp2) = w
  ControlLaw(cp3) = w
  ControlLaw(Cstart) = Cnm & ((p1 & Dp1) | (p2 & Dp2) | (p3 & Dp3))
  ControlLaw(Dselect1) = p1 & (Dp2 | Dp3)
  ControlLaw(Dselect2) = p2 & (Dp1 | Dp3)
  ControlLaw(Dselect3) = p3 & (Dp1 | Dp2)
*/

/*
% Runtime for SOLUTION for SETUP_CONSTANTS: 120 ms (walltime: 120 ms)
% Finished searching for valid initial states 140 ms walltime (140 ms runtime), since start: 1820 ms
% Starting random_animate (max_steps=5)
perform_random_step(1,1)
random(2,1)
perform_random_step(1,2)
random(2,1)
perform_random_step(1,3)
random(2,1)
"Compute supCCP..."
"Compute state feedback functions..."
"Save results..."
perform_random_step(1,4)
random(2,1)
"The control is nonconflicting."

ALL OPERATIONS COVERED

perform_random_step(1,5)
random(2,1)
finished_random_animate(5,[runtime/350,total_runtime/350,walltime/350])
% Finished random_animate (errors=0, warnings=0)
% Finished processing file after 550 ms walltime (530 ms runtime), since start: 2180 ms

% Finished process_file (errors=0, warnings=0)
*/