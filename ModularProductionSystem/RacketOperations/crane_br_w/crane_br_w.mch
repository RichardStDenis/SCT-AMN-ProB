// copyright Richard St_Denis, Universite de Sherbrooke, 2023.
MODEL SBcrane_br_w

SETS
 States = {w_Cidle, p1_Cbusy, p2_Cbusy,
           p3_Cbusy };
 Events = {C_eot, C_pck1, C_pck2, C_pck3};
 LocalStates = {p1, p2, p3, w, Cbusy, Cidle}

CONSTANTS
 G_FB,
 seqC, crane,
 Qpred, BadStates

PROPERTIES
 G_FB = rec(       // Automaton modeling the free behavior
            states: States,
            events: Events,
            tf: {(w_Cidle,C_pck1,p1_Cbusy), (w_Cidle,C_pck2,p2_Cbusy), (w_Cidle,C_pck3,p3_Cbusy),
                 (p1_Cbusy,C_eot,w_Cidle), (p2_Cbusy,C_eot,w_Cidle), (p3_Cbusy,C_eot,w_Cidle) },
            x_0: w_Cidle,
            Xm: {w_Cidle},
            ctrlEvents: {C_pck1, C_pck2, C_pck3} )
 &
 seqC = {
      (w_Cidle,w),(p1_Cbusy,p1),(p2_Cbusy,p2),
      (p3_Cbusy,p3) }
 &
 crane = {
      (w_Cidle,Cidle),(p1_Cbusy,Cbusy),(p2_Cbusy,Cbusy),
      (p3_Cbusy,Cbusy) }
 &
 BadStates =  States    // to be completed
 &
 Qpred = States - BadStates
END