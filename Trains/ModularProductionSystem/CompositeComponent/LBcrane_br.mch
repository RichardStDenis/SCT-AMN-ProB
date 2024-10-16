// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBcrane_br
/*
  This crane is a base rail crane driven by a servomotor, which picks up a
  piece from three possible entry positions (end positions of the sorting
  station) and moves it to a unique exit position (the top a feed magazine),
  where it is put down.
*/

SETS
/*
  boom states:                                   crane state (interface):
      BMatpos0 := boom at position 0
    BMatpos123 := boom at postion 1, 2 or 3
         BMnr0 := boom near position 0
       BMnr123 := boom near position 1, 2, or 3
  bridge states:
     BRe := bridge completely extended             the bridge is aligned on the output position
    BRie := bridge in extension
    BRir := bridge in retraction
     BRr := bridge completely retracted            the bridge is aligned on the input positions
  winch states:
     We := winch completely extended               the winch is at the level of one of input positions
    Wie := winch in extension
    Wir := winch in retraction
     Wr := winch completely retracted              the winch is at the output position level
  hook states:
     He := hook completely extended                the hook is closed
    Hie := hook in extension
    Hir := hook in retraction
     Hr := hook completely retracted               the hook is open

  crane_br states (interface):
    idle
    busy
*/
 StatesG_A = {BMatpos0_BRr_Wr_Hr,    BMnr123_BRr_Wr_Hr,     BMatpos123_BRr_Wr_Hr,
              BMatpos123_BRr_Wie_Hr, BMatpos123_BRr_We_Hr,  BMatpos123_BRr_We_Hie,
              BMatpos123_BRr_We_He,  BMatpos123_BRr_Wir_He, BMatpos123_BRr_Wr_He,
              BMnr0_BRr_Wr_He,       BMatpos0_BRr_Wr_He,    BMatpos0_BRie_Wr_He,
              BMatpos0_BRe_Wr_He,    BMatpos0_BRe_Wr_Hir,   BMatpos0_BRe_Wr_Hr,
              BMatpos0_BRir_Wr_Hr };
 StatesG_I = {CBRbusy, CBRidle};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <boom, bridge, winch, hook>

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(BM_mb) = BMatpos123 & Wr & He
      ControlLaw(BM_mf1) = BMatpos0 & BRr & Hr
      ControlLaw(BM_mf2) = BMatpos0 & BRr & Hr
      ControlLaw(BM_mf3) = BMatpos0 & BRr & Hr
      ControlLaw(BR_expand) = BMatpos0 & BRr & He
      ControlLaw(BR_contract) = BRe & Hr
      ControlLaw(W_gd) = BMatpos123 & Wr & Hr
      ControlLaw(W_gu) = We & He
      ControlLaw(H_grab) = We & Hr
      ControlLaw(H_release) = BRe & He

    Observer:
      Project_BM_mf1() := { return Func_pck1(); }
      Project_BM_mf2() := { return Func_pck2(); }
      Project_BM_mf3() := { return Func_pck3(); }
      Project_W_eoa() := { return Func_eop(); }
*/

/*
  Commands and signals:
    boom events:                                 crane events (interface):
      BM_eomb := end of moving backward
      BM_eomf := end of moving forward
        BM_mb := move backward (to position 0)
       BM_mf1 := move forward to position 1        pck1 := pick up a piece at position 1
       BM_mf2 := move forward to position 2        pck2 := pick up a piece at position 2
       BM_mf3 := move forward to position 3        pck2 := pick up a piece at position 3
    bridge events:
           BR_eoe := end of expansion
           BR_eoc := end of contraction
        BR_expand := expand
      BR_contract := contract
    winch events:
      W_eoa := end of ascent                       eop := end of pick up
      W_eod := end of descent
       W_gd := go down
       W_gu := go up
    hook events:
          H_eog := end of grabbing
          H_eor := end of releasing
         H_grab := grab
      H_release := release

  The event ``eot'' (end of transfer) could be replaced by the event ``eop'' (end of picking
  up a piece) for consistency of marking. Then theta(BR_eoc) = eot and theta(W_eoa) = silentEvent.
*/
 Input = {BM_eomb, BM_eomf, BM_mb, BM_mf1, BM_mf2, BM_mf3,
          BR_contract, BR_eoc, BR_eoe, BR_expand,
          W_eoa, W_eod, W_gd, W_gu,
          H_eog, H_eor, H_grab, H_release};
 Output = {eop, pck1, pck2, pck3, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 //G_FB,         Deterministic automaton exhaustively modeling the device behavior
 //              (defined in file SBcrane_br)
 G_ts,        // G_FB under control of a state feedback function (solution of a control problem)
 bound_G_ts,  // Bound on the trace length of the controlled device behavior
 G_A,         // Mealy machine modeling the device implementation (agent)
 bound_G_A,   // Bound on the trace length of the agent
 G_I,         // (quotient deterministic) automaton modeling a device abstraction (interface)
 bound_G_I    // Bound on the trace length of the interface

PROPERTIES
 VocalOutput = Output - {silentEvent}
 &
 G_ts = rec(states: StatesG_A,
            events: Input,
            tf:{(BMatpos0_BRr_Wr_Hr,BM_mf1,BMnr123_BRr_Wr_Hr),
                (BMatpos0_BRr_Wr_Hr,BM_mf2,BMnr123_BRr_Wr_Hr),
                (BMatpos0_BRr_Wr_Hr,BM_mf3,BMnr123_BRr_Wr_Hr),
                (BMnr123_BRr_Wr_Hr,BM_eomf,BMatpos123_BRr_Wr_Hr),
                (BMatpos123_BRr_Wr_Hr,W_gd,BMatpos123_BRr_Wie_Hr),
                (BMatpos123_BRr_Wie_Hr,W_eod,BMatpos123_BRr_We_Hr),
                (BMatpos123_BRr_We_Hr,H_grab,BMatpos123_BRr_We_Hie),
                (BMatpos123_BRr_We_Hie,H_eog,BMatpos123_BRr_We_He),
                (BMatpos123_BRr_We_He,W_gu,BMatpos123_BRr_Wir_He),
                (BMatpos123_BRr_Wir_He,W_eoa,BMatpos123_BRr_Wr_He),
                (BMatpos123_BRr_Wr_He,BM_mb,BMnr0_BRr_Wr_He),
                (BMnr0_BRr_Wr_He,BM_eomb,BMatpos0_BRr_Wr_He),
                (BMatpos0_BRr_Wr_He,BR_expand,BMatpos0_BRie_Wr_He),
                (BMatpos0_BRie_Wr_He,BR_eoe,BMatpos0_BRe_Wr_He),
                (BMatpos0_BRe_Wr_He,H_release,BMatpos0_BRe_Wr_Hir),
                (BMatpos0_BRe_Wr_Hir,H_eor,BMatpos0_BRe_Wr_Hr),
                (BMatpos0_BRe_Wr_Hr,BR_contract,BMatpos0_BRir_Wr_Hr),
                (BMatpos0_BRir_Wr_Hr,BR_eoc,BMatpos0_BRr_Wr_Hr) },
            x_0: BMatpos0_BRr_Wr_Hr,
            Xm: {BMatpos0_BRr_Wr_Hr},
            ctrlEvents: {BM_mb, BM_mf1, BM_mf2, BM_mf3, BR_contract, BR_expand,
                         W_gd, W_gu, H_grab, H_release } ) &
 bound_G_ts = 16
 & 
 G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,
           tf:{(BMatpos0_BRr_Wr_Hr,(BM_mf1,pck1),BMnr123_BRr_Wr_Hr),
               (BMatpos0_BRr_Wr_Hr,(BM_mf2,pck2),BMnr123_BRr_Wr_Hr),
               (BMatpos0_BRr_Wr_Hr,(BM_mf3,pck3),BMnr123_BRr_Wr_Hr),
               (BMnr123_BRr_Wr_Hr,(BM_eomf,silentEvent),BMatpos123_BRr_Wr_Hr),
               (BMatpos123_BRr_Wr_Hr,(W_gd,silentEvent),BMatpos123_BRr_Wie_Hr),
               (BMatpos123_BRr_Wie_Hr,(W_eod,silentEvent),BMatpos123_BRr_We_Hr),
               (BMatpos123_BRr_We_Hr,(H_grab,silentEvent),BMatpos123_BRr_We_Hie),
               (BMatpos123_BRr_We_Hie,(H_eog,silentEvent),BMatpos123_BRr_We_He),
               (BMatpos123_BRr_We_He,(W_gu,silentEvent),BMatpos123_BRr_Wir_He),
               (BMatpos123_BRr_Wir_He,(W_eoa,eop),BMatpos123_BRr_Wr_He),
               (BMatpos123_BRr_Wr_He,(BM_mb,silentEvent),BMnr0_BRr_Wr_He),
               (BMnr0_BRr_Wr_He,(BM_eomb,silentEvent),BMatpos0_BRr_Wr_He),
               (BMatpos0_BRr_Wr_He,(BR_expand,silentEvent),BMatpos0_BRie_Wr_He),
               (BMatpos0_BRie_Wr_He,(BR_eoe,silentEvent),BMatpos0_BRe_Wr_He),
               (BMatpos0_BRe_Wr_He,(H_release,silentEvent),BMatpos0_BRe_Wr_Hir),
               (BMatpos0_BRe_Wr_Hir,(H_eor,silentEvent),BMatpos0_BRe_Wr_Hr),
               (BMatpos0_BRe_Wr_Hr,(BR_contract,silentEvent),BMatpos0_BRir_Wr_Hr),
               (BMatpos0_BRir_Wr_Hr,(BR_eoc,silentEvent),BMatpos0_BRr_Wr_Hr) },
           x_0: BMatpos0_BRr_Wr_Hr,
           Xm: {BMatpos0_BRr_Wr_Hr},
           ctrlEvents: {BM_mb, BM_mf1, BM_mf2, BM_mf3, BR_contract, BR_expand,
                        W_gd, W_gu, H_grab, H_release } ) &
 bound_G_A = 16
 &
 G_I = rec(states: StatesG_I,
           events: Output - {silentEvent},
           tf: {(CBRidle,pck1,CBRbusy), (CBRidle,pck2,CBRbusy), (CBRidle,pck3,CBRbusy),
                (CBRbusy,eop,CBRidle)},
           x_0: CBRidle,
           Xm: {CBRidle},
           ctrlEvents: {pck1, pck2, pck3} ) &
 bound_G_I = 2
END

/*
OUTPUT:
% Runtime for SOLUTION for SETUP_CONSTANTS: 300 ms (walltime: 300 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2260 ms
Number of states of the transition system 16
Number of transitions of the transition system: 18
Size of the prefix-closed language of the transition system: 49
Size of the marked language of the transition system: 4
Number of states of the agent: 16
Number of transitions of the agent: 18
Size of the prefix-closed language of the agent: 49
Size of the marked language of the agent: 4
Number of states of the interface: 2
Number of transitions the interface: 4
Size of the prefix-closed language of the interface: 7
Size of the marked language of the interface: 4
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2270 ms
"Marking is weakly consistent."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 3650 ms
"The causal map is an observer."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 3830 ms
"Control coincidence property is satisfied."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 3840 ms
"The interface is nonblocking."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 3840 ms
Deadlock reached after 7 steps (after Check_NonBlocking_Interface).

% Finished processing file after 2080 ms walltime (1950 ms runtime), since start: 3860 ms
*/