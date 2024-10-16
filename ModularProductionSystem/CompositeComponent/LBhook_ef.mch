// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBhook_ef
/*
  An extendable hook with a failure/recovery mode.
  The hypothesis of a failure when dropping a piece is ruled out.
  Two cases are possible when trying to grab a piece.
  First, if a piece is in place, the winch will never reach its fully extended
  position. It is therefore necessary to make vacuum of the pneumatic suction
  cup just after initiating the descent of the winch so that confirmation of
  full vacuum can signal the success of the operation.
  Second, if no piece is in place, there will be no full vacuum and the winch will
  reach its fully extended position. In that case the operation fails.
  In this logic, the descent must be cancelled in the case of success (normal mode)
  and vacuum must be cancelled in the case of failure (recovery mode).
  A sequencer memorizes normal and recovery modes.
*/

SETS
/*
  seq states:
     drpStg := drop stage
    failStg := failure stage
     grbStg := grab stage
  winch states:
     We := winch completely extended
    Wie := winch in extension
    Wir := winch in retraction
     Wr := winch completely retracted
  hook states:
     Hl := hook loosing air
    Hnv := no vacuum
     Hp := hook pumpimg air
     Hv := vacuum

  hook_ef states (interface):
    aWp := a workpiece
    dWp := dropping a workpiece
    gWp := grabbing a workpiece
    nWp := no workpiece (after completely dropping a workpiece)
     rp := recovery process (no workpiece to grab)
     wo := waiting option
*/
 StatesG_A = {drpStg_We_Hnv, drpStg_Wr_Hv, drpStg_Wir_Hnv, drpStg_Wie_Hv, drpStg_We_Hv,
              drpStg_We_Hl,
              grbStg_Wr_Hnv, grbStg_Wie_Hnv, grbStg_Wie_Hp, grbStg_Wie_Hv, grbStg_Wir_Hv,
              failStg_Wir_Hnv, failStg_We_Hp, failStg_Wr_Hnv, failStg_We_Hv,
              failStg_We_Hnv, failStg_We_Hl };
 StatesG_I = {HaWp, HdWp, HgWp, HnWp, Hrp, Hwo};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <seqH, winch, hook>

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(clr) = failStg & Wr
      ControlLaw(W_cd) = grbStg & Wie & Hv
      ControlLaw(W_gd) = (grbStg | drpStg) & Wr
      ControlLaw(W_gu) = We & Hnv
      ControlLaw(H_cg) =failStg & Hp
      ControlLaw(H_grab) = Wie & Hnv
      ControlLaw(H_drop) = We & Hv

    Observer:
      Project_W_clr() := { return Func_rst(); }
      Project_W_cd() := { return Func_confirm(); }
      Project_W_eoa() := {
        if ( grbStg ) then return Func_eog()
                      else if ( drpStg ) then return Func_eod();
                                         else return true(); }
      Project_W_eod() :=
        if ( grbStg ) then return Func_abort()
                      else return true(); }
      Project_W_gd() :=  {
        if ( grbStg ) then return Func_grab()
                      else return Func_drop(); }
*/

/*
  Commands and signals:
    seq event:                         hook_ef events:
      clr (synthetic event)              rst := reset
    winch events:
       W_cd := cancel descent            confirm (grabbing)
      W_eoa := end of ascent             eod/eog := end of dropping/grabbing
      W_eod := end of descent            abort
       W_gd := go down                   drop/grab
       W_gu := go up
    hook events:
        H_cg := cancel grab
      H_drop := drop a piece
       H_eod := end of dropping
       H_eog := end of grabbing
      H_grab := grab a piece
*/
 Input = {clr, W_cd, W_eoa, W_eod, W_gd, W_gu, H_cg, H_drop, H_eod, H_eog, H_grab};
 Output = {abort, confirm, drop, eod, eog, grab, rst, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 //G_FB,         Deterministic automaton exhaustively modeling the device behavior
 //              (defined in file SBhook_ef)
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
            tf:{(grbStg_Wr_Hnv,W_gd,grbStg_Wie_Hnv),   (grbStg_Wie_Hnv,H_grab,grbStg_Wie_Hp),
                (grbStg_Wie_Hp,H_eog,grbStg_Wie_Hv),   (grbStg_Wie_Hv,W_cd,grbStg_Wir_Hv),
                (grbStg_Wir_Hv,W_eoa,drpStg_Wr_Hv),
                (drpStg_Wr_Hv,W_gd,drpStg_Wie_Hv),     (drpStg_Wie_Hv,W_eod,drpStg_We_Hv),
                (drpStg_We_Hv,H_drop,drpStg_We_Hl),    (drpStg_We_Hl,H_eod,drpStg_We_Hnv),
                (drpStg_We_Hnv,W_gu,drpStg_Wir_Hnv),   (drpStg_Wir_Hnv,W_eoa,grbStg_Wr_Hnv),

                (grbStg_Wie_Hnv,W_eod,failStg_We_Hnv),
                (grbStg_Wie_Hp,W_eod,failStg_We_Hp),
                (failStg_We_Hp,H_eog,failStg_We_Hv),   (failStg_We_Hp,H_cg,failStg_We_Hnv),
                (grbStg_Wie_Hv,W_eod,failStg_We_Hv),
                (failStg_We_Hv,H_drop,failStg_We_Hl),  (failStg_We_Hl,H_eod,failStg_We_Hnv),
                (failStg_We_Hnv,W_gu,failStg_Wir_Hnv), (failStg_Wir_Hnv,W_eoa,failStg_Wr_Hnv),
                (failStg_Wr_Hnv,clr,grbStg_Wr_Hnv) },
            x_0: grbStg_Wr_Hnv,
            Xm: {grbStg_Wr_Hnv},
            ctrlEvents: {clr, W_cd, W_gd, W_gu, H_cg, H_drop, H_grab} ) &
 bound_G_ts = 11
 &
 G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,
           tf:{(grbStg_Wr_Hnv,(W_gd,grab),grbStg_Wie_Hnv),          (grbStg_Wie_Hnv,(H_grab,silentEvent),grbStg_Wie_Hp),
               (grbStg_Wie_Hp,(H_eog,silentEvent),grbStg_Wie_Hv),   (grbStg_Wie_Hv,(W_cd,confirm),grbStg_Wir_Hv),
               (grbStg_Wir_Hv,(W_eoa,eog),drpStg_Wr_Hv),
               (drpStg_Wr_Hv,(W_gd,drop),drpStg_Wie_Hv),            (drpStg_Wie_Hv,(W_eod,silentEvent),drpStg_We_Hv),
               (drpStg_We_Hv,(H_drop,silentEvent),drpStg_We_Hl),    (drpStg_We_Hl,(H_eod,silentEvent),drpStg_We_Hnv),
               (drpStg_We_Hnv,(W_gu,silentEvent),drpStg_Wir_Hnv),   (drpStg_Wir_Hnv,(W_eoa,eod),grbStg_Wr_Hnv),

               (grbStg_Wie_Hnv,(W_eod,abort),failStg_We_Hnv),
               (grbStg_Wie_Hp,(W_eod,abort),failStg_We_Hp),
               (failStg_We_Hp,(H_eog,silentEvent),failStg_We_Hv),   (failStg_We_Hp,(H_cg,silentEvent),failStg_We_Hnv),
               (grbStg_Wie_Hv,(W_eod,abort),failStg_We_Hv),
               (failStg_We_Hv,(H_drop,silentEvent),failStg_We_Hl),  (failStg_We_Hl,(H_eod,silentEvent),failStg_We_Hnv),
               (failStg_We_Hnv,(W_gu,silentEvent),failStg_Wir_Hnv), (failStg_Wir_Hnv,(W_eoa,silentEvent),failStg_Wr_Hnv),
               (failStg_Wr_Hnv,(clr,rst),grbStg_Wr_Hnv) },
            x_0: grbStg_Wr_Hnv,
            Xm: {grbStg_Wr_Hnv},
            ctrlEvents: {clr, W_cd, W_gd, W_gu, H_cg, H_drop, H_grab} ) &
 bound_G_A = 11
 &
 G_A'ctrlEvents = G_ts'ctrlEvents
 &
 G_I = rec(states: StatesG_I,
           events: Output - {silentEvent},
           tf: {(HnWp,grab,Hwo),  (Hwo,confirm,HgWp), (HgWp,eog,HaWp),
                (HaWp,drop,HdWp), (HdWp,eod,HnWp), 
                                  (Hwo,abort,Hrp), (Hrp,rst,HnWp) },
           x_0: HnWp,
           Xm: {HnWp},
           ctrlEvents: {drop, grab, rst} ) &
 bound_G_I = 5
END

/*
OUTPUT
% Runtime for SOLUTION for SETUP_CONSTANTS: 240 ms (walltime: 250 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2620 ms
Number of states of the transition system 17
Number of transitions of the transition system: 21
Size of the prefix-closed language of the transition system: 33
Size of the marked language of the transition system: 6
Number of states of the agent: 17
Number of transitions of the agent: 21
Size of the prefix-closed language of the agent: 33
Size of the marked language of the agent: 6
Number of states of the interface: 6
Number of transitions the interface: 7
Size of the prefix-closed language of the interface: 8
Size of the marked language of the interface: 3
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2630 ms
"Marking is consistent."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2630 ms
"The causal map is an observer."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2840 ms
"Control coincidence property is satisfied."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2860 ms
"The interface is nonblocking."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2860 ms
Deadlock reached after 7 steps (after Check_NonBlocking_Interface).

% Finished processing file after 660 ms walltime (580 ms runtime), since start: 2880 ms
*/