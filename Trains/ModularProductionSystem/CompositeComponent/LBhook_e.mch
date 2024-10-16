// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBhook_e
/*
  An extendable hook without a failure/recovery mode.
  This component constitutes a hoist of a certain form of crane. A hook (suction cup)
  is attached to the end of a winch to allow for extension and retraction when
  grabbing or dropping a piece, respectively.
*/

SETS
/*
  seq states (MW, a memory word):
    drpStg := drop stage
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

  hook_e states (interface):
    aWp := a workpiece
    dWp := dropping a workpiece
    gWp := grabbing a workpiece
    nWp := no workpiece
*/
 StatesG_A = {grbStg_Wr_Hnv, grbStg_Wie_Hnv, grbStg_We_Hnv,  grbStg_We_Hp,
              grbStg_We_Hv,  grbStg_Wir_Hv,  drpStg_Wr_Hv,   drpStg_Wie_Hv,
              drpStg_We_Hv,  drpStg_We_Hl,   drpStg_We_Hnv,  drpStg_Wir_Hnv };
 StatesG_I = {HaWp, HdWp, HgWp, HnWp};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <seqH, winch, hook>

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(W_gd) = Wr
      ControlLaw(W_gu) = We & ((drpStg & Hnv) | (grbStg & Hv))
      ControlLaw(H_drop) = drpStg & We & Hv
      ControlLaw(H_grab) = grbStg & We & Hnv

    Observer:
      Project_W_eoa() := {
        if ( grbStg ) then return Func_eog()
                      else return Func_eod(); }
      Project_W_gd() :=  {
        if ( grbStg ) then return Func_grab()
                      else return Func_drop(); }
*/

/*
  Commands and signals:
    winch events:                      hook_e:
      W_eoa := end of ascent             eod/eog := end of dropping/grabbing
      W_eod := end of descent
       W_gd := go down                   drop/grab := drop/grab a workpiece
       W_gu := go up
    hook events :
      H_drop := drop a piece
       H_eod := end of dropping
       H_eog := end of grabbing
      H_grab := grab a piece
*/
 Input = {W_eoa, W_eod, W_gd, W_gu, H_drop, H_eod, H_eog, H_grab};
 Output = {drop, eod, eog, grab, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 //G_FB,         Deterministic automaton exhaustively modeling the device behavior
 //              (defined in file SBhook_e)
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
            tf:{(grbStg_Wr_Hnv,W_gd,grbStg_Wie_Hnv),
                (grbStg_Wie_Hnv,W_eod,grbStg_We_Hnv),
                (grbStg_We_Hnv,H_grab,grbStg_We_Hp),
                (grbStg_We_Hp,H_eog,grbStg_We_Hv),
                (grbStg_We_Hv,W_gu,grbStg_Wir_Hv),
                (grbStg_Wir_Hv,W_eoa,drpStg_Wr_Hv),
                (drpStg_Wr_Hv,W_gd,drpStg_Wie_Hv),
                (drpStg_Wie_Hv,W_eod,drpStg_We_Hv),
                (drpStg_We_Hv,H_drop,drpStg_We_Hl),
                (drpStg_We_Hl,H_eod,drpStg_We_Hnv),
                (drpStg_We_Hnv,W_gu,drpStg_Wir_Hnv),
                (drpStg_Wir_Hnv,W_eoa,grbStg_Wr_Hnv) },
            x_0: grbStg_Wr_Hnv,
            Xm: {grbStg_Wr_Hnv},
            ctrlEvents: {H_drop, H_grab, W_gd, W_gu} ) &
 bound_G_ts = 12
 & 
 G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,
            tf:{(grbStg_Wr_Hnv,(W_gd,grab),grbStg_Wie_Hnv),
                (grbStg_Wie_Hnv,(W_eod,silentEvent),grbStg_We_Hnv),
                (grbStg_We_Hnv,(H_grab,silentEvent),grbStg_We_Hp),
                (grbStg_We_Hp,(H_eog,silentEvent),grbStg_We_Hv),
                (grbStg_We_Hv,(W_gu,silentEvent),grbStg_Wir_Hv),
                (grbStg_Wir_Hv,(W_eoa,eog),drpStg_Wr_Hv),
                (drpStg_Wr_Hv,(W_gd,drop),drpStg_Wie_Hv),
                (drpStg_Wie_Hv,(W_eod,silentEvent),drpStg_We_Hv),
                (drpStg_We_Hv,(H_drop,silentEvent),drpStg_We_Hl),
                (drpStg_We_Hl,(H_eod,silentEvent),drpStg_We_Hnv),
                (drpStg_We_Hnv,(W_gu,silentEvent),drpStg_Wir_Hnv),
                (drpStg_Wir_Hnv,(W_eoa,eod),grbStg_Wr_Hnv) },
           x_0: grbStg_Wr_Hnv,
           Xm: {grbStg_Wr_Hnv},
           ctrlEvents: {H_drop, H_grab, W_gd, W_gu} ) &
 bound_G_A = 12
 &
 G_I = rec(states: StatesG_I,
           events: Output - {silentEvent},
           tf: {(HnWp,grab,HgWp), (HgWp,eog,HaWp),
                (HaWp,drop,HdWp), (HdWp,eod,HnWp)},
           x_0: HnWp,
           Xm: {HnWp},
           ctrlEvents: {drop, grab} ) &
 bound_G_I = 4
END

/*
OUTPUT:
% Runtime for SOLUTION for SETUP_CONSTANTS: 210 ms (walltime: 210 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2110 ms
Number of states of the transition system 12
Number of transitions of the transition system: 12
Size of the prefix-closed language of the transition system: 13
Size of the marked language of the transition system: 2
Number of states of the agent: 12
Number of transitions of the agent: 12
Size of the prefix-closed language of the agent: 13
Size of the marked language of the agent: 2
Number of states of the interface: 4
Number of transitions the interface: 4
Size of the prefix-closed language of the interface: 5
Size of the marked language of the interface: 2
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2120 ms
"Marking is consistent."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2120 ms
"The causal map is an observer."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2180 ms
"Control coincidence property is satisfied."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2190 ms
"The interface is nonblocking."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2190 ms
Deadlock reached after 7 steps (after Check_NonBlocking_Interface).

% Finished processing file after 430 ms walltime (380 ms runtime), since start: 2200 ms

*/