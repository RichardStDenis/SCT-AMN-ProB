// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBhandling
/*
  The handling station consists of a sorter, a crane and three buffers, each of capacity
  one, which correspond to three slots which accepting a piece. The empty buffer closest
  to the processing station should always be filled first by the sorter device and the
  full buffer farthest from the processing station is emptied first by the crane.
*/

SETS
/*
  sorter states:
    Snpt := no position targeted
    Sp1t := first position targeted
    Sp2t := second position targeted
    Sp3t := third position targeted
  buffers states:
    awp := a workpiece in the buffer
    nwp := no workpiece in the buffer
  crane states:
    Cp1 := picking-up a piece at the first position
    Cp2 := picking-up a piece at the second position
    Cp3 := picking-up a piece at the third position
     Cw := waiting

  handling station states (interface):
    busy
    idle

*/
 StatesG_A = {Snpt_nwp_nwp_nwp_Cw,Sp1t_nwp_nwp_nwp_Cw,Snpt_awp_nwp_nwp_Cw,
              Snpt_awp_nwp_nwp_Cp1,Sp2t_awp_nwp_nwp_Cw,Sp2t_awp_nwp_nwp_Cp1,
              Snpt_awp_awp_nwp_Cw,Snpt_awp_awp_nwp_Cp2,Snpt_awp_awp_nwp_Cp1,
              Sp2t_nwp_nwp_nwp_Cw,Sp3t_awp_awp_nwp_Cw,Sp3t_awp_awp_nwp_Cp2,
              Snpt_nwp_awp_nwp_Cw,Sp3t_awp_awp_nwp_Cp1,Snpt_awp_awp_awp_Cw,
              Snpt_awp_awp_awp_Cp3,Snpt_awp_awp_awp_Cp2,Sp3t_awp_nwp_nwp_Cw,
              Sp1t_nwp_awp_nwp_Cw,Snpt_nwp_awp_nwp_Cp2,Snpt_awp_awp_awp_Cp1,
              Sp3t_nwp_awp_nwp_Cw,Snpt_awp_nwp_awp_Cw,Sp3t_awp_nwp_nwp_Cp1,
              Sp1t_nwp_awp_nwp_Cp2,Snpt_nwp_awp_awp_Cw,Sp3t_nwp_awp_nwp_Cp2,
              Snpt_awp_nwp_awp_Cp3,Sp2t_awp_nwp_awp_Cw,Snpt_awp_nwp_awp_Cp1,
              Sp3t_nwp_nwp_nwp_Cw,Sp1t_nwp_awp_awp_Cw,Snpt_nwp_awp_awp_Cp3,
              Snpt_nwp_awp_awp_Cp2,Sp2t_awp_nwp_awp_Cp3,Snpt_nwp_nwp_awp_Cw,
              Sp2t_awp_nwp_awp_Cp1,Sp1t_nwp_awp_awp_Cp3,Sp1t_nwp_awp_awp_Cp2,
              Sp1t_nwp_nwp_awp_Cw,Snpt_nwp_nwp_awp_Cp3,Sp2t_nwp_nwp_awp_Cw,
              Sp1t_nwp_nwp_awp_Cp3,Sp2t_nwp_nwp_awp_Cp3 };
 StatesG_I = {Hbusy, Hidle};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(S_mp1) = Snpt & nwp_buffer1
      ControlLaw(S_mp2) = Snpt & awp_buffer1 & nwp_buffer2
      ControlLaw(S_mp3) = Snpt & awp_buffer1 & awp_buffer2 & nwp_buffer3
      ControlLaw(C_pck1) = Cw & awp_buffer1 & nwp_buffer2 & nwp_buffer3
      ControlLaw(C_pck2) = Cw & awp_buffer2 & nwp_buffer3
      ControlLaw(C_pck3) = Cw & awp_buffer3

    Observer:
      Project_S_mp1() := { return Func_start(); }
      Project_S_mp2() := { return Func_start(); }
      Project_S_mp3() := { return Func_start(); }
      Project_S_pap1() := { return Func_stop(); }
      Project_S_pap2() := { return Func_stop(); }
      Project_S_pap3() := { return Func_stop(); }
*/

/*
  Commands and signals:
    sorter events:                                    handling station events (interface):
       S_mp1 := move the piece at the first slot        start
       S_mp2 := move the piece at the second slot       start
       S_mp3 := move the piece at the third slot        start
      S_pap1 := piece arrival at the first slo          stop
      S_pap2 := piece arrival at the second slot        stop
      S_pap3 := piece arrival at the third slot         stop
    crane events:
      C_eop1 := end of pick up at the first slot
      C_eop2 := end of pick up at the second slot
      C_eop3 := end of pick up at the third slot
      C_pck1 := pick up a piece at the first slot
      C_pck2 := pick up a piece at the second slot
      C_pck3 := pick up a piece at the third slot
*/
 Input = {S_mp1, S_mp2, S_mp3, S_pap1, S_pap2, S_pap3,
          C_eop1, C_eop2, C_eop3, C_pck1, C_pck2, C_pck3 };
 Output = {start, stop, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 //G_FB,         Deterministic automaton exhaustively modeling the device behavior
 //              (defined in file SBhandling)
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
            tf:{                                                    (Snpt_nwp_nwp_nwp_Cw,S_mp1,Sp1t_nwp_nwp_nwp_Cw),
                (Sp1t_nwp_nwp_nwp_Cw,S_pap1,Snpt_awp_nwp_nwp_Cw),
                (Snpt_awp_nwp_nwp_Cw,C_pck1,Snpt_awp_nwp_nwp_Cp1),  (Snpt_awp_nwp_nwp_Cw,S_mp2,Sp2t_awp_nwp_nwp_Cw),
                (Snpt_awp_nwp_nwp_Cp1,C_eop1,Snpt_nwp_nwp_nwp_Cw),  (Snpt_awp_nwp_nwp_Cp1,S_mp2,Sp2t_awp_nwp_nwp_Cp1),

                (Sp2t_awp_nwp_nwp_Cw,S_pap2,Snpt_awp_awp_nwp_Cw),
                (Sp2t_awp_nwp_nwp_Cw,C_pck1,Sp2t_awp_nwp_nwp_Cp1),
                (Snpt_awp_awp_nwp_Cw,C_pck2,Snpt_awp_awp_nwp_Cp2),  (Snpt_awp_awp_nwp_Cw,S_mp3,Sp3t_awp_awp_nwp_Cw),
                (Snpt_awp_awp_nwp_Cp2,C_eop2,Snpt_awp_nwp_nwp_Cw),  (Snpt_awp_awp_nwp_Cp2,S_mp3,Sp3t_awp_awp_nwp_Cp2),

                (Sp2t_awp_nwp_nwp_Cp1,S_pap2,Snpt_awp_awp_nwp_Cp1),
                (Sp2t_awp_nwp_nwp_Cp1,C_eop1,Sp2t_nwp_nwp_nwp_Cw),
                (Sp2t_nwp_nwp_nwp_Cw,S_pap2,Snpt_nwp_awp_nwp_Cw),
                (Snpt_awp_awp_nwp_Cp1,C_eop1,Snpt_nwp_awp_nwp_Cw),  (Snpt_awp_awp_nwp_Cp1,S_mp3,Sp3t_awp_awp_nwp_Cp1),
                (Snpt_nwp_awp_nwp_Cw,C_pck2,Snpt_nwp_awp_nwp_Cp2),  (Snpt_nwp_awp_nwp_Cw,S_mp1,Sp1t_nwp_awp_nwp_Cw),
                (Snpt_nwp_awp_nwp_Cp2,C_eop2,Snpt_nwp_nwp_nwp_Cw),  (Snpt_nwp_awp_nwp_Cp2,S_mp1,Sp1t_nwp_awp_nwp_Cp2),

                (Sp3t_awp_awp_nwp_Cw,S_pap3,Snpt_awp_awp_awp_Cw),
                (Sp3t_awp_awp_nwp_Cw,C_pck2,Sp3t_awp_awp_nwp_Cp2),
                (Snpt_awp_awp_awp_Cw,C_pck3,Snpt_awp_awp_awp_Cp3),
                (Snpt_awp_awp_awp_Cp3,C_eop3,Snpt_awp_awp_nwp_Cw),

                (Sp3t_awp_awp_nwp_Cp2,S_pap3,Snpt_awp_awp_awp_Cp2),
                (Sp3t_awp_awp_nwp_Cp2,C_eop2,Sp3t_awp_nwp_nwp_Cw),
                (Snpt_awp_awp_awp_Cp2,C_eop2,Snpt_awp_nwp_awp_Cw),
                (Snpt_awp_nwp_awp_Cw,C_pck3,Snpt_awp_nwp_awp_Cp3),  (Snpt_awp_nwp_awp_Cw,S_mp2,Sp2t_awp_nwp_awp_Cw),
                (Snpt_awp_nwp_awp_Cp3,C_eop3,Snpt_awp_nwp_nwp_Cw),  (Snpt_awp_nwp_awp_Cp3,S_mp2,Sp2t_awp_nwp_awp_Cp3),

                (Sp3t_awp_awp_nwp_Cp1,S_pap3,Snpt_awp_awp_awp_Cp1),
                (Sp3t_awp_awp_nwp_Cp1,C_eop1,Sp3t_nwp_awp_nwp_Cw),
                (Snpt_awp_awp_awp_Cp1,C_eop1,Snpt_nwp_awp_awp_Cw),
                (Snpt_nwp_awp_awp_Cw,C_pck3,Snpt_nwp_awp_awp_Cp3),  (Snpt_nwp_awp_awp_Cw,S_mp1,Sp1t_nwp_awp_awp_Cw),
                (Snpt_nwp_awp_awp_Cp3,C_eop3,Snpt_nwp_awp_nwp_Cw),  (Snpt_nwp_awp_awp_Cp3,S_mp1,Sp1t_nwp_awp_awp_Cp3),

                (Sp3t_nwp_awp_nwp_Cw,S_pap3,Snpt_nwp_awp_awp_Cw),
                (Sp3t_nwp_awp_nwp_Cw,C_pck2,Sp3t_nwp_awp_nwp_Cp2),
                (Sp3t_nwp_awp_nwp_Cp2,C_eop2,Sp3t_nwp_nwp_nwp_Cw),
                (Sp3t_nwp_awp_nwp_Cp2,S_pap3,Snpt_nwp_awp_awp_Cp2),
                (Sp3t_nwp_nwp_nwp_Cw,S_pap3,Snpt_nwp_nwp_awp_Cw),
                (Snpt_nwp_awp_awp_Cp2,C_eop2,Snpt_nwp_nwp_awp_Cw),  (Snpt_nwp_awp_awp_Cp2,S_mp1,Sp1t_nwp_awp_awp_Cp2),
                (Snpt_nwp_nwp_awp_Cw,C_pck3,Snpt_nwp_nwp_awp_Cp3),  (Snpt_nwp_nwp_awp_Cw,S_mp1,Sp1t_nwp_nwp_awp_Cw),
                (Snpt_nwp_nwp_awp_Cp3,C_eop3,Snpt_nwp_nwp_nwp_Cw),  (Snpt_nwp_nwp_awp_Cp3,S_mp1,Sp1t_nwp_nwp_awp_Cp3),

                (Sp1t_nwp_awp_nwp_Cw,S_pap1,Snpt_awp_awp_nwp_Cw),
                (Sp1t_nwp_awp_nwp_Cw,C_pck2,Sp1t_nwp_awp_nwp_Cp2),
                (Sp1t_nwp_awp_nwp_Cp2,C_eop2,Sp1t_nwp_nwp_nwp_Cw),
                (Sp1t_nwp_awp_nwp_Cp2,S_pap1,Snpt_awp_awp_nwp_Cp2),

                (Sp2t_awp_nwp_awp_Cw,S_pap2,Snpt_awp_awp_awp_Cw),
                (Sp2t_awp_nwp_awp_Cw,C_pck3,Sp2t_awp_nwp_awp_Cp3),
                (Sp2t_awp_nwp_awp_Cp3,C_eop3,Sp2t_awp_nwp_nwp_Cw),
                (Sp2t_awp_nwp_awp_Cp3,S_pap2,Snpt_awp_awp_awp_Cp3),

                (Sp1t_nwp_awp_awp_Cw,S_pap1,Snpt_awp_awp_awp_Cw),
                (Sp1t_nwp_awp_awp_Cw,C_pck3,Sp1t_nwp_awp_awp_Cp3),
                (Sp1t_nwp_awp_awp_Cp3,C_eop3,Sp1t_nwp_awp_nwp_Cw),
                (Sp1t_nwp_awp_awp_Cp3,S_pap1,Snpt_awp_awp_awp_Cp3),

                (Sp1t_nwp_awp_awp_Cp2,S_pap1,Snpt_awp_awp_awp_Cp2),
                (Sp1t_nwp_awp_awp_Cp2,C_eop2,Sp1t_nwp_nwp_awp_Cw),

                (Sp1t_nwp_nwp_awp_Cw,S_pap1,Snpt_awp_nwp_awp_Cw),
                (Sp1t_nwp_nwp_awp_Cw,C_pck3,Sp1t_nwp_nwp_awp_Cp3),
                (Sp1t_nwp_nwp_awp_Cp3,C_eop3,Sp1t_nwp_nwp_nwp_Cw),
                (Sp1t_nwp_nwp_awp_Cp3,S_pap1,Snpt_awp_nwp_awp_Cp3),

                (Sp3t_awp_nwp_nwp_Cw,S_pap3,Snpt_awp_nwp_awp_Cw),
                (Sp3t_awp_nwp_nwp_Cw,C_pck1,Sp3t_awp_nwp_nwp_Cp1),
                (Sp3t_awp_nwp_nwp_Cp1,C_eop1,Sp3t_nwp_nwp_nwp_Cw),
                (Sp3t_awp_nwp_nwp_Cp1,S_pap3,Snpt_awp_nwp_awp_Cp1),
                (Snpt_awp_nwp_awp_Cp1,C_eop1,Snpt_nwp_nwp_awp_Cw),  (Snpt_awp_nwp_awp_Cp1,S_mp2,Sp2t_awp_nwp_awp_Cp1),

                (Sp2t_awp_nwp_awp_Cp1,S_pap2,Snpt_awp_awp_awp_Cp1),
                (Sp2t_awp_nwp_awp_Cp1,C_eop1,Sp2t_nwp_nwp_awp_Cw),
                (Sp2t_nwp_nwp_awp_Cw,S_pap2,Snpt_nwp_awp_awp_Cw),
                (Sp2t_nwp_nwp_awp_Cw,C_pck3,Sp2t_nwp_nwp_awp_Cp3),
                (Sp2t_nwp_nwp_awp_Cp3,S_pap2,Snpt_nwp_awp_awp_Cp3),
                (Sp2t_nwp_nwp_awp_Cp3,C_eop3,Sp2t_nwp_nwp_nwp_Cw) },
            x_0: Snpt_nwp_nwp_nwp_Cw,
            Xm: {Snpt_nwp_nwp_nwp_Cw},
            ctrlEvents: {S_mp1, S_mp2, S_mp3, C_pck1, C_pck2, C_pck3} ) &
 bound_G_ts = 12
 & 
 G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,
           tf:{                                                                  (Snpt_nwp_nwp_nwp_Cw,(S_mp1,start),Sp1t_nwp_nwp_nwp_Cw),
               (Sp1t_nwp_nwp_nwp_Cw,(S_pap1,stop),Snpt_awp_nwp_nwp_Cw),
               (Snpt_awp_nwp_nwp_Cw,(C_pck1,silentEvent),Snpt_awp_nwp_nwp_Cp1),  (Snpt_awp_nwp_nwp_Cw,(S_mp2,start),Sp2t_awp_nwp_nwp_Cw),
               (Snpt_awp_nwp_nwp_Cp1,(C_eop1,silentEvent),Snpt_nwp_nwp_nwp_Cw),  (Snpt_awp_nwp_nwp_Cp1,(S_mp2,start),Sp2t_awp_nwp_nwp_Cp1),

               (Sp2t_awp_nwp_nwp_Cw,(S_pap2,stop),Snpt_awp_awp_nwp_Cw),
               (Sp2t_awp_nwp_nwp_Cw,(C_pck1,silentEvent),Sp2t_awp_nwp_nwp_Cp1),
               (Snpt_awp_awp_nwp_Cw,(C_pck2,silentEvent),Snpt_awp_awp_nwp_Cp2),  (Snpt_awp_awp_nwp_Cw,(S_mp3,start),Sp3t_awp_awp_nwp_Cw),
               (Snpt_awp_awp_nwp_Cp2,(C_eop2,silentEvent),Snpt_awp_nwp_nwp_Cw),  (Snpt_awp_awp_nwp_Cp2,(S_mp3,start),Sp3t_awp_awp_nwp_Cp2),

               (Sp2t_awp_nwp_nwp_Cp1,(S_pap2,stop),Snpt_awp_awp_nwp_Cp1),
               (Sp2t_awp_nwp_nwp_Cp1,(C_eop1,silentEvent),Sp2t_nwp_nwp_nwp_Cw),
               (Sp2t_nwp_nwp_nwp_Cw,(S_pap2,stop),Snpt_nwp_awp_nwp_Cw),
               (Snpt_awp_awp_nwp_Cp1,(C_eop1,silentEvent),Snpt_nwp_awp_nwp_Cw),  (Snpt_awp_awp_nwp_Cp1,(S_mp3,start),Sp3t_awp_awp_nwp_Cp1),
               (Snpt_nwp_awp_nwp_Cw,(C_pck2,silentEvent),Snpt_nwp_awp_nwp_Cp2),  (Snpt_nwp_awp_nwp_Cw,(S_mp1,start),Sp1t_nwp_awp_nwp_Cw),
               (Snpt_nwp_awp_nwp_Cp2,(C_eop2,silentEvent),Snpt_nwp_nwp_nwp_Cw),  (Snpt_nwp_awp_nwp_Cp2,(S_mp1,start),Sp1t_nwp_awp_nwp_Cp2),

               (Sp3t_awp_awp_nwp_Cw,(S_pap3,stop),Snpt_awp_awp_awp_Cw),
               (Sp3t_awp_awp_nwp_Cw,(C_pck2,silentEvent),Sp3t_awp_awp_nwp_Cp2),
               (Snpt_awp_awp_awp_Cw,(C_pck3,silentEvent),Snpt_awp_awp_awp_Cp3),
               (Snpt_awp_awp_awp_Cp3,(C_eop3,silentEvent),Snpt_awp_awp_nwp_Cw),

               (Sp3t_awp_awp_nwp_Cp2,(S_pap3,stop),Snpt_awp_awp_awp_Cp2),
               (Sp3t_awp_awp_nwp_Cp2,(C_eop2,silentEvent),Sp3t_awp_nwp_nwp_Cw),
               (Snpt_awp_awp_awp_Cp2,(C_eop2,silentEvent),Snpt_awp_nwp_awp_Cw),
               (Snpt_awp_nwp_awp_Cw,(C_pck3,silentEvent),Snpt_awp_nwp_awp_Cp3),  (Snpt_awp_nwp_awp_Cw,(S_mp2,start),Sp2t_awp_nwp_awp_Cw),
               (Snpt_awp_nwp_awp_Cp3,(C_eop3,silentEvent),Snpt_awp_nwp_nwp_Cw),  (Snpt_awp_nwp_awp_Cp3,(S_mp2,start),Sp2t_awp_nwp_awp_Cp3),

               (Sp3t_awp_awp_nwp_Cp1,(S_pap3,stop),Snpt_awp_awp_awp_Cp1),
               (Sp3t_awp_awp_nwp_Cp1,(C_eop1,silentEvent),Sp3t_nwp_awp_nwp_Cw),
               (Snpt_awp_awp_awp_Cp1,(C_eop1,silentEvent),Snpt_nwp_awp_awp_Cw),
               (Snpt_nwp_awp_awp_Cw,(C_pck3,silentEvent),Snpt_nwp_awp_awp_Cp3),  (Snpt_nwp_awp_awp_Cw,(S_mp1,start),Sp1t_nwp_awp_awp_Cw),
               (Snpt_nwp_awp_awp_Cp3,(C_eop3,silentEvent),Snpt_nwp_awp_nwp_Cw),  (Snpt_nwp_awp_awp_Cp3,(S_mp1,start),Sp1t_nwp_awp_awp_Cp3),

               (Sp3t_nwp_awp_nwp_Cw,(S_pap3,stop),Snpt_nwp_awp_awp_Cw),
               (Sp3t_nwp_awp_nwp_Cw,(C_pck2,silentEvent),Sp3t_nwp_awp_nwp_Cp2),
               (Sp3t_nwp_awp_nwp_Cp2,(C_eop2,silentEvent),Sp3t_nwp_nwp_nwp_Cw),
               (Sp3t_nwp_awp_nwp_Cp2,(S_pap3,stop),Snpt_nwp_awp_awp_Cp2),
               (Sp3t_nwp_nwp_nwp_Cw,(S_pap3,stop),Snpt_nwp_nwp_awp_Cw),
               (Snpt_nwp_awp_awp_Cp2,(C_eop2,silentEvent),Snpt_nwp_nwp_awp_Cw),  (Snpt_nwp_awp_awp_Cp2,(S_mp1,start),Sp1t_nwp_awp_awp_Cp2),
               (Snpt_nwp_nwp_awp_Cw,(C_pck3,silentEvent),Snpt_nwp_nwp_awp_Cp3),  (Snpt_nwp_nwp_awp_Cw,(S_mp1,start),Sp1t_nwp_nwp_awp_Cw),
               (Snpt_nwp_nwp_awp_Cp3,(C_eop3,silentEvent),Snpt_nwp_nwp_nwp_Cw),  (Snpt_nwp_nwp_awp_Cp3,(S_mp1,start),Sp1t_nwp_nwp_awp_Cp3),

               (Sp1t_nwp_awp_nwp_Cw,(S_pap1,stop),Snpt_awp_awp_nwp_Cw),
               (Sp1t_nwp_awp_nwp_Cw,(C_pck2,silentEvent),Sp1t_nwp_awp_nwp_Cp2),
               (Sp1t_nwp_awp_nwp_Cp2,(C_eop2,silentEvent),Sp1t_nwp_nwp_nwp_Cw),
               (Sp1t_nwp_awp_nwp_Cp2,(S_pap1,stop),Snpt_awp_awp_nwp_Cp2),

               (Sp2t_awp_nwp_awp_Cw,(S_pap2,stop),Snpt_awp_awp_awp_Cw),
               (Sp2t_awp_nwp_awp_Cw,(C_pck3,silentEvent),Sp2t_awp_nwp_awp_Cp3),
               (Sp2t_awp_nwp_awp_Cp3,(C_eop3,silentEvent),Sp2t_awp_nwp_nwp_Cw),
               (Sp2t_awp_nwp_awp_Cp3,(S_pap2,stop),Snpt_awp_awp_awp_Cp3),

               (Sp1t_nwp_awp_awp_Cw,(S_pap1,stop),Snpt_awp_awp_awp_Cw),
               (Sp1t_nwp_awp_awp_Cw,(C_pck3,silentEvent),Sp1t_nwp_awp_awp_Cp3),
               (Sp1t_nwp_awp_awp_Cp3,(C_eop3,silentEvent),Sp1t_nwp_awp_nwp_Cw),
               (Sp1t_nwp_awp_awp_Cp3,(S_pap1,stop),Snpt_awp_awp_awp_Cp3),

               (Sp1t_nwp_awp_awp_Cp2,(S_pap1,stop),Snpt_awp_awp_awp_Cp2),
               (Sp1t_nwp_awp_awp_Cp2,(C_eop2,silentEvent),Sp1t_nwp_nwp_awp_Cw),

               (Sp1t_nwp_nwp_awp_Cw,(S_pap1,stop),Snpt_awp_nwp_awp_Cw),
               (Sp1t_nwp_nwp_awp_Cw,(C_pck3,silentEvent),Sp1t_nwp_nwp_awp_Cp3),
               (Sp1t_nwp_nwp_awp_Cp3,(C_eop3,silentEvent),Sp1t_nwp_nwp_nwp_Cw),
               (Sp1t_nwp_nwp_awp_Cp3,(S_pap1,stop),Snpt_awp_nwp_awp_Cp3),

               (Sp3t_awp_nwp_nwp_Cw,(S_pap3,stop),Snpt_awp_nwp_awp_Cw),
               (Sp3t_awp_nwp_nwp_Cw,(C_pck1,silentEvent),Sp3t_awp_nwp_nwp_Cp1),
               (Sp3t_awp_nwp_nwp_Cp1,(C_eop1,silentEvent),Sp3t_nwp_nwp_nwp_Cw),
               (Sp3t_awp_nwp_nwp_Cp1,(S_pap3,stop),Snpt_awp_nwp_awp_Cp1),
               (Snpt_awp_nwp_awp_Cp1,(C_eop1,silentEvent),Snpt_nwp_nwp_awp_Cw),  (Snpt_awp_nwp_awp_Cp1,(S_mp2,start),Sp2t_awp_nwp_awp_Cp1),

               (Sp2t_awp_nwp_awp_Cp1,(S_pap2,stop),Snpt_awp_awp_awp_Cp1),
               (Sp2t_awp_nwp_awp_Cp1,(C_eop1,silentEvent),Sp2t_nwp_nwp_awp_Cw),
               (Sp2t_nwp_nwp_awp_Cw,(S_pap2,stop),Snpt_nwp_awp_awp_Cw),
               (Sp2t_nwp_nwp_awp_Cw,(C_pck3,silentEvent),Sp2t_nwp_nwp_awp_Cp3),
               (Sp2t_nwp_nwp_awp_Cp3,(S_pap2,stop),Snpt_nwp_awp_awp_Cp3),
               (Sp2t_nwp_nwp_awp_Cp3,(C_eop3,silentEvent),Sp2t_nwp_nwp_nwp_Cw) },
            x_0: Snpt_nwp_nwp_nwp_Cw,
            Xm: {Snpt_nwp_nwp_nwp_Cw},
            ctrlEvents: {S_mp1, S_mp2, S_mp3, C_pck1, C_pck2, C_pck3} ) &
 bound_G_A = 12
 &
 G_I = rec(states: StatesG_I,
           events: Output - {silentEvent},
           tf: {(Hidle,start,Hbusy), (Hbusy,stop,Hidle)},
           x_0: Hidle,
           Xm: {Hidle},
           ctrlEvents: {start} ) &
 bound_G_I = 6
END