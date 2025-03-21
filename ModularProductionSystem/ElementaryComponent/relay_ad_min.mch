// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL relay_ad_min
/*
  A mechanical relay (to electrically turn on/off the power supplied to another device,
  while considering latency effects) with asymmetric delay,
   - actuator Y0 to close/open the relay,
   - on-delay timer (ODT) T0 to consider latency effects when closing the relay, and
   - on-delay timer (ODT) T1 to consider latency effects when opening the relay.

  The relay is either normally open or normally closed. If it is normally open, there
  is no electrical contact and the device is off. If it is normally closed, there is
  an electrical contact and the device is on. The timers of type on-delay timer (ODT),
  have their own preset value (possibly zero), rising signal, and initial state:
    - T0 is expired, T0 : ODT[S := nY0 & nM1, pM0, dt0] (dt0 is the delay):
        - the activation signal S is defined by the logical formulas nY0 & nM1;
        - the Boolean memory element M0, in positive form (i.e., pM0), indicates
          the initial state of the timer (pT0);
        - the constant timing delay is dt0
    - T1 is cleared, T1 : ODT[S := pY0 & nM0, nM1, dt1] (dt1 is the delay):
        - the activation signal S is defined by the logical formulas pY0 & nM0;
        - the Boolean memory element M1, in negative form (i.e., nM1), indicates
          the initial state of the timer (nT1);
        - the constant timing delay is dt1.
  It interface is minimal (activate/end of activation).
*/

DEFINITIONS
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../../Theories/Mealy_basis.def";

SETS
/*
  Polarity of Boolean sensors (input) and the actuator (command):
    n (negative), p (positive, default).
                             states of the interface
    x0 := nY0M0nM1              off (no electrical contact)
    x1 := Y0M0nM1              noff := not off
    x2 := Y0nM0nM1                     not off
    x3 := Y0nM0M1                      not off
    x4 := nY0nM0M1                     not off
    x5 := nY0nM0nM1                    not off
  _M0M1 is physically impossible due to the setting of delays.
*/
 StatesG_A = {x0, x1, x2, x3, x4, x5, illegal};
 StatesG_I = {Roff, Rnoff};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <Y0, M0, M1>
    StateValues = {nY0, pY0, nM0, pM0, nM1, pM1}.
      GetStateValues : G_FB'states --> POW(StateValues):
        GetStateValues(x0) = {nY0, pM0, nM1}
        GetStateValues(x1) = {pY0, pM0, nM1)
        GetStateValues(x2) = {pY0, nM0, nM1}
        GetStateValues(x3) = {pY0, nM0, pM1}
        GetStateValues(x4) = {nY0, nM0, pM1}
        GetStateValues(x5) = {nY0, nM0, nM1}

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(fY0) = pY0 & pM1
      ControlLaw(rY0) = nY0 & pM0

    Observer:
      Project_rY0() := { return Func_activate(); }
      Project_rM0() := { return Func_eod(); }

    Command to be sent to an actuator or a memory element to reflect the occurrence an event:
      Lactions_fY0() := { reset(Y0); }
      Lactions_rY0() := { set(Y0); }
      Lactions_fM0() := { reset(M0); }
      Lactions_rM0() := { set(M0); }
      Lactions_fM1() := { reset(M1); }
      Lactions_rM1() := { set(M1); }

    Cycle() := Cycle() + {
      if ( (pY0 | pM1) & pM0 ) then Func_fM0();
      if ( (nY0 | pM0) & pM1 ) then Func_fM1();
      if ( T0 & nY0 & nM1 & nM0 ) then Func_rM0();
      Eval_T(T0, nY0 & nM1 & nM0);
      if ( T1 & pY0 & nM0 & nM1 ) then Func_rM1();
      Eval_T(T1, pY0 & nM0 & nM1);
      Func_fY0();
      Func_rY0(); }
*/

/*
  Commands of the actuator and signals of sensors:
    f (signal falling edge), r (signal rising edge).
                                       events of the interface
    fY0 := open the relay
    rY0 := close the relay               activate (the device)
    fM0 := timer cleared
    rM0 := relay opened                  eod := end of deactivation
    fM1 := timer cleared
    rM1 := relay closed
*/
 Input = {fY0, rY0, fM0, rM0, fM1, rM1};
 Output = {activate, eod, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 G_FB,        // Deterministic automaton exhaustively modeling the device behavior
 bound_G_FB,  // Bound on the trace length of the device behavior
 G_ts,        // G_FB under control of a state feedback function (solution of a control problem)
 bound_G_ts,  // Bound on the trace length of the controlled device behavior
 G_A,         // Mealy machine modeling the device implementation (agent)
 bound_G_A,   // Bound on the trace length of the agent
 G_I,         // (quotient deterministic) automaton modeling a device abstraction (interface)
 bound_G_I    // Bound on the trace length of the interface

PROPERTIES
 VocalOutput = Output - {silentEvent}
 &
 G_FB = rec(states: StatesG_A,
            events: Input,
            tf: {(x0,rY0,x1), (x0,fY0,illegal),
                 (x1,fM0,x2), (x1,fY0,illegal), (x1,rY0,illegal),
                 (x2,rM1,x3), (x2,fY0,illegal), (x2,rY0,illegal),
                 (x3,fY0,x4),                   (x3,rY0,illegal),
                 (x4,fM1,x5), (x4,fY0,illegal), (x4,rY0,illegal),
                 (x5,rM0,x0), (x5,fY0,illegal), (x5,rY0,illegal) },
            // other uncontrollabled transitions on fM0, rM0, fM1, and rM1
            // are almost impossible (statistically validated), unless the relay is faulty.
            x_0: x0,
            Xm: {x0},
            ctrlEvents: {fY0, rY0} ) &
 bound_G_FB = 6
 &
 G_ts = rec(states: StatesG_A - {illegal},
            events: Input,
            tf: {(x0,rY0,x1), (x1,fM0,x2), (x2,rM1,x3),
                 (x3,fY0,x4), (x4,fM1,x5), (x5,rM0,x0) },
            x_0: x0,
            Xm: {x0},
            ctrlEvents: {fY0, rY0} ) &
 bound_G_ts = 6
 &
 G_A = rec(states: StatesG_A - {illegal},
           inputs: Input,
           outputs: Output,
           tf: {(x0,(rY0,activate),x1),    (x1,(fM0,silentEvent),x2), (x2,(rM1,silentEvent),x3),
                (x3,(fY0,silentEvent),x4), (x4,(fM1,silentEvent),x5), (x5,(rM0,eod),x0) },
           x_0: x0,
           Xm: {x0},
           ctrlEvents: {fY0, rY0} ) &
 bound_G_A = 6
 &
 G_I = rec(states: StatesG_I,
           events: VocalOutput,
           tf: {(Roff,activate,Rnoff), (Rnoff,eod,Roff) },
           x_0: Roff,
           Xm: {Roff},
           ctrlEvents: {activate} ) &
 bound_G_I = 2
END

/*
OUTPUT

Number of states of the exhaustive model: 7
Number of transitions of the exhaustive model: 16
Number of states of the transition structure: 6
Number of transitions of the transition structure: 6
Number of states of the interface: 2
Number of transitions the interface: 2
cover(DisplayStatistics)
"The device (agent) is controllable."
cover(Check_controllability)
"The device (agent) is nonblocking."
cover(Check_nonblocking_Agent)
"The interface is nonblocking."
cover(Check_nonblocking_Interface)
Disabled events: {(x0|->{fY0}),(x1|->{fY0,rY0}),(x2|->{fY0,rY0}),(x3|->{rY0}),(x4|->{fY0,rY0}),(x5|->{fY0,rY0})}
Enabled events: {(x0|->{rY0}),(x1|->{fM0}),(x2|->{rM1}),(x3|->{fY0}),(x4|->{fM1}),(x5|->{rM0})}
"The SFBC functions are complementary."
Control law: {(fY0|->x3),(rY0|->x0)}
cover(Display_SFBC_function)

% Finished processing file after 280 ms walltime (280 ms runtime), since start: 1970 ms


Size of the prefix-closed language of the exhaustive behavior: 17
Size of the marked language of the exhaustive behavior: 2
Number of states of the agent: 6
Number of transitions of the agent: 6
Size of the prefix-closed language of the agent: 7
Size of the marked language of the agent: 2
Number of states of the interface: 2
Number of transitions the interface: 2
Size of the prefix-closed language of the interface: 3
Size of the marked language of the interface: 2
cover(DisplayStatistics)
"The device (agent) is controllable."
cover(Check_Controllability)
"The device (agent) is Lm_closed."
cover(Check_Lm_closed)
The supervisor (relation): {(x0|->rY0),(x1|->fM0),(x2|->rM1),(x3|->fY0),(x4|->fM1),(x5|->rM0)}
cover(Display_supervisor)
State realization of the supervisor (disabled events): {(x0|->{fY0}),(x1|->{fY0,rY0}),(x2|->{fY0,rY0}),(x3|->{rY0}),(x4|->{fY0,rY0}),(x5|->{fY0,rY0})}
State realization of a supervisor (enabled events): {(x0|->{rY0}),(x1|->{fM0}),(x2|->{rM1}),(x3|->{fY0}),(x4|->{fM1}),(x5|->{rM0})}
cover(Display_supervisors)
"Marking is consistent."
cover(Check_WeakConsistencyOfMarking)
"The causal map is an observer."
cover(Check_ObserverProperty)
"Control coincidence property is satisfied."
cover(Check_ConditionOnSCT)
"The interface is nonblocking."
cover(Check_NonBlocking_Interface)

% Finished processing file after 330 ms walltime (320 ms runtime), since start: 2170 ms
*/