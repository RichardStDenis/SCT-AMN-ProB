// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL relay_sd
/*
  A mechanical relay (to electrically turn on/off the power supplied to another device,
  while considering latency effects) with symmetric delay,
   - actuator Y0 to close/open the relay, and
   - dual-delay timer T0 to consider latency effects when closing/opening the relay.

  The relay is either normally open or normally closed. If it is normally open, there
  is no electrical contact and the device is off. If it is normally closed, there is
  an electrical contact and the device is on. The memorized timer of type dual delay
  timer (DDT), which behaves as two independent timers with a single preset value. The
  rising signal occurs when the preset value associated to the first timer (called delay
  timer) has been reached and the falling signal occurs when the preset value (period)
  associated to the second timer (called period timer) has been reached. The timer has
  a preset value (possibly zero), rising signal, and initial state:
    - T0 : DDT[S := (pY0 & nM0) | (nY0 & pM0), nM0, dt] (dt is the delay)
        - the activation signal S is defined by the logical formulas pY0 & nM0 and
          nY0 & pM0 to start the delay timer and period timer, respectively;
        - the Boolean memory element M0, in negative form (i.e., nM0), indicates
          the initial state (nT0) of the memorized timer;
        - the constant timing delay is dt.
*/

DEFINITIONS
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../../Theories/Mealy_basis.def";

SETS
/*
  Polarity of the Boolean sensor (input) and the actuator (command):
    n (negative), p (positive, default).
                             states of the interface
    x0 := nY0nM0                off (no electrical contact)
    x1 := Y0nM0                dtat := delay timer accumulates time (preset value not reached)
    x2 := Y0M0                   on (electrical contact)
    x3 := nY0M0                ptat := period timer accumulates time (preset value not reached)
*/
 StatesG_A = {x0, x1, x2, x3, illegal};
 StatesG_I = {Rdtat, Roff, Ron, Rptat};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <Y0, M0>
    StateValues = {nY0, pY0, nM0, pM0}.
      GetStateValues : G_FB'states --> POW(StateValues):
        GetStateValues(x0) = {nY0, nM0}
        GetStateValues(x1) = {pY0, nM0)
        GetStateValues(x2) = {pY0, pM0}
        GetStateValues(x3) = {nY0, pM0}

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(fY0) = pY0 & pM0
      ControlLaw(rY0) = nY0 & nM0

    Observer:
      Project_fY0() := { return Func_deactivate(); }
      Project_rY0() := { return Func_activate(); }
      Project_fM0() := { return Func_eod(); }
      Project_rM0() := { return Func_eoa(); }

    Command to be sent to an actuator or a memory element to reflect the occurrence an event:
      Lactions_fY0() := { reset(Y0); }
      Lactions_rY0() := { set(Y0); }
      Lactions_fM0() := { reset(M0); }
      Lactions_rM0() := { set(M0); }

    Cycle() := Cycle() + {
      if ( T0 & nY0 & pM0 ) then Func_fM0();
      if ( T0 & pY0 & nM0 ) then Func_rM0();
      Eval_T(T0, (pY0 & nM0) | (nY0 & pM0));
      Func_fY0();
      Func_rY0(); }
*/

/*
  Commands of the actuator and signals of the sensor:
    f (signal falling edge), r (signal rising edge).
                                       events of the interface
    fY0 := open the relay                deactivate (the device)
    rY0 := close the relay               activate (the device)
    fM0 := relay opened                  eod := end of deactivation
    rM0 := relay closed                  eoa := end of activation
*/
 Input = {fY0, rY0, fM0, rM0};
 Output = {activate, deactivate, eoa, eod, silentEvent}

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
                 (x1,rM0,x2), (x1,fY0,illegal), (x1,rY0,illegal),
                 (x2,fY0,x3),                   (x2,rY0,illegal),
                 (x3,fM0,x0), (x3,fY0,illegal), (x3,rY0,illegal) },
            // other uncontrollabled transitions on fM0 and rM0
            // are almost impossible (statistically validated), unless the relay is faulty.
            x_0: x0,
            Xm: {x0},
            ctrlEvents: {fY0, rY0} ) &
 bound_G_FB = 4
 &
 G_ts = rec(states: StatesG_A - {illegal},
            events: Input,
            tf: {(x0,rY0,x1), (x1,rM0,x2), (x2,fY0,x3), (x3,fM0,x0) },
            x_0: x0,
            Xm: {x0},
            ctrlEvents: {fY0, rY0} ) &
 bound_G_ts = 4
 &
 G_A = rec(states: StatesG_A - {illegal},
           inputs: Input,
           outputs: Output,
           tf: {(x0,(rY0,activate),x1),   (x1,(rM0,eoa),x2), 
                (x2,(fY0,deactivate),x3), (x3,(fM0,eod),x0) },
           x_0: x0,
           Xm: {x0},
           ctrlEvents: {fY0, rY0} ) &
 bound_G_A = 4
 &
 G_I = rec(states: StatesG_I,
           events: VocalOutput,
           tf: {(Roff,activate,Rdtat),  (Rdtat,eoa,Ron),
                (Ron,deactivate,Rptat), (Rptat,eod,Roff) },
           x_0: Roff,
           Xm: {Roff},
           ctrlEvents: {activate, deactivate} ) &
 bound_G_I = 4
END

/*
OUTPUT:

Number of states of the exhaustive model: 5
Number of transitions of the exhaustive model: 10
Number of states of the transition structure: 4
Number of transitions of the transition structure: 4
Number of states of the interface: 4
Number of transitions the interface: 4
cover(DisplayStatistics)
"The device (agent) is controllable."
cover(Check_controllability)
"The device (agent) is nonblocking."
cover(Check_nonblocking_Agent)
"The interface is nonblocking."
cover(Check_nonblocking_Interface)
Disabled events: {(x0|->{fY0}),(x1|->{fY0,rY0}),(x2|->{rY0}),(x3|->{fY0,rY0})}
Enabled events: {(x0|->{rY0}),(x1|->{rM0}),(x2|->{fY0}),(x3|->{fM0})}
"The SFBC functions are complementary."
Control law: {(fY0|->x2),(rY0|->x0)}
cover(Display_SFBC_function)

% Finished processing file after 270 ms walltime (260 ms runtime), since start: 1980 ms


Size of the prefix-closed language of the exhaustive behavior: 11
Size of the marked language of the exhaustive behavior: 2
Number of states of the agent: 4
Number of transitions of the agent: 4
Size of the prefix-closed language of the agent: 5
Size of the marked language of the agent: 2
Number of states of the interface: 4
Number of transitions the interface: 4
Size of the prefix-closed language of the interface: 5
Size of the marked language of the interface: 2
cover(DisplayStatistics)
"The device (agent) is controllable."
cover(Check_Controllability)
"The device (agent) is Lm_closed."
cover(Check_Lm_closed)
The supervisor (relation): {(x0|->rY0),(x1|->rM0),(x2|->fY0),(x3|->fM0)}
cover(Display_supervisor)
State realization of the supervisor (disabled events): {(x0|->{fY0}),(x1|->{fY0,rY0}),(x2|->{rY0}),(x3|->{fY0,rY0})}
State realization of a supervisor (enabled events): {(x0|->{rY0}),(x1|->{rM0}),(x2|->{fY0}),(x3|->{fM0})}
cover(Display_supervisors)
"Marking is consistent."
cover(Check_WeakConsistencyOfMarking)
"The causal map is an observer."
cover(Check_ObserverProperty)
"Control coincidence property is satisfied."
cover(Check_ConditionOnSCT)
"The interface is nonblocking."
cover(Check_NonBlocking_Interface)

% Finished processing file after 320 ms walltime (310 ms runtime), since start: 2140 ms
*/