// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL jack211r
/*
  A bistable, latched jack (always complete the last command when the power is lost) with
   - five self-locking valves for two stable (stop on power interruption) positions (2),
   - sensor to detect complete retraction (1), and
   - sensor to detect complete extension (1).
  The cylinder is initially retracted (r).
*/

DEFINITIONS
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../../Theories/Mealy_basis.def";

SETS
/*
  Polarity of Boolean sensors (input) and actuators (command):
    n (negative), p (positive, default).
                             states of the interface
    x0 := Y0nY1X0nX1            r := completely retracted (e.g., at left)
    x1 := nY0Y1X0nX1           ie := in extension
    x2 := nY0Y1nX0nX1                in extension
    x3 := nY0Y1nX0X1            e := completely extended (e.g., at right)
    x4 := Y0nY1nX0X1           ir := in retraction
    x5 := Y0nY1nX0nX1                in retraction
  _X0X1 is physically impossible because the cylinder cannot be simultaneously
  at the far beginning and the far end.
*/
 StatesG_A = {x0, x1, x2, x3, x4, x5, illegal};
 StatesG_I = {Je, Jie, Jir, Jr};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <Y0, Y1, X0, X1>
    StateValues = {nY0, pY0, nY1, pY1, nX0, pX0, nX1, pX1}.
      GetStateValues : G_FB'states --> POW(StateValues):
        GetStateValues(x0) = {pY0, nY1, pX0, nX1}
        GetStateValues(x1) = {nY0, pY1, pX0, nX1)
        GetStateValues(x2) = {nY0, pY1, nX0, nX1}
        GetStateValues(x3) = {nY0, pY1, nX0, pX1}
        GetStateValues(x4) = {pY0, nY1, nX0, pX1}
        GetStateValues(x5) = {pY0, nY1, nX0, nX1}

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(fY0rY1) = pY0 & pX0
      ControlLaw(fY1rY0) = pY1 & pX1

    Observer:
      Project_fY0rY1() := { return Func_extend(); }
      Project_fY1rY0() := { return Func_retract(); }
      Project_rX0() := { return Func_eor(); }
      Project_rX1() := { return Func_eoe(); }

    Commands to be sent to actuators to reflect the occurrence an event:
      Lactions_fY0rY1() := { reset(Y0); set(Y1); }
      Lactions_fY1rY0() := { reset(Y1); set(Y0); }

    Cycle() := Cycle() + {
      if ( rX0 ) then Func_rX0();
      if ( rX1 ) then Func_rX1();
      Func_fY0rY1();
      Func_fY1rY0(); }
*/

/*
  Commands of actuators and signals of sensors:
    f (signal falling edge), r (signal rising edge).
                                       events of the interface
    fY0rY1 := extend                     extend
    fY1rY0 := retract                    retract
    fX0 := in extension
    rX0 := end of retraction             eor := end of retraction
    fX1 := in retraction
    rX1 := end of extension              eoe := end of extension
*/
 Input = {fY0rY1, fY1rY0, fX0, rX0, fX1, rX1};
 Output = {eoe, eor, extend, retract, silentEvent}

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
            tf: {(x0,fY0rY1,x1),                      (x0,fY1rY0,illegal),
                 (x1,fX0,x2),    (x1,fY0rY1,illegal), (x1,fY1rY0,illegal),
                 (x2,rX1,x3),    (x2,fY0rY1,illegal), (x2,fY1rY0,illegal),
                 (x3,fY1rY0,x4), (x3,fY0rY1,illegal),
                 (x4,fX1,x5),    (x4,fY0rY1,illegal), (x4,fY1rY0,illegal),
                 (x5,rX0,x0),    (x5,fY0rY1,illegal), (x5,fY1rY0,illegal) },
            // other uncontrollabled transitions on fX0, rX0, fX1, and rX1
            // are almost impossible (statistically validated), unless the cylinder sticks.
            x_0: x0,
            Xm: {x0},
            ctrlEvents: {fY0rY1, fY1rY0} ) &
 bound_G_FB = 6
 &
 G_ts = rec(states: StatesG_A - {illegal},
            events: Input,
            tf: {(x0,fY0rY1,x1), (x1,fX0,x2), (x2,rX1,x3),
                 (x3,fY1rY0,x4), (x4,fX1,x5), (x5,rX0,x0) },
            x_0: x0,
            Xm: {x0},
            ctrlEvents: {fY0rY1, fY1rY0} ) &
 bound_G_ts = 6
 &
 G_A = rec(states: StatesG_A - {illegal},
           inputs: Input,
           outputs: Output,
           tf: {(x0,(fY0rY1,extend),x1),  (x1,(fX0,silentEvent),x2), (x2,(rX1,eoe),x3),
                (x3,(fY1rY0,retract),x4), (x4,(fX1,silentEvent),x5), (x5,(rX0,eor),x0) },
           x_0: x0,
           Xm: {x0},
           ctrlEvents: {fY0rY1, fY1rY0} ) &
 bound_G_A = 6
 &
 G_I = rec(states: StatesG_I,
           events: VocalOutput,
           tf: {(Jr,extend,Jie), (Jie,eoe,Je), (Je,retract,Jir), (Jir,eor,Jr)},
           x_0: Jr,
           Xm: {Jr},
           ctrlEvents: {extend, retract} ) &
 bound_G_I = 4
END

/*
OUTPUT:

Number of states of the exhaustive model: 7
Number of transitions of the exhaustive model: 16
Number of states of the transition structure: 6
Number of transitions of the transition structure: 6
Number of states of the interface: 4
Number of transitions the interface: 4
cover(DisplayStatistics)
"The device (agent) is controllable."
cover(Check_controllability)
"The device (agent) is nonblocking."
cover(Check_nonblocking_Agent)
"The interface is nonblocking."
cover(Check_nonblocking_Interface)
Disabled events: {(x0|->{fY1rY0}),(x1|->{fY0rY1,fY1rY0}),(x2|->{fY0rY1,fY1rY0}),(x3|->{fY0rY1}),(x4|->{fY0rY1,fY1rY0}),(x5|->{fY0rY1,fY1rY0})}
Enabled events: {(x0|->{fY0rY1}),(x1|->{fX0}),(x2|->{rX1}),(x3|->{fY1rY0}),(x4|->{fX1}),(x5|->{rX0})}
"The SFBC functions are complementary."
Control law: {(fY0rY1|->x0),(fY1rY0|->x3)}
cover(Display_SFBC_function)

% Finished processing file after 280 ms walltime (270 ms runtime), since start: 1970 ms

Size of the prefix-closed language of the exhaustive behavior: 17
Size of the marked language of the exhaustive behavior: 2
Number of states of the agent: 6
Number of transitions of the agent: 6
Size of the prefix-closed language of the agent: 7
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
The supervisor (relation): {(x0|->fY0rY1),(x1|->fX0),(x2|->rX1),(x3|->fY1rY0),(x4|->fX1),(x5|->rX0)}
cover(Display_supervisor)
State realization of the supervisor (disabled events): {(x0|->{fY1rY0}),(x1|->{fY0rY1,fY1rY0}),(x2|->{fY0rY1,fY1rY0}),(x3|->{fY0rY1}),(x4|->{fY0rY1,fY1rY0}),(x5|->{fY0rY1,fY1rY0})}
State realization of a supervisor (enabled events): {(x0|->{fY0rY1}),(x1|->{fX0}),(x2|->{rX1}),(x3|->{fY1rY0}),(x4|->{fX1}),(x5|->{rX0})}
cover(Display_supervisors)
"Marking is consistent."
cover(Check_WeakConsistencyOfMarking)
"The causal map is an observer."
cover(Check_ObserverProperty)
"Control coincidence property is satisfied."
cover(Check_ConditionOnSCT)
"The interface is nonblocking."
cover(Check_NonBlocking_Interface)

% Finished processing file after 350 ms walltime (340 ms runtime), since start: 2190 ms
*/