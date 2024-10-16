// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL conveyor_belt
/*
  A belt conveyer with
   - sensor X0 to detect the arrival of a piece at one end of the conveyer,
   - sensor X1 to detect the departure of a piece at the other end of the
     conveyer, and
   - actuator Y0 to start and stop (via a relay) an electrical motor that drives
     the conveyor.
  The conveyor carries a piece from the entry position to one or more exit positions.
  The exit sensor of the conveyor cannot discriminate the exit position taken by a
  piece but still detects its exit. It is assumed that there is only one piece at a
  time on the conveyor.
*/

SETS
/*
  Polarity of Boolean sensors (input) and the actuator (command):
    n (negative), p (positive, default).
                             states of the interface
    x0 := nY0X0nX1             nm := no motion
    x1 := nY0nX0nX1            nm
    x2 := Y0nX0nX1             im := in motion
    x3 := Y0X0nX1              im
    x4 := Y0X0X1               im
    x5 := nY0X0X1              im
*/
 StatesG_A = {x0, x1, x2, x3, x4, x5, illegal};
 StatesG_I = {BCim, BCnm};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <Y0, X0, X1>
    StateValues = {nY0, pY0, nX0, pX0, nX1, pX1}
      GetStateValues : G_FB'states --> POW(StateValues):
        GetStateValues(x0) = {nY0, pX0, nX1}
        GetStateValues(x1) = {nY0, nX0, nX1}
        GetStateValues(x2) = {pY0, nX0, nX1}
        GetStateValues(x3) = {pY0, pX0, nX1}
        GetStateValues(x4) = {pY0, pX0, pX1}
        GetStateValues(x5) = {nY0, pX0, pX1}

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(fY0) = pY0 & pX1
      ControlLaw(rY0) = nY0 & nX0

    Observer:
      Project_fY0() := { return Func_mso(); }
      Project_rY0() := { return Func_start(); }

    Commands to be sent to an actuator to reflect the occurrence an event:
      Lactions_fY0() := { reset(Y0); }
      Lactions_rY0() := { set(Y0); }

    Cycle() := Cycle() + {
      Func_fY0();
      Func_rY0(); }
*/

/*
  Commands of the actuator and signals of sensors:
    f (signal falling edge), r (signal rising edge).
                                                 events of the interface
    fY0 := turn off the motor                      mso := motor switched off
    rY0 := turn on the motor                       start := start the belt conveyor
    fX0 := a piece at the entry position
    rX0 := no piece at the entry position
    fX1 := no piece on the conveyor
    rX1 := exit of a piece from the belt
*/
 Input = {fY0, rY0, fX0, rX0, fX1, rX1};
 Output = {mso, start, silentEvent}

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
            tf: {(x0,fX0,x1), (x0,fY0,illegal), (x0,rY0,illegal),
                 (x1,rY0,x2), (x1,fY0,illegal),
                 (x2,rX0,x3), (x2,fY0,illegal), (x2,rY0,illegal),
                 (x3,rX1,x4), (x3,fY0,illegal), (x3,rY0,illegal),
                 (x4,fY0,x5),                   (x4,rY0,illegal),
                 (x5,fX1,x0), (x5,fY0,illegal), (x5,rY0,illegal) },
            // other uncontrollabled transitions on fX0, rX0, fX1, and rX1
            // are almost impossible (statistically validated).
            x_0: x0,
            Xm: {x0},        // Add x1 and x5 for consistency of marking
            ctrlEvents: {fY0, rY0} ) &
 bound_G_FB = 6
 &
 G_ts = rec(states: StatesG_A - {illegal},
            events: Input,
            tf: {(x0,fX0,x1), (x1,rY0,x2), (x2,rX0,x3),
                 (x3,rX1,x4), (x4,fY0,x5), (x5,fX1,x0) },
            x_0: x0,
            Xm: {x0},        // Add x1 and x5 for consistency of marking
            ctrlEvents: {fY0, rY0} ) &
 bound_G_ts = 6
 &
 G_A = rec(states: StatesG_A - {illegal},
           inputs: Input,
           outputs: Output,
           tf: {(x0,(fX0,silentEvent),x1), (x1,(rY0,start),x2), (x2,(rX0,silentEvent),x3),
                (x3,(rX1,silentEvent),x4), (x4,(fY0,mso),x5),   (x5,(fX1,silentEvent),x0) },
           x_0: x0,
           Xm: {x0},         // Add x1 and x5 for consistency of marking
           ctrlEvents: {fY0, rY0} ) &
 bound_G_A = 6
 &
 G_I = rec(states: StatesG_I,
           events: VocalOutput,
           tf: {(BCnm,start,BCim), (BCim,mso,BCnm)},
           x_0: BCnm,
           Xm: {BCnm},
           ctrlEvents: {start} ) &
 bound_G_I = 2
END

/*
OUTPUT:
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
Disabled events: {(x0|->{fY0,rY0}),(x1|->{fY0}),(x2|->{fY0,rY0}),(x3|->{fY0,rY0}),(x4|->{rY0}),(x5|->{fY0,rY0})}
Enabled events: {(x0|->{fX0}),(x1|->{rY0}),(x2|->{rX0}),(x3|->{rX1}),(x4|->{fY0}),(x5|->{fX1})}
"The SFBC functions are complementary."
Control law: {(fY0|->x4),(rY0|->x1)}
cover(Display_SFBC_function)

% Finished processing file after 280 ms walltime (280 ms runtime), since start: 1980 ms


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
The supervisor (relation): {(x0|->fX0),(x1|->rY0),(x2|->rX0),(x3|->rX1),(x4|->fY0),(x5|->fX1)}
cover(Display_supervisor)
State realization of the supervisor (disabled events): {(x0|->{fY0,rY0}),(x1|->{fY0}),(x2|->{fY0,rY0}),(x3|->{fY0,rY0}),(x4|->{rY0}),(x5|->{fY0,rY0})}
State realization of a supervisor (enabled events): {(x0|->{fX0}),(x1|->{rY0}),(x2|->{rX0}),(x3|->{rX1}),(x4|->{fY0}),(x5|->{fX1})}
cover(Display_supervisors)
"Marking is weakly consistent."
cover(Check_WeakConsistencyOfMarking)
"The causal map is an observer."
cover(Check_ObserverProperty)
"Control coincidence property is satisfied."
cover(Check_ConditionOnSCT)
"The interface is nonblocking."
cover(Check_NonBlocking_Interface)

% Finished processing file after 420 ms walltime (380 ms runtime), since start: 2220 ms
*/