// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL injector
/*
  A jack coupled with a cylinder barrel located in the basis of a gravity-feed
  magazine with:
   - three valves for one stable position,
   - sensor X0 to detect workpiece arrival,
   - sensor X1 to detect complete retraction,
   - sensor X2 to detect complete extension, and
   - actuator Y0 to extend and retract the cylinder.
  The magazine (cylindrical column) is monitored by a sensor located at its base to
  detect the presence of a workpiece when obstructing a light beam. The injector
  pushes the workpiece out of the magazine.
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
    x0 := X0nY0X1nX2             r := completely retracted
    x1 := nX0nY0X1nX2          wim := workpiece in the magazine
    x2 := nX0Y0X1nX2            ie := in extension (pushing the workpiece)
    x3 := nX0Y0nX1nX2                 in extension
    x4 := nX0Y0nX1X2             e := completely extended (holding the workpiece)
    x5 := nX0nY0nX1X2                 in retraction
    x6 := nX0nY0nX1nX2                in retraction
    x7 := X0nY0nX1nX2                 completely retracted
  _X1X2 is physically impossible because the cylinder cannot be simultaneously
  at the far beginning and the far end.
*/

 StatesG_A = {x0, x1, x2, x3, x4, x5, x6, x7, illegal};
 StatesG_I = {Ie, Iie, Ir, Iwim};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <Y0, X0, X1, X2>:
    StateValues = {nY0, pY0, nX0, pX0, nX1, pX1, nX2, pX2}.
      GetStateValues : G_FB'states --> POW(StateValues):
        GetStateValues(x0) = {pX0, nY0, pX1, nX2}
        GetStateValues(x1) = {nX0, nY0, pX1, nX2)
        GetStateValues(x2) = {nX0, pY0, pX1, nX2}
        GetStateValues(x3) = {nX0, pY0, nX1, nX2}
        GetStateValues(x4) = {nX0, pY0, nX1, pX2}
        GetStateValues(x5) = {nX0, nY0, nX1, pX2}
        GetStateValues(x6) = {nX0, nY0, nX1, nX2}
        GetStateValues(x7) = {pX0, nY0, nX1, nX2}

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(fY0) = {pY0, pX2}
      ControlLaw(rY0) = {nX0, nY0, pX1}

    Observer:
      Project_fY0() := { return Func_rearm(); }
      Project_rY0() := { return Func_inject(); }
      Project_fX0() := { return Func_wpa(); }
      Project_rX2() := { return Func_eoi(); }

    Commands to be sent to an actuator to reflect the occurrence an event:
      Lactions_fY0() := { reset(Y0); }
      Lactions_rY0() := { set(Y0); }

    Cycle() := Cycle() + {
      if ( fX0 ) then Func_fX0();
      if ( rX2 ) then Func_rX2();
      Func_fY0();
      Func_rY0(); }
*/

/*
  Commands of the actuator and signals of sensors:
    f (signal falling edge), r (signal rising edge).
                                                 events of the interface
    fX0 := light beam obstructed                      wpa := workpiece arrival
    rX0 := light beam not obstructed
    fY0 := retract                                  rearm := rearm the injector
    rY0 := extend                                  inject := inject a workpiece
    rX1 := end of retraction
    rX2 := end of extension                           eoi := end of injection

*/
 Input = {fY0, rY0, fX0, rX0, fX1, rX1, fX2, rX2};
 Output = {eoi, inject, rearm, wpa, silentEvent}

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
                 (x2,fX1,x3), (x2,fY0,illegal), (x2,rY0,illegal),
                 (x3,rX2,x4), (x3,fY0,illegal), (x3,rY0,illegal),
                 (x4,fY0,x5),                   (x4,rY0,illegal),
                 (x5,fX2,x6), (x5,fY0,illegal), (x5,rY0,illegal),
                 (x6,rX0,x7), (x6,fY0,illegal), (x6,rY0,illegal),
                 (x7,rX1,x0), (x7,fY0,illegal), (x7,rY0,illegal) },
            // other uncontrollabled transitions on fX0, rX0, fX1, rX1, fX2, and rX2
            // are almost impossible (statistically validated), unless the cylinder sticks.
            x_0: x0,
            Xm: {x0},        // Add x5, x6, and  x7 for consistency of marking
            ctrlEvents: {fY0, rY0} ) &
 bound_G_FB = 8
 &
 G_ts = rec(states: StatesG_A - {illegal},
            events: Input,
            tf: {(x0,fX0,x1), (x1,rY0,x2), (x2,fX1,x3), (x3,rX2,x4),
                 (x4,fY0,x5), (x5,fX2,x6), (x6,rX0,x7), (x7,rX1,x0) },
            x_0: x0,
            Xm: {x0},        // Add x5, x6, and  x7 for consistency of marking
            ctrlEvents: {fY0, rY0} ) &
 bound_G_ts = 8
 &
 G_A = rec(states: StatesG_A - {illegal},
           inputs: Input,
           outputs: Output,
           tf: {(x0,(fX0,wpa),x1),         (x1,(rY0,inject),x2),
                (x2,(fX1,silentEvent),x3), (x3,(rX2,eoi),x4),
                (x4,(fY0,rearm),x5),       (x5,(fX2,silentEvent),x6),
                (x6,(rX0,silentEvent),x7), (x7,(rX1,silentEvent),x0) },
           x_0: x0,
           Xm: {x0},         // Add x5, x6, and  x7 for consistency of marking
           ctrlEvents: {fY0, rY0} ) &
 bound_G_A = 8
 &
 G_I = rec(states: StatesG_I,
           events: VocalOutput,
           tf: {(Ir,wpa,Iwim), (Iwim,inject,Iie), (Iie,eoi,Ie), (Ie,rearm,Ir)},
           x_0: Ir,
           Xm: {Ir},
           ctrlEvents: {inject, rearm} ) &
 bound_G_I = 4
END

/*
OUTPUT:

Number of states of the exhaustive model: 9
Number of transitions of the exhaustive model: 22
Number of states of the transition structure: 8
Number of transitions of the transition structure: 8
Number of states of the interface: 4
Number of transitions the interface: 4
cover(DisplayStatistics)
"The device (agent) is controllable."
cover(Check_controllability)
"The device (agent) is nonblocking."
cover(Check_nonblocking_Agent)
"The interface is nonblocking."
cover(Check_nonblocking_Interface)
Disabled events: {(x0|->{fY0,rY0}),(x1|->{fY0}),(x2|->{fY0,rY0}),(x3|->{fY0,rY0}),(x4|->{rY0}),(x5|->{fY0,rY0}),(x6|->{fY0,rY0}),(x7|->{fY0,rY0})}
Enabled events: {(x0|->{fX0}),(x1|->{rY0}),(x2|->{fX1}),(x3|->{rX2}),(x4|->{fY0}),(x5|->{fX2}),(x6|->{rX0}),(x7|->{rX1})}
"The SFBC functions are complementary."
Control law: {(fY0|->x4),(rY0|->x1)}
cover(Display_SFBC_function)

% Finished processing file after 280 ms walltime (280 ms runtime), since start: 1970 ms


Size of the prefix-closed language of the exhaustive behavior: 23
Size of the marked language of the exhaustive behavior: 2
Number of states of the agent: 8
Number of transitions of the agent: 8
Size of the prefix-closed language of the agent: 9
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
The supervisor (relation): {(x0|->fX0),(x1|->rY0),(x2|->fX1),(x3|->rX2),(x4|->fY0),(x5|->fX2),(x6|->rX0),(x7|->rX1)}
cover(Display_supervisor)
State realization of the supervisor (disabled events): {(x0|->{fY0,rY0}),(x1|->{fY0}),(x2|->{fY0,rY0}),(x3|->{fY0,rY0}),(x4|->{rY0}),(x5|->{fY0,rY0}),(x6|->{fY0,rY0}),(x7|->{fY0,rY0})}
State realization of a supervisor (enabled events): {(x0|->{fX0}),(x1|->{rY0}),(x2|->{fX1}),(x3|->{rX2}),(x4|->{fY0}),(x5|->{fX2}),(x6|->{rX0}),(x7|->{rX1})}
cover(Display_supervisors)
"Marking is weakly consistent."
cover(Check_WeakConsistencyOfMarking)
"The causal map is an observer."
cover(Check_ObserverProperty)
"Control coincidence property is satisfied."
cover(Check_ConditionOnSCT)
"The interface is nonblocking."
cover(Check_NonBlocking_Interface)

% Finished processing file after 500 ms walltime (480 ms runtime), since start: 2330 ms
*/