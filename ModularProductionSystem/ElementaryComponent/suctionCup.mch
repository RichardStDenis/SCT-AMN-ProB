// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL suctionCup
/*
  A bistable vacuum pump with
   - sensor X0 to react to the differential pressure (the difference between
     the pressure inside the cup and the atmospheric pressure outside the cup) and
   - actuators Y0 and Y1 to start vacuum (fY0rY1) and stop vacuum (fY1rY0).
  A suction cup can pick, hold, and place objects with smooth surfaces by vacuum.
*/

DEFINITIONS
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../../Theories/Mealy_basis.def";

SETS
/*
  Polarity of the Boolean sensor (input) and actuators (command):
    n (negative), p (positive, default).
                             states of the interface
    x0 := Y0nY1nX0             nv := no vacuum
    x1 := nY0Y1nX0              p := pumping air
    x2 := nY0Y1X0               v := vacuum
    x3 := Y0nY1X0               l := loosing air
*/
 StatesG_A = {x0, x1, x2, x3, illegal};
 StatesG_I = {SCl, SCnv, SCp, SCv};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <Y0, Y1, X0>
    StateValues = {nY0, pY0, nY1, pY1, nX0, pX0}
      GetStateValues : G_FB'states --> POW(StateValues):
        GetStateValues(x0) = {pY0, nY1, nX0}
        GetStateValues(x1) = {nY0, pY1, nX0)
        GetStateValues(x2) = {nY0, pY1, pX0}
        GetStateValues(x3) = {pY0, nY1, pX0}

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(fY0rY1) = pY0 & nX0
      ControlLaw(fY1rY0) = pY1 & pX0

    Observer:
      Project_fX0() := { return Func_ldp(); }
      Project_rX0() := { return Func_hdp(); }
      Project_fY0rY1() := { return Func_pump(); }
      Project_fY1rY0() := { return Func_expel(); }

    Commands to be sent to actuators to reflect the occurrence of an event:
      Lactions_fY0rY1() := { reset(Y0); set(Y1) }
      Lactions_fY1rY0() := { reset(Y1); set(Y0); }

    Cycle() := Cycle() + {
      if ( fX0 ) then Func_fX0();
      if ( rX0 ) then Func_rX0();
      Func_fY0rY1();
      Func_fY1rY0(); }
*/

/*
  Commands of actuators and signals of the sensor:
    f (signal falling edge), r (signal rising edge).
                                       events of the interface
    fY0rY1 := start vacuum               pump
    fY1rY0 := stop vacuum                expel
    fX0                                  ldp := low differential pressure
    rX0                                  hdp := high differential pressure
*/
 Input = {fY0rY1, fY1rY0, fX0, rX0};
 Output = {expel, hdp, ldp, pump, silentEvent}

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
                 (x1,rX0,x2),    (x1,fY0rY1,illegal), (x1,fY1rY0,illegal),
                 (x2,fY1rY0,x3), (x2,fY0rY1,illegal),
                 (x3,fX0,x0),    (x3,fY0rY1,illegal), (x3,fY1rY0,illegal) },
            // other uncontrollabled transitions on fX0 and rX0 are almost
            // impossible (statistically validated).
            x_0: x0,
            Xm: {x0},
            ctrlEvents: {fY0rY1, fY1rY0} ) &
 bound_G_FB = 4
 &
 G_ts = rec(states: StatesG_A - {illegal},
            events: Input,
            tf: {(x0,fY0rY1,x1), (x1,rX0,x2),
                 (x2,fY1rY0,x3), (x3,fX0,x0) },
            x_0: x0,
            Xm: {x0},
            ctrlEvents: {fY0rY1, fY1rY0} ) &
 bound_G_ts = 4
 &
 G_A = rec(states: StatesG_A - {illegal},
           inputs: Input,
           outputs: Output,
           tf: {(x0,(fY0rY1,pump),x1),  (x1,(rX0,hdp),x2),
                (x2,(fY1rY0,expel),x3), (x3,(fX0,ldp),x0) },
           x_0: x0,
           Xm: {x0},
           ctrlEvents: {fY0rY1, fY1rY0} ) &
 bound_G_A = 4
 &
 G_I = rec(states: StatesG_I,
           events: VocalOutput,
           tf: {(SCnv,pump,SCp), (SCp,hdp,SCv),
                (SCv,expel,SCl), (SCl,ldp,SCnv) },
           x_0: SCnv,
           Xm: {SCnv},
           ctrlEvents: {expel, pump} ) &
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
Disabled events: {(x0|->{fY1rY0}),(x1|->{fY0rY1,fY1rY0}),(x2|->{fY0rY1}),(x3|->{fY0rY1,fY1rY0})}
Enabled events: {(x0|->{fY0rY1}),(x1|->{rX0}),(x2|->{fY1rY0}),(x3|->{fX0})}
"The SFBC functions are complementary."
Control law: {(fY0rY1|->x0),(fY1rY0|->x2)}
cover(Display_SFBC_function)

% Finished processing file after 270 ms walltime (270 ms runtime), since start: 1990 ms


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
The supervisor (relation): {(x0|->fY0rY1),(x1|->rX0),(x2|->fY1rY0),(x3|->fX0)}
cover(Display_supervisor)
State realization of the supervisor (disabled events): {(x0|->{fY1rY0}),(x1|->{fY0rY1,fY1rY0}),(x2|->{fY0rY1}),(x3|->{fY0rY1,fY1rY0})}
State realization of a supervisor (enabled events): {(x0|->{fY0rY1}),(x1|->{rX0}),(x2|->{fY1rY0}),(x3|->{fX0})}
cover(Display_supervisors)
"Marking is consistent."
cover(Check_WeakConsistencyOfMarking)
"The causal map is an observer."
cover(Check_ObserverProperty)
"Control coincidence property is satisfied."
cover(Check_ConditionOnSCT)
"The interface is nonblocking."
cover(Check_NonBlocking_Interface)

% Finished processing file after 330 ms walltime (320 ms runtime), since start: 2160 ms
*/