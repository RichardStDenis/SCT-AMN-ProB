// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL stepperMotor
/*
  A stepper motor with:
   - sensor X0 (not a position sensor) to indicate the end of a shaft-rotation
     step and
   - actuator Y0 to turn the shaft motor one step.
  A stepper motor converts electricity into rotation. Each pulse of electricity
  turns the motor shaft one step. So, a stepper motor divides a full rotation
  into a number of equal steps. One feature of a stepper motor is its ability
  to position very accurately (with a small latency time at stopping). It must
  be controlled locally to turn off the motor because of strict temporal
  constraints involved between the sensor and driving mechanism.
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
    x0 := nY0X0                nm := no motion (at a fixed position)
    x1 := Y0X0                 im := in motion
    x2 := Y0nX0                      in motion
*/
 StatesG_A = {x0, x1, x2, illegal};
 StatesG_I = {SMim, SMnm};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables = <Y0, X0>
    State values = {nY0, pY0, nX0, pX0}
      GetStateValues : G_FB'states --> POW(StateValues):
        GetStateValues(x0) = {nY0, pX0},
        GetStateValues(x1) = {pY0, pX0},
        GetStateValues(x2) = {pY0, nX0}.

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(rY0) = {nY0}.

    Observer:
      Project_rY0() := { return Func_shift(); }
      Project_rX0fY0() := { return Func_eos(); }

    Command to be sent to an actuator to reflect the occurrence an event:
      Lactions_rY0() := { set(Y0); }
      Lactions_rX0fY0() := { reset(Y0); }

    Cycle() := Cycle() + {
      if ( rX0 ) then Func_rX0fY0();
      Func_rY0(); }
*/

/*
  Commands of the actuator and signals of the sensor:
    f (signal falling edge), r (signal rising edge).
                                            events of the interface
       rY0 := turn the shaft motor one step   rotate
       fX0 := end of a shaft-rotation step
    rX0fX0 := stop the motor                  eor := end of rotation
*/
 Input = {rY0, fX0, rX0fY0};
 Output = {eor, rotate, silentEvent}

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
            tf: {(x0,rY0,x1),
                 (x1,fX0,x2),    (x1,rY0,illegal),
                 (x2,rX0fY0,x0), (x2,rY0,illegal) },
            // other uncontrollabled transitions on fX0, rX0
            // are almost impossible (statistically validated)
            x_0: x0,
            Xm: {x0},
            ctrlEvents: {rY0} ) &
 bound_G_FB = 3
 &
 G_ts = rec(states: StatesG_A - {illegal},
            events: Input,
            tf: {(x0,rY0,x1), (x1,fX0,x2), (x2,rX0fY0,x0)},
            x_0: x0,
            Xm: {x0},
            ctrlEvents: {rY0} ) &
 bound_G_ts = 3
 &
 G_A = rec(states: StatesG_A - {illegal},
           inputs: Input,
           outputs: Output,
           tf: {(x0,(rY0,rotate),x1), (x1,(fX0,silentEvent),x2), (x2,(rX0fY0,eor),x0)},
           x_0: x0,
           Xm: {x0},
           ctrlEvents: {rY0} ) &
 bound_G_A = 3
 &
 G_I = rec(states: StatesG_I,
           events: VocalOutput,
           tf: {(SMnm,rotate,SMim), (SMim,eor,SMnm)},
           x_0: SMnm,
           Xm: {SMnm},
           ctrlEvents: {rotate} ) &
 bound_G_I = 2
END

/*
OUTPUT:

Number of states of the exhaustive model: 4
Number of transitions of the exhaustive model: 5
Number of states of the transition structure: 3
Number of transitions of the transition structure: 3
Number of states of the interface: 2
Number of transitions the interface: 2
cover(DisplayStatistics)
"The device (agent) is controllable."
cover(Check_controllability)
"The device (agent) is nonblocking."
cover(Check_nonblocking_Agent)
"The interface is nonblocking."
cover(Check_nonblocking_Interface)
Disabled events: {(x0|->{}),(x1|->{rY0}),(x2|->{rY0})}
Enabled events: {(x0|->{rY0}),(x1|->{fX0}),(x2|->{rX0fY0})}
"The SFBC functions are complementary."
Control law: {(rY0|->x0)}
cover(Display_SFBC_function)

% Finished processing file after 270 ms walltime (270 ms runtime), since start: 1960 ms


Size of the prefix-closed language of the exhaustive behavior: 6
Size of the marked language of the exhaustive behavior: 2
Number of states of the agent: 3
Number of transitions of the agent: 3
Size of the prefix-closed language of the agent: 4
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
The supervisor (relation): {(x0|->rY0),(x1|->fX0),(x2|->rX0fY0)}
cover(Display_supervisor)
State realization of the supervisor (disabled events): {(x1|->{rY0}),(x2|->{rY0})}
State realization of a supervisor (enabled events): {(x0|->{rY0}),(x1|->{fX0}),(x2|->{rX0fY0})}
cover(Display_supervisors)
"Marking is consistent."
cover(Check_WeakConsistencyOfMarking)
"The causal map is an observer."
cover(Check_ObserverProperty)
"Control coincidence property is satisfied."
cover(Check_ConditionOnSCT)
"The interface is nonblocking."
cover(Check_NonBlocking_Interface)

% Finished processing file after 310 ms walltime (300 ms runtime), since start: 2160 ms
*/