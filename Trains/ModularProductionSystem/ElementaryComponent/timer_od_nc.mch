// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL timer_od_nc
/*
  An on delay timer (i.e., a memorized timer encapsulated in a component) with
   - actuator (memory element) M0 to enable, disable, or reset the timer (with input
     OFF/ON) and
   - sensor (memory element) M1 to detect that timing delay is elapsed (with output
     OFF/ON).
  The timer behavior is the following:
   1) the timer is initially CLEARED (i.e., input and output are both OFF);
   2) when the timer is enabled (i.e., the input is set to ON), it accumulates
      time upward from 0 toward a preset value (i.e, the timing delay);
   3) the previous operation CANNOT be cancelled (nc) before the timer has reached
      the preset value (the output does not change);
   4) when the timing delay is elapsed, there is a rising edge pM1 (i.e., the output
      turns to ON) and the accumulator continues to increase until the timer is
      disabled (i.e., the input is set to OFF) having as consequence a falling edge
      fM1 (the output turns OFF);
   5) when the timer is disabled, the accumulator value is set to 0.

  The following declaration
    T0 : ODT[S := pM0, nM1, dt]
  declares a memorized on delay timer, where
   - the activation signal S is defined by the logical formula pM0;
   - the Boolean memory element M1, in negative form (i.e., nM1), indicates
     the initial state of the timer (nT0);
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
    x0 := nM0nM1                 i := idle
    x1 := pM0nM1                ip := in progress (accumulator increases)
    x2 := pM0pM1               rtc := ready to clear
    x3 := nM0pM1                      delay expired, accumulator has reached
                                      or exceeded the preset value
*/
 StatesG_A = {x0, x1, x2, x3, illegal};
 StatesG_I = {Ti, Tip, Trtc};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <M0, M1>
    State values = {nM0, pM0, nM1, pM1}
      GetStateValues : G_FB'states --> POW(StateValues):
        GetStateValues(x0) = {nM0, nM1}
        GetStateValues(x1) = {pM0, nM1)
        GetStateValues(x2) = {pM0, pM1}
        GetStateValues(x3) = {nM0, pM1}

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(fM0) = {pM0, pM1}
      ControlLaw(rM0) = {nM0, nM1}

    Observer:
      Project_fM0() := { return Func_dsbl(); }
      Project_rM0() := { return Func_enbl(); }
      Project_rM1() := { return Func_dexp(); }

    Command to be sent to a memory element to reflect the occurrence an event:
      Lactions_fM0() := { reset(M0); }
      Lactions_rM0() := { set(M0); }
      Lactions_fM1() := { reset(M1); }
      Lactions_rM1() := { set(M1); }

    Cycle() := Cycle() + {
      if ( nM0 & pM1 ) then Func_fM1();          // return to the initial state
      if ( pT0 & pM0 & nM1 ) then Func_rM1();    // detect that the timing delay is elapsed
      Eval_T(T0, pM0 & nM1)                      // accumulate time upward
      Func_fM0();
      Func_rM0(); }
*/

/*
  Commands of the actuator and signals of the sensor:
    f (signal falling edge), r (signal rising edge).
                                       events of the interface
    fM0 := input set to OFF              dsbl := disable
    rM0 := input set to ON               enbl := enable
    fM1 := output turns OFF
    rM1 := output set to ON              dexp := delay expired
*/
 Input = {fM0, rM0, fM1, rM1};
 Output = {dexp, dsbl, enbl, silentEvent}

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
            tf: {(x0,rM0,x1), (x0,fM0,illegal),
                 (x1,rM1,x2), (x1,fM0,illegal), (x1,rM0,illegal),
                 (x2,fM0,x3),                   (x2,rM0,illegal),
                 (x3,fM1,x0), (x3,fM0,illegal), (x3,rM0,illegal) },
            // other uncontrollabled transitions on fM1 and rM1
            // are almost impossible (statistically validated), unless the timer is faulty
            x_0: x0,
            Xm: {x0},        // Add x3 for consistency of marking
            ctrlEvents: {fM0, rM0} ) &
 bound_G_FB = 4
 &
 G_ts = rec(states: StatesG_A - {illegal},
            events: Input,
            tf: {(x0,rM0,x1), (x1,rM1,x2), (x2,fM0,x3), (x3,fM1,x0) },
            x_0: x0,
            Xm: {x0},        // Add x3 for consistency of marking
            ctrlEvents: {fM0, rM0} ) &
 bound_G_ts = 4
 &
 G_A = rec(states: StatesG_A - {illegal},
           inputs: Input,
           outputs: Output,
           tf: {(x0,(rM0,enbl),x1), (x1,(rM1,dexp),x2),
                (x2,(fM0,dsbl),x3), (x3,(fM1,silentEvent),x0) },
           x_0: x0,
           Xm: {x0},         // Add x3 for consistency of marking
           ctrlEvents: {fM0, rM0} ) &
 bound_G_A = 4
 &
 G_I = rec(states: StatesG_I,
           events: VocalOutput,
           tf: {(Ti,enbl,Tip), (Tip,dexp,Trtc), (Trtc,dsbl,Ti)},
           x_0: Ti,
           Xm: {Ti},
           ctrlEvents: {dsbl, enbl} ) &
 bound_G_I = 3
END

/*
OUTPUT:

Number of states of the exhaustive model: 5
Number of transitions of the exhaustive model: 10
Number of states of the transition structure: 4
Number of transitions of the transition structure: 4
Number of states of the interface: 3
Number of transitions the interface: 3
cover(DisplayStatistics)
"The device (agent) is controllable."
cover(Check_controllability)
"The device (agent) is nonblocking."
cover(Check_nonblocking_Agent)
"The interface is nonblocking."
cover(Check_nonblocking_Interface)
Disabled events: {(x0|->{fM0}),(x1|->{fM0,rM0}),(x2|->{rM0}),(x3|->{fM0,rM0})}
Enabled events: {(x0|->{rM0}),(x1|->{rM1}),(x2|->{fM0}),(x3|->{fM1})}
"The SFBC functions are complementary."
Control law: {(fM0|->x2),(rM0|->x0)}
cover(Display_SFBC_function)

% Finished processing file after 270 ms walltime (270 ms runtime), since start: 1990 ms


Size of the prefix-closed language of the exhaustive behavior: 11
Size of the marked language of the exhaustive behavior: 2
Number of states of the agent: 4
Number of transitions of the agent: 4
Size of the prefix-closed language of the agent: 5
Size of the marked language of the agent: 2
Number of states of the interface: 3
Number of transitions the interface: 3
Size of the prefix-closed language of the interface: 4
Size of the marked language of the interface: 2
cover(DisplayStatistics)
"The device (agent) is controllable."
cover(Check_Controllability)
"The device (agent) is Lm_closed."
cover(Check_Lm_closed)
The supervisor (relation): {(x0|->rM0),(x1|->rM1),(x2|->fM0),(x3|->fM1)}
cover(Display_supervisor)
State realization of the supervisor (disabled events): {(x0|->{fM0}),(x1|->{fM0,rM0}),(x2|->{rM0}),(x3|->{fM0,rM0})}
State realization of a supervisor (enabled events): {(x0|->{rM0}),(x1|->{rM1}),(x2|->{fM0}),(x3|->{fM1})}
cover(Display_supervisors)
"Marking is weakly consistent."
cover(Check_WeakConsistencyOfMarking)
"The causal map is an observer."
cover(Check_ObserverProperty)
"Control coincidence property is satisfied."
cover(Check_ConditionOnSCT)
"The interface is nonblocking."
cover(Check_NonBlocking_Interface)

% Finished processing file after 360 ms walltime (350 ms runtime), since start: 2220 ms
*/