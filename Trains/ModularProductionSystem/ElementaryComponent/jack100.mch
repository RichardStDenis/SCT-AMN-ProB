// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL jack100
/*
  A jack with
   - three valves for one stable position (1) and
   - actuator Y0 to extend and retract the cylinder.
  The sensors to detect complete extension (T1) and complete retraction (T0) are
  replaced by two memorized timers of type on-delay timer (ODT) with rising signal
  when the delay expired. In the initial state T0 is expired and T1 is cleared:
    T0 : ODT[S := nY0 & nM1, pM0, dt0] (dt0 is the delay)
         - the activation signal S is defined by the logical formula nY0 & nM1;
         - the Boolean memory element M0, in positive form (i.e., pM0), indicates
           the initial state of the timer (pT0);
         - the constant timing delay is dt0.
    T1 : ODT[S := pY0 & nM0, nM1, dt1] (dt1 is the delay).
         - the activation signal S is defined by the logical formula pY0 & nM0;
         - the Boolean memory element M1, in negative form (i.e., nM1), indicates
           the initial state of the timer (nT1);
         - the constant timing delay is dt1.
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
    x0 := nY0M0nM1              r := completely retracted
    x1 := Y0M0nM1              ie := in extension
    x2 := Y0nM0nM1                   in extension
    x3 := Y0nM0M1               e := completely extended
    x4 := nY0nM0M1             ir := in retraction
    x5 := nY0nM0nM1                  in retraction
  _M0M1 is physically possible since rM0 (resp. rM1) can occur before fM1 (resp. fM0).
  Here these situation does not occur on a PLC. Thus, they discarded and, at worst,
  these events are perceived as simultaneous.
*/
 StatesG_A = {x0, x1, x2, x3, x4, x5, illegal};
 StatesG_I = {Je, Jie, Jir, Jr};

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
      ControlLaw(fY0) = {pY0, pM1}
      ControlLaw(rY0) = {nY0, pM0}

    Observer:
      Project_fY0() := { return Func_retract(); }
      Project_rY0() := { return Func_extend(); }
      Project_rM0() := { return Func_eor(); }
      Project_rM1() := { return Func_eoe(); }

    Command to be sent to an actuator to reflect the occurrence an event:
      Lactions_fY0() := { reset(Y0); }
      Lactions_rY0() := { set(Y0); }
      Lactions_fM0() := { reset(M0); }
      Lactions_rM0() := { set(M0); }
      Lactions_fM1() := { reset(M1); }
      Lactions_rM1() := { set(M1); }

    Cycle() := Cycle() + {
      if ( (pY0 | pM1) & pM0 ) then Func_fM0();
      if ( (nY0 | pM0) & pM1 ) then Func_fM1();
      if ( pT0 & nY0 & nM1 & nM0 ) then Func_rM0();
      eval(T0, nY0 & nM1 & nM0);
      if ( pT1 & pY0 & nM0 & nM1 ) then Func_rM1();
      eval(T1, pY0 & nM0 & nM1);
      Func_fY0();
      Func_rY0(); }
*/

/*
  Commands of the actuator and signals of sensors:
    f (signal falling edge), r (signal rising edge).
                                       events of the interface
    fY0 := retract                       retract
    rY0 := extend                        extend
    fM0 := in extension
    rM0 := end of retraction             eor := end of retraction
    fM1 := in retraction
    rM1 := end of extension              eoe := end of extension
*/
 Input = {fY0, rY0, fM0, rM0, fM1, rM1};
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
            tf: {(x0,rY0,x1), (x0,fY0,illegal),
                 (x1,fM0,x2), (x1,fY0,illegal), (x1,rY0,illegal),
                 (x2,rM1,x3), (x2,fY0,illegal), (x2,rY0,illegal),
                 (x3,fY0,x4),                   (x3,rY0,illegal),
                 (x4,fM1,x5), (x4,fY0,illegal), (x4,rY0,illegal),
                 (x5,rM0,x0), (x5,fY0,illegal), (x5,rY0,illegal) },
            // other uncontrollabled transitions on fM0, rM0, fM1, and rM1
            // are almost impossible (statistically validated), unless the cylinder sticks.
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
           tf: {(x0,(rY0,extend),x1),  (x1,(fM0,silentEvent),x2), (x2,(rM1,eoe),x3),
                (x3,(fY0,retract),x4), (x4,(fM1,silentEvent),x5), (x5,(rM0,eor),x0) },
           x_0: x0,
           Xm: {x0},
           ctrlEvents: {fY0, rY0} ) &
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
Disabled events: {(x0|->{fY0}),(x1|->{fY0,rY0}),(x2|->{fY0,rY0}),(x3|->{rY0}),(x4|->{fY0,rY0}),(x5|->{fY0,rY0})}
Enabled events: {(x0|->{rY0}),(x1|->{fM0}),(x2|->{rM1}),(x3|->{fY0}),(x4|->{fM1}),(x5|->{rM0})}
"The SFBC functions are complementary."
Control law: {(fY0|->x3),(rY0|->x0)}
cover(Display_SFBC_function)

% Finished processing file after 280 ms walltime (270 ms runtime), since start: 1980 ms


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

% Finished processing file after 350 ms walltime (340 ms runtime), since start: 2170 ms
*/