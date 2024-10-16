// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL jack111cecr
/*
  A jack with
   - three valves for one stable position (1),
   - sensor X0 to detect complete retraction (1),
   - sensor X1 to detect complete extension (1), and
   - actuator Y0 to extend and retract the cylinder, cancel extension, and
     cancel retraction.
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
    x0 := nY0X0nX1              r := completely retracted
    x1 := Y0X0nX1              ie := in extension
    x2 := Y0nX0nX1                   in extension
    x3 := Y0nX0X1               e := completely extended
    x4 := nY0nX0X1             ir := in retraction
    x5 := nY0nX0nX1                  in retraction
  _X0X1 is physically impossible because the cylinder cannot be simultaneously
  at the far beginning and the far end.
*/
 StatesG_A = {x0, x1, x2, x3, x4, x5, illegal};
 StatesG_I = {Je, Jie, Jir, Jr};

/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <Y0, X0, X1>
    StateValues = {nY0, pY0, nX0, pX0, nX1, pX1}
      GetStateValues : G_FB'states --> POW(StateValues):
        GetStateValues(x0) = {nY0, pX0, nX1}
        GetStateValues(x1) = {pY0, pX0, nX1)
        GetStateValues(x2) = {pY0, nX0, nX1}
        GetStateValues(x3) = {pY0, nX0, pX1}
        GetStateValues(x4) = {nY0, nX0, pX1}
        GetStateValues(x5) = {nY0, nX0, nX1}

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(fY0) = pY0 & nX0
      ControlLaw(rY0) = nY0 & nX1

    Observer:
      Project_fY0() := {
        if ( pX1 ) then return Func_retract()
                   else return Func_ce(); }
      Project_rY0() := {
        if ( pX0 ) then return Func_extend()
                   else return Func_cr(); }
      Project_rX0() := {
        if ( nY0 ) then return Func_eor()
                   else return true;   // the event is erased }
      Project_rX1() := {
        if ( pY0 ) then return Func_eoe()
                   else return true;   // the event is erased }

    Command to be sent to an actuator to reflect the occurrence an event:
      Lactions_fY0() := { reset(Y0); }
      Lactions_rY0() := { set(Y0); }

    Cycle() := Cycle() + {
      if ( rX0 ) then Func_rX0();
      if ( rX1 ) then Func_rX1();
      Func_fY0();
      Func_rY0(); }
*/

/*
  Commands of the actuator and signals of sensors:
    f (signal falling edge), r (signal rising edge).
                                       events of the interface
    fY0 := retract                       retract
           cancel extension               ce := cancel extension
    rY0 := extend                        extend
           cancel retraction              cr := cancel retraction
    fX0 := in extension
    rX0 := end of retraction             eor := end of retraction
    fX1 := in retraction
    rX1 := end of extension              eoe := end of extension
*/
 Input = {fY0, rY0, fX0, rX0, fX1, rX1};
 Output = {ce, cr, eoe, eor, extend, retract, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 G_FB,        // Deterministic automaton exhaustively modeling the device behavior
 bound_G_FB,  // Bound on the trace length of the device behavior
 G_ts,        // G_FB under control of a state feedback function (solution of a control problem)
 bound_G_ts,  // Bound on the trace length of the controlled device behavior
 G_A,         // Mealy machine modeling the device implementation (agent)
 bound_G_A,   // Bound on the trace length of the agent
 G_I,         // (quotient deterministic) automaton modeling a device abstraction (interface)
 bound_G_I,   // Bound on the trace length of the interface

 agentIOonLoops,             // The set of i/o not to be repeated in traces
 agentEventsOnLoops,         // The set events (inputs) not to be repeated in traces
 interfaceEventsOnLoops      // The set of events (outputs) not to be repeated in traces

PROPERTIES
 VocalOutput = Output - {silentEvent}
 &
 G_FB = rec(states: StatesG_A,
            events: Input,
            tf: {(x0,rY0,x1),                           (x0,fY0,illegal),
                 (x1,fX0,x2),                           (x1,fY0,illegal), (x1,rY0,illegal),
                 (x2,rX1,x3), (x2,fY0,x5), (x2,rX0,x1),                   (x2,rY0,illegal),
                 (x3,fY0,x4),                                             (x3,rY0,illegal),
                 (x4,fX1,x5),                           (x4,fY0,illegal), (x4,rY0,illegal),
                 (x5,rX0,x0), (x5,rY0,x2), (x5,rX1,x4), (x5,fY0,illegal) },
            // other uncontrollabled transitions on fX0, rX0, fX1, and rX1
            // are almost impossible (statistically validated), unless the cylinder sticks.
            x_0: x0,
            Xm: {x0},
            ctrlEvents: {fY0, rY0} ) &
 bound_G_FB = 12
 &
 G_ts = rec(states: StatesG_A - {illegal},
            events: Input,
            tf: {(x0,rY0,x1), (x1,fX0,x2), (x2,rX1,x3), (x2,fY0,x5), (x2,rX0,x1),
                 (x3,fY0,x4), (x4,fX1,x5), (x5,rX0,x0), (x5,rY0,x2), (x5,rX1,x4) },
            x_0: x0,
            Xm: {x0},
            ctrlEvents: {fY0, rY0} ) &
 bound_G_ts = 12
 &
 G_A = rec(states: StatesG_A - {illegal},
           inputs: Input,
           outputs: Output,
           tf: {(x0,(rY0,extend),x1),      (x1,(fX0,silentEvent),x2), (x2,(rX1,eoe),x3),
                (x2,(fY0,ce),x5),          (x2,(rX0,silentEvent),x1), (x3,(fY0,retract),x4),
                (x4,(fX1,silentEvent),x5), (x5,(rX0,eor),x0),         (x5,(rY0,cr),x2),
                (x5,(rX1,silentEvent),x4) },
           x_0: x0,
           Xm: {x0},
           ctrlEvents: {fY0, rY0} ) &
 bound_G_A = 14
 &
 G_I = rec(states: StatesG_I,
           events: VocalOutput,
           tf: {(Jr,extend,Jie),  (Jie,eoe,Je), (Jie,ce,Jir),
                (Je,retract,Jir), (Jir,eor,Jr), (Jir,cr,Jie) },
           x_0: Jr,
           Xm: {Jr},
           ctrlEvents: {ce, cr, extend, retract} ) &
 bound_G_I = 7
 &
 agentIOonLoops = {(fY0,ce), (rY0,cr), (rY0,extend), (rX0,silentEvent), (rX1,silentEvent)} &
 agentEventsOnLoops = {rX0, rX1} &
 interfaceEventsOnLoops = {ce, cr, extend}
END

/*
OUTPUT:
Number of states of the exhaustive model: 7
Number of transitions of the exhaustive model: 18
Number of states of the transition structure: 6
Number of transitions of the transition structure: 10
Number of states of the interface: 4
Number of transitions the interface: 6
cover(DisplayStatistics)
"The device (agent) is controllable."
cover(Check_controllability)
"The device (agent) is nonblocking."
cover(Check_nonblocking_Agent)
"The interface is nonblocking."
cover(Check_nonblocking_Interface)
Disabled events: {(x0|->{fY0}),(x1|->{fY0,rY0}),(x2|->{rY0}),(x3|->{rY0}),(x4|->{fY0,rY0}),(x5|->{fY0})}
Enabled events: {(x0|->{rY0}),(x1|->{fX0}),(x2|->{fY0,rX0,rX1}),(x3|->{fY0}),(x4|->{fX1}),(x5|->{rY0,rX0,rX1})}
"The SFBC functions are complementary."
Control law: {(fY0|->x2),(fY0|->x3),(rY0|->x0),(rY0|->x5)}
cover(Display_SFBC_function)

% Finished processing file after 280 ms walltime (280 ms runtime), since start: 1980 ms


Size of the prefix-closed language of the exhaustive behavior: 1030
Size of the marked language of the exhaustive behavior: 111
Number of states of the agent: 6
Number of transitions of the agent: 10
Size of the prefix-closed language of the agent: 132
Size of the marked language of the agent: 36
Number of states of the interface: 4
Number of transitions the interface: 6
Size of the prefix-closed language of the interface: 17
Size of the marked language of the interface: 6
cover(DisplayStatistics)
"The device (agent) is controllable."
cover(Check_controllability)
"The device (agent) is Lm_closed."
cover(Check_Lm_closed)
The supervisor (relation): {(x0|->rY0),(x1|->fX0),(x2|->fY0),(x2|->rX0),(x2|->rX1),(x3|->fY0),(x4|->fX1),(x5|->rY0),(x5|->rX0),(x5|->rX1)}
cover(Display_supervisor)
State realization of the supervisor (disabled events): {(x0|->{fY0}),(x1|->{fY0,rY0}),(x2|->{rY0}),(x3|->{rY0}),(x4|->{fY0,rY0}),(x5|->{fY0})}
State realization of a supervisor (enabled events): {(x0|->{rY0}),(x1|->{fX0}),(x2|->{fY0,rX0,rX1}),(x3|->{fY0}),(x4|->{fX1}),(x5|->{rY0,rX0,rX1})}
cover(Display_supervisors)
"Marking is consistent."
cover(Check_WeakConsistencyOfMarking)
"The causal map is an observer."
cover(Check_ObserverProperty)
"Control coincidence property is satisfied."
cover(Check_ConditionOnSCT)
"The interface is nonblocking."
cover(Check_NonBlocking_Interface)

% Finished processing file after 8240 ms walltime (7470 ms runtime), since start: 10090 ms
*/