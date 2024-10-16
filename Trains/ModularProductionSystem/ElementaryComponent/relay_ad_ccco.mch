// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL relay_ad_ccco
/*
  A mechanical relay (to electrically turn on/off the power supplied to another device,
  while considering latency effects) with asymmetric delay,
   - actuator Y0 to close/open the relay, cancel close, and cancel open
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
    x0 := nY0M0nM1             off (no electrical contact)
    x1 := Y0M0nM1              t1at := timer 1 accumulates time (preset value not reached)
    x2 := Y0nM0nM1                     timer 1 accumulates time
    x3 := Y0nM0M1               on (electrical contact)
    x4 := nY0nM0M1             t0at := timer 0 accumulates time (preset value not reached)
    x5 := nY0nM0nM1                    timer 0 accumulates time
  _M0M1 is physically impossible due to the setting of delays.
*/
 StatesG_A = {x0, x1, x2, x3, x4, x5, illegal};
 StatesG_I = {Roff, Ron, Rt0at, Rt1at};

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
      ControlLaw(fY0) = pY0 & nX0
      ControlLaw(rY0) = nY0 & nM1

    Observer:
      Project_fY0() := {
        if ( pM1 ) then return Func_deactivate()
                   else return Func_ca(); }
      Project_rY0() := {
        if ( pM0 ) then return Func_activate()
                   else return Func_cd(); }
      Project_rM0() := {
        if ( nY0 ) then return Func_eod()
                   else return true;   // the event is erased }
      Project_rM1() := {
        if ( pY0 ) then return Func_eoa()
                   else return true;   // the event is erased }

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
    fY0 := open the relay                deactivate (the device)
           cancel close                   ca := cancel activation
    rY0 := close the relay               activate (the device)
           cancel open                    cd := cancel deactivation
    fM0 := timer cleared
    rM0 := relay opened                  eod := end of deactivation
    fM1 := timer cleared
    rM1 := relay closed                  eoa := end of activation
*/
 Input = {fY0, rY0, fM0, rM0, fM1, rM1};
 Output = {activate, ca, cd, deactivate, eoa, eod, silentEvent}

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
                 (x1,fM0,x2),                           (x1,fY0,illegal), (x1,rY0,illegal),
                 (x2,rM1,x3), (x2,fY0,x5), (x2,rM0,x1),                   (x2,rY0,illegal),
                 (x3,fY0,x4),                                             (x3,rY0,illegal),
                 (x4,fM1,x5),                           (x4,fY0,illegal), (x4,rY0,illegal),
                 (x5,rM0,x0), (x5,rY0,x2), (x5,rM1,x4), (x5,fY0,illegal) },
            // other uncontrollabled transitions on fM0, rM0, fM1, and rM1
            // are almost impossible (statistically validated), unless the relay is faulty.
            x_0: x0,
            Xm: {x0},
            ctrlEvents: {fY0, rY0} ) &
 bound_G_FB = 12
 &
 G_ts = rec(states: StatesG_A - {illegal},
            events: Input,
            tf: {(x0,rY0,x1), (x1,fM0,x2), (x2,rM1,x3), (x2,fY0,x5), (x2,rM0,x1),
                 (x3,fY0,x4), (x4,fM1,x5), (x5,rM0,x0), (x5,rY0,x2), (x5,rM1,x4) },
            x_0: x0,
            Xm: {x0},
            ctrlEvents: {fY0, rY0} ) &
 bound_G_ts = 12
 &
 G_A = rec(states: StatesG_A - {illegal},
           inputs: Input,
           outputs: Output,
           tf: {(x0,(rY0,activate),x1),    (x1,(fM0,silentEvent),x2), (x2,(rM1,eoa),x3),
                (x2,(fY0,ca),x5),          (x2,(rM0,silentEvent),x1), (x3,(fY0,deactivate),x4),
                (x4,(fM1,silentEvent),x5), (x5,(rM0,eod),x0),         (x5,(rY0,cd),x2),
                (x5,(rM1,silentEvent),x4) },
           x_0: x0,
           Xm: {x0},
           ctrlEvents: {fY0, rY0} ) &
 bound_G_A = 14
 &
 G_I = rec(states: StatesG_I,
           events: VocalOutput,
           tf: {(Roff,activate,Rt1at),  (Rt1at,eoa,Ron),  (Rt1at,ca,Rt0at),
                (Ron,deactivate,Rt0at), (Rt0at,eod,Roff), (Rt0at,cd,Rt1at) },
           x_0: Roff,
           Xm: {Roff},
           ctrlEvents: {activate, ca, cd, deactivate} ) &
 bound_G_I = 7
 &
 agentIOonLoops = {(fY0,ca), (rY0,cd), (rY0,activate), (rM0,silentEvent), (rM1,silentEvent)} &
 agentEventsOnLoops = {rM0, rM1} &
 interfaceEventsOnLoops = {activate, ca, cd}
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
Enabled events: {(x0|->{rY0}),(x1|->{fM0}),(x2|->{fY0,rM0,rM1}),(x3|->{fY0}),(x4|->{fM1}),(x5|->{rY0,rM0,rM1})}
"The SFBC functions are complementary."
Control law: {(fY0|->x2),(fY0|->x3),(rY0|->x0),(rY0|->x5)}
cover(Display_SFBC_function)

% Finished processing file after 280 ms walltime (270 ms runtime), since start: 1990 ms


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
cover(Check_Controllability)
"The device (agent) is Lm_closed."
cover(Check_Lm_closed)
The supervisor (relation): {(x0|->rY0),(x1|->fM0),(x2|->fY0),(x2|->rM0),(x2|->rM1),(x3|->fY0),(x4|->fM1),(x5|->rY0),(x5|->rM0),(x5|->rM1)}
cover(Display_supervisor)
State realization of the supervisor (disabled events): {(x0|->{fY0}),(x1|->{fY0,rY0}),(x2|->{rY0}),(x3|->{rY0}),(x4|->{fY0,rY0}),(x5|->{fY0})}
State realization of a supervisor (enabled events): {(x0|->{rY0}),(x1|->{fM0}),(x2|->{fY0,rM0,rM1}),(x3|->{fY0}),(x4|->{fM1}),(x5|->{rY0,rM0,rM1})}
cover(Display_supervisors)
"Marking is consistent."
cover(Check_WeakConsistencyOfMarking)
"The causal map is an observer."
cover(Check_ObserverProperty)
"Control coincidence property is satisfied."
cover(Check_ConditionOnSCT)
"The interface is nonblocking."
cover(Check_NonBlocking_Interface)

% Finished processing file after 8290 ms walltime (7480 ms runtime), since start: 10140 ms
*/