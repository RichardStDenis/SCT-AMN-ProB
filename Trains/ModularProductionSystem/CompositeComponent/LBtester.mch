// copyright Richard St_Denis, Universite de Sherbrooke, 2024.
MODEL LBtester
/*
  The tester consists of a probe mounted on a jack that is deployed into a hole
  of a workpiece to test its depth. Its deployment can be canceled anytime. The
  test is positive when the cylinder is completely deployed and negative otherwise.
  Since there is no sensor to detect the negative case, a timer is used. Its expiration
  indicates the negative case.

  Its control specification is almost explicit in the assembly diagram. The loop
  transitions added to the two component instances are effectively the expression
  of constraints that have been made explicit by sharing events between the the probe
  and timer.
*/

SETS
/*
   probe states:                       in tester
     Pe := probe completely extended     down
    Pie := probe in extension            in descent
    Pir := probe in retraction           in ascent
     Pr := probe completely retracted    up
  timer states:
      Ti := timer idle
     Tip := timer in progress
    Trtc := timer ready to clear

  tester states:
     TSTa := active
     TSTr := ready
     TSTw := waiting
*/
 StatesG_A = {Pe_Ti, Pe_Tip, Pe_Trtc, Pie_Ti, Pie_Tip, Pie_Trtc,
              Pir_Ti, Pir_Trtc, Pr_Ti, Pr_Trtc};
 StatesG_I = {TSTa, TSTr, TSTw};
/*
  Abstract dynamic (D. Côté, doctoral thesis, 2011):
    State variables: <probe, timer>

    ControlLaw : G_FB'events --> POW(StateValues):
      ControlLaw(P_cd) = Pie & Trtc
      ControlLaw(P_gd) = Pr & Ti
      ControlLaw(P_gu) = Pe & (Ti | Trtc)
      ControlLaw(T_clr) = (Pe | Pir | Pr) & Trtc
      ControlLaw(T_rst) = Pe & Tip
      ControlLaw(T_set) = Pie & Ti

    Observer:
      Project_Pcd() := { return Func_failure(); }
      Project_Peoa() := { return Func_eot(); }
      Project_Peod() := { return Func_success(); }
      Project_Pgd() :=  { return Func_test(); }
*/

/*
  Commands and signals:
    probe events:                      tester events (interface):
      P_cd := cancel descent             failure := negative case
     P_eoa := end of ascent                  eot := end of test
     P_eod := end of descent             success := positive case
      P_gd := go down                       test := start the test procedure
      P_gu := go up
    timer events :
      T_clr := clear timer
      T_exp := time interval expired
      T_rst := reset the timer
      T_set := set (enable) the timer
*/
 Input = {P_cd, P_eod, P_eoa, P_gd, P_gu, T_clr, T_exp, T_rst, T_set};
 Output = {eot, failure, success, test, silentEvent}

CONSTANTS
 VocalOutput, // The set of vocal outputs
 //G_FB,         Deterministic automaton exhaustively modeling the device behavior
 //              (defined in file SBtester)
 G_ts,        // G_FB under control of a state feedback function (solution of a control problem)
 bound_G_ts,  // Bound on the trace length of the controlled device behavior
 G_A,         // Mealy machine modeling the device implementation (agent)
 bound_G_A,   // Bound on the trace length of the agent
 G_I,         // (quotient deterministic) automaton modeling a device abstraction (interface)
 bound_G_I    // Bound on the trace length of the interface

PROPERTIES
 VocalOutput = Output - {silentEvent}
 &
 G_ts = rec(states: StatesG_A,
            events: Input,
            tf: {(Pr_Ti,P_gd,Pie_Ti), (Pie_Ti,P_eod,Pe_Ti),
                 (Pe_Ti,P_gu,Pir_Ti), (Pir_Ti,P_eoa,Pr_Ti),
                 (Pie_Ti,T_set,Pie_Tip), (Pie_Tip,P_eod,Pe_Tip),   (Pe_Tip,T_exp,Pe_Trtc),
                                         (Pie_Tip,T_exp,Pie_Trtc), (Pie_Trtc,P_eod,Pe_Trtc),
                 (Pe_Tip,T_rst,Pe_Ti),
                 (Pie_Trtc,P_cd,Pir_Trtc), (Pir_Trtc,T_clr,Pir_Ti),
                 (Pe_Trtc,T_clr,Pe_Ti),
                 (Pe_Trtc,P_gu,Pir_Trtc), (Pir_Trtc,P_eoa,Pr_Trtc), (Pr_Trtc,T_clr,Pr_Ti) },
            x_0: Pr_Ti,
            Xm: {Pr_Ti},
            ctrlEvents: {P_cd, P_gd, P_gu, T_clr, T_rst, T_set} ) &
 bound_G_ts = 7
 & 
 G_A = rec(states: StatesG_A,
           inputs: Input,
           outputs: Output,
           tf: {(Pr_Ti,(P_gd,test),Pie_Ti),         (Pie_Ti,(P_eod,success),Pe_Ti),
                (Pe_Ti,(P_gu,silentEvent),Pir_Ti),  (Pir_Ti,(P_eoa,eot),Pr_Ti),
                (Pie_Ti,(T_set,silentEvent),Pie_Tip), (Pie_Tip,(P_eod,success),Pe_Tip),       (Pe_Tip,(T_exp,silentEvent),Pe_Trtc),
                                                      (Pie_Tip,(T_exp,silentEvent),Pie_Trtc), (Pie_Trtc,(P_eod,success),Pe_Trtc),
                (Pe_Tip,(T_rst,silentEvent),Pe_Ti),
                (Pie_Trtc,(P_cd,failure),Pir_Trtc), (Pir_Trtc,(T_clr,silentEvent),Pir_Ti),
                (Pe_Trtc,(T_clr,silentEvent),Pe_Ti),
                (Pe_Trtc,(P_gu,silentEvent),Pir_Trtc), (Pir_Trtc,(P_eoa,eot),Pr_Trtc), (Pr_Trtc,(T_clr,silentEvent),Pr_Ti) },
           x_0: Pr_Ti,
           Xm: {Pr_Ti},
           ctrlEvents: {P_cd, P_gd, P_gu, T_clr, T_rst, T_set} ) &
 bound_G_A = 7
 &
 G_I = rec(states: StatesG_I,
           events: Output - {silentEvent},
           tf: {(TSTr,test,TSTw), (TSTw,failure,TSTa), (TSTw,success,TSTa), (TSTa,eot,TSTr)},
           x_0: TSTr,
           Xm: {TSTr},
           ctrlEvents: {test} ) &
 bound_G_I = 3
END

/*
OUTPUT:

% Runtime for SOLUTION for SETUP_CONSTANTS: 210 ms (walltime: 220 ms)
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2140 ms
Number of states of the transition system 10
Number of transitions of the transition system: 16
Size of the prefix-closed language of the transition system: 34
Size of the marked language of the transition system: 11
Number of states of the agent: 10
Number of transitions of the agent: 16
Size of the prefix-closed language of the agent: 34
Size of the marked language of the agent: 11
Number of states of the interface: 3
Number of transitions the interface: 4
Size of the prefix-closed language of the interface: 6
Size of the marked language of the interface: 3
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2150 ms
"Marking is weakly consistent."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2240 ms
"The causal map is an observer."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2360 ms
"Control coincidence property is satisfied."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2360 ms
"The interface is nonblocking."
Finished Invariant Checking 0 ms walltime (0 ms runtime), since start: 2360 ms
Deadlock reached after 7 steps (after Check_NonBlocking_Interface).

% Finished processing file after 610 ms walltime (540 ms runtime), since start: 2380 ms
*/