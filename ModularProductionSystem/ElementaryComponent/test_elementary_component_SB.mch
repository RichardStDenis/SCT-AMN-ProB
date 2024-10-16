// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL test_elementary_component_SB

SEES
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------

  conveyor_belt /*@file "./conveyor_belt.mch" */
//injector /*@file "./injector.mch" */
//jack100 /*@file "./jack100.mch" */
//jack101 /*@file "./jack101.mch" */
//jack110 /*@file "./jack110.mch" */
//jack111 /*@file "./jack111.mch" */
//jack111ce /*@file "./jack111ce.mch" */
//jack111cr /*@file "./jack111cr.mch" */
//jack111cecr /*@file "./jack111cecr.mch" */
//jack111_2ms /*@file "./jack111_2ms.mch" */
//jack111min /*@file "./jack111min.mch" */
//jack211e /*@file "./jack211e.mch" */
//jack211r /*@file "./jack211r.mch" */
//jack211rmin /*@file "./jack211rmin.mch" */
//relay_ad /*@file "./relay_ad.mch" */
//relay_ad_cc /*@file "./relay_ad_cc.mch" */
//relay_ad_co /*@file "./relay_ad_co.mch" */
//relay_ad_ccco /*@file "./relay_ad_ccco.mch" */
//relay_ad_min /*@file "./relay_ad_min.mch" */
//relay_nd /*@file "./relay_nd.mch" */
//relay_nd_min /*@file "./relay_nd_min.mch" */
//relay_sd /*@file "./relay_sd.mch" */
//relay_sd_min /*@file "./relay_sd_min.mch" */
//stepperMotor /*@file "./stepperMotor.mch" */
//suctionCup /*@file "./suctionCup.mch" */
//suctionCup_cp /*@file "./suctionCup_cp.mch" */
//timer_od /*@file "./timer_od.mch" */
//timer_od_e /*@file "./timer_od_e.mch" */
//timer_od_nc /*@file "./timer_od_nc.mch" */
//timer_od_nc_e /*@file "./timer_od_nc_e.mch" */

CONSTANTS
 Q       // The set of good states representing a predicate

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 1000000;
 SET_PREF_SMT == FALSE;

 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../../Theories/Mealy_basis.def";
 "../../Theories/SBCDES.def";
 "LibraryIO.def";

VARIABLES
 code,        // Sequencing code
 sfbc_d,      // SFBC function for the set of disabled events at any state x
 sfbc_e       // SFBC function for the set of enabled events at any state x

INITIALISATION
 code := 0;
 sfbc_d := {}; sfbc_e := {}

INVARIANT
 code >= 0 & code <= 5 &
 sfbc_d : SFBCf(G_FB'states,G_FB'ctrlEvents) &
 sfbc_e : SFBCf(G_FB'states,G_FB'events)

PROPERTIES
 G_FB : Generator_C(G_FB'states,G_FB'events) &
 G_ts : Generator_C(G_ts'states,G_ts'events) &
 G_ts'states = G_FB'states - {illegal} & G_ts'events = G_FB'events & G_ts'tf <: G_FB'tf &
 G_ts'x_0 = G_FB'x_0 & G_ts'Xm = G_FB'Xm &
 G_ts'ctrlEvents = G_FB'ctrlEvents &
 G_A : Mealy_machine(G_A'states,G_A'inputs, G_A'outputs) &
 G_A'states = G_ts'states & G_A'inputs = G_ts'events &
 GetInTransition_Mealy(G_A, G_A'inputs, G_A'outputs, G_A'states) = G_ts'tf &
 G_A'x_0 = G_ts'x_0 & G_A'Xm = G_ts'Xm &
 G_A'ctrlEvents = G_ts'ctrlEvents &
 G_I : Generator_C(G_I'states,G_I'events) &
 Q = G_ts'states

OPERATIONS
 DisplayStatistics = PRE code = 0 THEN
  PRINTF("Number of states of the exhaustive model: ", [card(G_FB'states)]);
  PRINTF("Number of transitions of the exhaustive model: ", [card(G_FB'tf)]);
  PRINTF("Number of states of the transition structure: ", [card(G_ts'states)]);
  PRINTF("Number of transitions of the transition structure: ", [card(G_ts'tf)]);
  PRINTF("Number of states of the interface: ", [card(G_I'states)]);
  PRINTF("Number of transitions the interface: ", [card(G_I'tf)]);
  code := 1 END;

 Check_controllability = PRE code = 1 &
  Controllable(G_FB, Q) THEN
   code := 2; PRINT("The device (agent) is controllable.") END;

 Check_nonblocking_Agent = PRE code = 2 &
  Nonblocking(G_FB, Q) THEN
   code := 3; PRINT("The device (agent) is nonblocking.") END;

 Check_nonblocking_Interface = PRE code = 3 &
  Nonblocking(G_I, G_I'states) THEN
   code := 4; PRINT("The interface is nonblocking.") END;

 Display_SFBC_function = PRE code = 4 THEN
  LET ctrl_field BE
   ctrl_field = CtrlField(G_FB, Q) IN
    sfbc_d := SFBC(G_FB,ctrl_field);
    sfbc_e := SFBCEnabled(G_FB,ctrl_field);
    PRINTF("Disabled events: ", [sfbc_d]);
    PRINTF("Enabled events: ", [sfbc_e]);
    IF !x_.(x_ : ctrl_field => sfbc_d (x_) /\ sfbc_e(x_) = {}) THEN
     PRINT("The SFBC functions are complementary.") END;
    LET FB_f BE
     FB_f = G_FB'ctrlEvents<|(SFBCEnabled_(G_FB,ctrl_field)~) IN
      PRINTF("Control law: ", [FB_f]) END END;
  code := 5 END END