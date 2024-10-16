// Richard St-Denis, Universite de Sherbrooke, 2023.
MODEL test_cc_cm_observer
 //---------------------------------------------------------------------
 // Model for input data
 //---------------------------------------------------------------------
SETS
 States_G_lo = {x1, x2, x3, x4, x5};
 Events_G_lo = {e1, e2, e3, e4};
 States_G_hi = {y1, y2, y3, y4};
 Events_G_hi = {t0, t1, t2}

CONSTANTS
 G_lo, M_G_lo,
 G_hi, M_G_hi,
 bound_Tr

ABSTRACT_CONSTANTS GetLgModel_lo, GetLgModel_hi

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 5000000;
 SET_PREF_SMT == FALSE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../Theories/SCT_core.def";
 "LibraryIO.def";

 CausalMap(map,E,T) == map : seq(E)+->seq(T);

 map_1(Mm,L,map) == {s_ | s_ : L & #t_.(t_ : Mm & map(s_) = t_)};
 ConsistencyOfMarking(L,Lm,Mm,map) == map_1(Mm,L,map) = Lm;

 Observer(L,M,T,theta) == !(s_,tau_).(s_ : L & tau_ : T &
  theta(s_)<-tau_ : M => #s__.(s__ : L & size(s__) > size(s_) &
   s__ /|\ size(s_) = s_ & theta(s__) = theta(s_)<-tau_ ) );

 X_tau(L,E,tau,theta) == {s_ | s_ : L-{[]} &
  #e_.(e_ : E & last(s_) = e_ &
   theta(s_) = theta(front(s_))<-tau ) };

 ControlCoincidence(L,E,Ec,Tc,theta) ==
  !tau_.(tau_ : Tc & X_tau(L,E,tau_,theta) /= {} =>
   !s__.(s__ : X_tau(L,E,tau_,theta) => last(s__) : Ec ) );

PROPERTIES
 bound_Tr = 10 &
 G_lo = rec(states: States_G_lo,
         events: Events_G_lo,
         tf: {(x1,e1,x2), (x1,e2,x3),
              (x2,e3,x4), (x3,e4,x5) },
         x_0: x1,
         Xm: {x4, x5},
         ctrlEvents : {e3, e4} ) &
 G_lo : Generator_C(States_G_lo,Events_G_lo) &

 GetLgModel_lo = lambdaGetLgModel(G_lo,bound_Tr,GetLgModel_lo) &
 M_G_lo = GetLgModel_lo(baseCaseTr(G_lo),baseCaseTk(G_lo),1) &

 G_hi = rec(states: States_G_hi,
         events: Events_G_hi,
         tf: {(y1,t0,y2), (y2,t1,y3), (y2,t2,y4) },
         x_0: y1,
         Xm: {y3, y4},
         ctrlEvents : {t1} ) &
 G_hi : Generator_C(States_G_hi,Events_G_hi) &

 GetLgModel_hi = lambdaGetLgModel(G_hi,bound_Tr,GetLgModel_hi) &
 M_G_hi = GetLgModel_hi(baseCaseTr(G_hi),baseCaseTk(G_hi),1)

VARIABLES
 code,   // Sequencing code
 theta   // A causal map

INVARIANT
 code >= 0 & code <= 7 &
 CausalMap(theta, G_lo'events, G_hi'events) 

INITIALISATION
 code := 0;
 theta := {[]|->[], [e1]|->[t0], [e2]|->[t0], [e1,e3]|->[t0,t1], [e2,e4]|->[t0,t2]}

OPERATIONS
 a_pair_of_languages = PRE code = 0 THEN
  PRINTF("Set of Strings: ~w~n", [prj1(POW(String(Events_G_lo)),POW(String(Events_G_lo)))(M_G_lo)]);
  PRINTF("Set of tasks: ~w~n", [prj2(POW(String(Events_G_lo)),POW(String(Events_G_lo)))(M_G_lo)]);
  PRINTF("Prefixes of tasks: ~w~n~n", [pr(prj2(POW(String(Events_G_lo)),POW(String(Events_G_lo)))(M_G_lo))]);
  PRINTF("Set of Strings: ~w~n", [prj1(POW(String(Events_G_hi)),POW(String(Events_G_hi)))(M_G_hi)]);
  PRINTF("Set of tasks: ~w~n", [prj2(POW(String(Events_G_hi)),POW(String(Events_G_hi)))(M_G_hi)]);
  PRINTF("Prefixes of tasks: ~w~n~n", [pr(prj2(POW(String(Events_G_hi)),POW(String(Events_G_hi)))(M_G_hi))]);
  PRINTF("Causal map: ~w~n", [theta]);
  code := 1 END;

 Check_ConsistencyMarking = PRE code = 1 &
  ConsistencyOfMarking(prj1(POW(String(Events_G_lo)),POW(String(Events_G_lo)))(M_G_lo),
                       prj2(POW(String(Events_G_lo)),POW(String(Events_G_lo)))(M_G_lo),
                       prj2(POW(String(Events_G_hi)),POW(String(Events_G_hi)))(M_G_hi), theta ) THEN
   code := 2; PRINT("Consistency of marking.") END;

 Check_ControlCoincidence = PRE code = 2 &
  ControlCoincidence(prj1(POW(String(Events_G_lo)),POW(String(Events_G_lo)))(M_G_lo),
                      G_lo'events,G_lo'ctrlEvents,G_hi'ctrlEvents,theta ) THEN
   code := 3; PRINT("Control coincidence.") END;

 CheckObserverProperty = PRE code = 3 &
  Observer(prj1(POW(String(Events_G_lo)),POW(String(Events_G_lo)))(M_G_lo),
           prj1(POW(String(Events_G_hi)),POW(String(Events_G_hi)))(M_G_hi), G_hi'events, theta ) THEN
   code := 4; PRINT("The causal map is an observer.") END
END

/*
OUTPUT:
Set of Strings: {{},{(1|->e1)},{(1|->e1),(2|->e3)},{(1|->e2)},{(1|->e2),(2|->e4)}}
Set of tasks: {{(1|->e1),(2|->e3)},{(1|->e2),(2|->e4)}}
Prefixes of tasks: {{},{(1|->e1)},{(1|->e1),(2|->e3)},{(1|->e2)},{(1|->e2),(2|->e4)}}

Set of Strings: {{},{(1|->t0)},{(1|->t0),(2|->t1)},{(1|->t0),(2|->t2)}}
Set of tasks: {{(1|->t0),(2|->t1)},{(1|->t0),(2|->t2)}}
Prefixes of tasks: {{},{(1|->t0)},{(1|->t0),(2|->t1)},{(1|->t0),(2|->t2)}}

Causal map: {({}|->{}),({(1|->e1)}|->{(1|->t0)}),({(1|->e1),
             (2|->e3)}|->{(1|->t0),(2|->t1)}),({(1|->e2)}|->{(1|->t0)}),
             ({(1|->e2),(2|->e4)}|->{(1|->t0),(2|->t2)})}
cover(a_pair_of_languages)
"Consistency of marking."
cover(Check_ConsistencyMarking)
"Control coincidence."
cover(Check_ControlCoincidence)


**** The observer property is not satisfied.
*/