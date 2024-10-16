// Richard St-Denis, Universite de Sherbrooke, 2024.
MODEL section5_4_find_ex5_Mcontrollable

SETS
 States; Events; Y

DEFINITIONS
 //---------------------------------------------------------------------
 // ProB settings
 //---------------------------------------------------------------------
 SET_PREF_MAX_INITIALISATIONS == 1;
 SET_PREF_MAX_OPERATIONS == 1;
 SET_PREF_TIME_OUT == 50000000;
 SET_PREF_SMT == FALSE;
 SET_PREF_KODKOD == FALSE;
 //---------------------------------------------------------------------
 // Import the theory
 //---------------------------------------------------------------------
 "../Theories/SBCDES.def";
 "LibraryIO.def"

PROPERTIES
 card(States) = 4 &
 card(Events) = 2 &
 card(Y) = 1

OPERATIONS
 findExample = ANY G_, Q_, m_ WHERE
  G_ : Generator_C(States,Events) &
  card(G_'states) = card(States) & card(G_'events) = card(Events) & card(G_'tf) : 4..4 &
  card(G_'Xm) = card(G_'states) & card(G_'ctrlEvents) = card(G_'events)  &

  !(e_).(e_ : G_'events => card(dom(G_'tf)|>{e_}) : 1..2) &              // there is a transition on every event, but no more than two
  card({G_'x_0}<|dom(G_'tf)) = 2 &                                       // two transitions from the initial state
  !(x_).(x_ : G_'states => card({x_}<|dom(G_'tf)) : 0..2) &              // no more then two transitions from a state
  card(G_'tf|>{G_'x_0}) = 0 &                                            // no transition to the initial state
  !(x_).(x_ : G_'states & x_ /= G_'x_0 => card(G_'tf|>{x_}) : 1..2) &    // at least one transition to a state, but no more then two
  !t_.(t_ : G_'tf => dom(dom({t_})) /= ran({t_})) &                      // no selfloop
  Reachable(G_,G_'states) &                                              // all states are reachable

  Q_ <: G_'states & card(Q_) = 2 &

  m_ : G_'states -->> Y &           // A total surjective function
  Mcontrollable(G_,m_,Q_) & not(StrongMcontrollable(G_,m_,Q_)) 
 THEN PRINT(G_); PRINT(Q_); PRINT(m_); PRINT(SFBC_breve(G_,m_,Q_)) END END

/*
OUTPUT:
rec(Xm:{States1,States2,States3,States4},
    ctrlEvents:{Events1,Events2},
    events:{Events1,Events2},
    states:{States1,States2,States3,States4},
    tf:{((States1|->Events2)|->States3),((States2|->Events1)|->States1),
        ((States2|->Events2)|->States1),((States3|->Events1)|->States4)},
    x_0:States2)
{States1,States2}
{(States1|->Y1),(States2|->Y1),(States3|->Y1),(States4|->Y1)}
{(States1|->{Events2}),(States2|->{Events2})}
Infinite loop reached after 4 steps (looping on findExample).
% Runtime: 627430 ms (total: 627430 ms, walltime: 629430 ms)
% Finished execute (errors=0, warnings=0)
% Finished processing file after 629480 ms walltime (627480 ms runtime), since start: 630980 ms
*/