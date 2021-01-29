/*
A labeled transition system (LTS) is comprised by States, a sub-set
of which are Initial, connected by transitions, here represented by
Events.
*/
sig State {
        trans : Event -> State
}
sig Init in State {}
sig Event {}

/*
The LTS does not contain deadlocks, ie, each state has at least a
transition.
*/
pred inv1 {
	all s : State | some Event.(s.trans)
}

/*
There is a single initial state.
*/
pred inv2 {
	one Init
}

/*
The LTS is deterministic, ie, each state has at most a transition for each event.
*/
pred inv3 {
	all s : State, e : Event | lone e.(s.trans)
}

/*
All states are reachable from an initial state.
*/
pred inv4 {
	//all s : State, i : Init | (i.trans) (trans.s)
  	//trans.trans
  	let adj = {x,y : State | some e : Event | x->e->y in trans} |
  	all disj i : Init, s : State | i->s in ^adj // dá-nos o fecho transitivo
}

/*
All the states have the same events available.
*/
pred inv5 {
	all s, s1 : State | (s.trans).State = (s1.trans).State
}

/*
Each event is available in at least a state.
*/
pred inv6 {
	//all e : Event | some s : State | e->s in State.trans
  	Event in State.trans.State
}

/*
The LTS is reversible, ie, from a reacheable state it is always possible
to return to an initial state.
*/
pred inv7 {
	//all n : State-Init | Init in ^(State.trans).n
}

