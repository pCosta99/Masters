// Model of a train station

sig Track {
	prox : set Track,
	signal : lone Signal
}
sig Junction extends Track {}
sig Entry, Exit in Track {}

sig Signal {}
var sig Green in Signal {}

sig Train {
	var pos : lone Track
}

fact Layout {
	// A track is a junction iff it has more than one successor or more than one predecessor
	all t : Track | t not in Junction iff (lone t.prox and lone prox.t)
	// No cycles
	no t : Track | t in t.^prox
	// Signals belong to one and only one track
	all s : Signal | one signal.s
	// All tracks before junctions have signals
	all j : Junction, t : prox.j | some t.signal
	// Entry tracks are those without predecessors and exit tracks are those without no successors
	all t : Track | t in Entry iff no prox.t
	all t : Track | t in Exit iff no t.prox
}

// The fundamental desired safety property

assert NoCollisions {
	always (all t : Track | lone pos.t)
}

// Specify the initial conditions of the system (no trains in the station
// and all signals red)
fact Init {
	no Green
}

// Specify the dynamics of the system, ensuring the desired safety properties

// Enter station
// Should be possible whenever an entry track is empty
pred enter [t : Train] {
	some e : Entry | no pos.e implies pos' = pos + t->e or pos' = pos
	// Rest stays equal
	Green' = Green	
	Track' = Track
	Train' = Train
}

// Move to next track or exit station
// Should be possible if there are no signals on the current track
// or the signal is green
// Signal must become red afterwards
pred move [t : Train] {
	t.pos.signal = Green or no t.pos.signal
	pos' = pos ++ t->(t.pos.prox)
	t'.pos.signal != Green or no t'.pos.signal
}

// Switch from red to green
// When should it be possible?!
pred on [s : Signal] {
	pos' = pos
	// A signal can turn to green if there is no risk of collision. 
	//That simply means that there must not be any other train able to pass there at the same moment (either because it doesn't exist or because it's track signal is red).
	let curr = signal.s | (prox.(curr.prox) - curr).signal != Green implies Green' = Green + s
}

pred nop {
	pos' = pos
	Green' = Green
}

fact Traces {
	//always (nop or (some t : Train | enter[t] or move[t]) or (some s : Signal | on[s]))
	always (nop or some s : Signal | on[s])
}

// Do we need extra facts about the train layout to ensure safety?!
fact Extra {

}

check NoCollisions


// Specify a scenario where the station has at least one Junction, all exits are
// acessible from all entries, and two trains keep entering and exiting the station.
// Parametrize the theme to show tracks with a red signal as red and
// tracks with a green signal as green, identify moving trains with a different
// shape, and hide all redundant information.

run Example {

}