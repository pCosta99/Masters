sig Track {
  prox : lone Track,
  signal : lone Signal
}

sig Junction extends Track {}

sig Signal {}

fact {
  // Lines are acyclic
  no t : Track | t in t.^prox
}

pred inv1 {
  // Each signal belongs to one and only one track
	//all s : Signal | one signal.s
  	let s = signal | s.~s in iden && iden & Signal->Signal in ~s.s
}



pred inv2 {
  // Only junctions can have more than one predecessor track
	let x = prox :> (Track-Junction) | x.~x in iden
}



pred inv3 {
  // All tracks that meet at a junction must have a signal
	prox.Junction in signal.Signal
}


