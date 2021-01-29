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
  	~signal.signal in iden and iden in ~signal.signal
}

pred inv2 {
  // Only junctions can have more than one predecessor track
  all t : Track - Junction | lone prox.t
}



pred inv3 {
  // All tracks that meet at a junction must have a signal
	all j : Junction, t : prox.j | one t.signal
}


