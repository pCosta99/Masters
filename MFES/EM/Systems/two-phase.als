// Model of a distributed two-phase commit protocol

// Possible internal node states
abstract sig State {}
one sig Working,Committed,Aborted,Prepared extends State {}

sig Message {
	type : one State,
	from : one Node
}

// All previously sent messages
var sig Sent in Message {}

abstract sig Node {
	var state : one State
}
one sig Master extends Node {
	// Set of workers the master already knows to be prepared
	var prepared : set Worker
}
sig Worker extends Node {}

// Some of the desired properties for the protocol

assert MessageTypes {
	// Master can only send messages of type Committed or Aborted
	// Workers can only send messages of type Prepared
	always { 
		all m : Sent | m.from in Master implies m.type in Committed+Aborted else m.type in Prepared
	}
}

assert MasterState {
	// Master can never be in the Prepared state
	always Master.state != Prepared
}

assert Consistency {
	// It is impossible for one node to be in the committed state 
	// and another to be in the aborted state
	always (no state.Committed or no state.Aborted)
}

assert Stability {
	// Once a node has entered the committed or aborted state
	// it remains in that state forever
	all n : Node | always (n.state in Committed+Aborted implies always n.state' = n.state)
}

assert EventuallyAllCommitted {
	// Once one node commits all will eventually commit
	always (some state.Committed implies eventually Node in state.Committed)
}

// Specify the initial conditions of the protocol, namely
// every node is working, there are no sent messages, and
// the master does not know any prepared workers

fact Init {
	no Sent // No sent messages
	no Master.prepared // No prepared workers known by the master
	Node.state = Working // Every node is working
}

// Specify the protocol operations, ensuring all safety properties

check MessageTypes for 5
check MasterState for 5
check Consistency for 5
check Stability for 5

// Worker finishes task and sends prepared message to master
// Errado
pred finish [w : Worker] {
	w.state = Working
	state' = state ++ w->Prepared // state only changes for w
	prepared' = prepared + Master->w // Worker is now in prepared list
	// Message exists and was sent
	some m : Message' | {
		 m not in Message
		m.type = Prepared 
		m.from = w
		Message' = Message + m
	}
}

// Worker aborts spontenously while working
pred spontaneousAbort [w : Worker] {
	w.state = Working
	state' = state ++ w->Aborted // state only changes for w
	Message' = Message // Messages stay the same
	Sent' = Sent // Sent stay the same
}

// Worker commits after receiving the respective message from master 
pred workerCommit [w : Worker] {
	// Message exists and was sent
	some m : Message' | {
		 m not in Message
		m.type = Commited 
		m.from = Master
		Message' = Message + m
	}
}

// Worker aborts after receiving the respective message from master 
pred workerAbort [w : Worker] {

}

// Master receives prepared message from worker
pred receivePrepared {

}

// Master tells everyone to commit when it knows every worker is prepared
pred masterCommit {

}

// Master spontenously decides to tell everyone to abort (maybe due to timeout)
pred masterAbort {

}

pred stutter {
	state' = state
	Sent' = Sent
	prepared' = prepared
}

fact Transitions {
	always {
		//stutter or receivePrepared or masterCommit or masterAbort or some w : Worker {
			//finish[w] or spontaneousAbort[w] or workerCommit[w] or workerAbort[w]
		//}
		stutter or some w : Worker | spontaneousAbort[w] //finish[w]
	}
}

// Specify a scenario where 3 Workers will all be committed
// Parametrize the theme to show only the sent messages
// show all relations of arity one as attributes, hide
// state atoms, and highlight committed nodes

run Example {

} for exactly 3 Worker, 5 Message

// Specify the fairness conditions needed to verify the liveness property

fact Fairness {

}

check EventuallyAllCommitted for 5

