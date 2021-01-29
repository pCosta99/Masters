// Algoritmo para formar uma spanning tree numa rede

sig Node {
	adj : set Node,				// Conjunto de nós vizinhos
	var rcvd : set Node,			// Nós dos quais já processou mensagens
	var parent : lone Node,		// O eventual pai do nó na spanning tree
	var children : set Node,		// Os eventuais filhos do nó na spanning tree
	var inbox : Node -> Type 	// Mensagens na inbox (nunca são apagadas)
}
one sig initiator extends Node {}	// O nó que inicia o protocolo

// Tipos de mensagens
abstract sig Type {}
one sig Ping, Echo extends Type {}

// Um nó considera-se ready quando já leu e processou mensagens de todos os seus vizinhos. 
// A execução do protocolo termina quando todos os nós estão ready.
fun ready : set Node {
	{ n : Node | n.adj in n.rcvd }
}

// O grafo definido pela relação adj não tem lacetes.
fact SemLacetes {
	all n : Node | n not in n.adj
}

// O grafo definido pela relação adj é não orientado.
fact NaoOrientado {
	adj = ~adj
}

// O grafo definido pela relação adj é ligado.
fact Ligado {
	all disj x,y : Node | x in y.^adj
}

// Inicialmente rcvd, parent e children estão vazias 
// e o initiator envia um Ping para todos os vizinhos

fact init {
	no rcvd
	no parent
	no children
	all n : initiator.adj | 	initiator->Ping in n.inbox
}


// Um finish pode ocorrer quando um nó está ready, enviando esse nó 
// uma mensagem do tipo Echo ao seu parent.
pred finish [n : Node] {
	/*n in ready
	one m : Message' | {
		m not in Message
		from' = from + (m->n.parent)
		type' = type + (m->Echo)
		inbox' = inbox + (n.parent -> m)
	}
	// unchanged
	rcvd' = rcvd
	parent' = parent
	children' = children*/
}

// Um read pode ocorrer quando um nó tem uma mensagem 
// ainda não processada na sua inbox. Se o nó não é o initiator
// e é a primeira mensagem que processa (necessariamente um Ping) 
// então o nó que enviou a mensagem passa a ser o seu parent
// na spanning tree e é enviado um Ping a todos os 
// restantes vizinhos (todos menos o novo parent). 
// Se a mensagem recebida é um Echo então o nó que enviou 
// a mensagem é adicionado ao conjunto dos seus children na spanning tree.

pred read [n : Node] {
	/*
	#n.inbox - #n.rcvd > 0 // existe uma mensagem não processada
	no n & initiator and no n.rcvd => {
		let m = n.inbox | {
			n.parent' = m.from
			children' = children + (m.from -> n)
		}
		one m2 : Message' | {
			m2 not in Message
			from' = from + (m2->n)
			type' = type + (m2->Ping)
			all n : n.adj - n.parent' |  n.inbox' = n.inbox + m2
			all n : Node - (n.adj - n.parent') | n.inbox' = n.inbox // assert no change
		}
	} else {
		// Foi um echo
		
	}*/
}

pred stutter {
	rcvd' = rcvd
	parent' = parent
	children' = children
	inbox' = inbox
}

fact transitions {
	always (stutter or some n : Node | read[n] or finish[n])
}

assert Invariante {
	// O initiatior nunca tem pai.
	no initiator.parent
	// O pai tem sempre que ser um dos vizinhos.
	all n : Node | n.parent in n.adj
	// Um nó só pode ser filho do seu pai.
	all n : Node, p : n.parent | p->n in children and one children.n
	// Um nó que esteja ready tem como filhos todos os nós dos quais é pai.
	all n : Node | n in ready => n.children = parent.n
}

assert SpanningTree {
	// Quando todos os nós estão ready a relação children forma uma 
	// spanning tree com raiz no initiator.

}

run {
} for exactly 3 Node
