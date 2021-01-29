// Algoritmo para formar uma spanning tree numa rede

sig Node {
	adj : set Node,				// Conjunto de nós vizinhos
	var rcvd : set Node,			// Nós dos quais já processou mensagens
	var parent : lone Node,		// O eventual pai do nó na spanning tree
	var children : set Node,		// Os eventuais filhos do nó na spanning tree
	var inbox : set Message		// Mensagens na inbox (nunca são apagadas)
}
one sig initiator extends Node {}	// O nó que inicia o protocolo

// Tipos de mensagens
abstract sig Type {}
one sig Ping, Echo extends Type {}

// Mensagens enviadas
sig Message {
	from : one Node,			// Nó que enviou a mensagem
	type : one Type			// Tipo da mensagem
}

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
	one Message
	Message.type = Ping
	Message.from = initiator
	all n : initiator.adj | Message in n.inbox
}


// Um finish pode ocorrer quando um nó está ready, enviando esse nó 
// uma mensagem do tipo Echo ao seu parent.
pred finish [n : Node] {
	n in ready
	one m : Message' | {
		m not in Message
		from' = from + (m->n.parent)
		type' = type + (m->Echo)
		inbox' = inbox + (n.parent -> m)
	}
	// unchanged
	rcvd' = rcvd
	parent' = parent
	children' = children
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
	#n.inbox - #n.rcvd > 0 // existe uma mensagem não processada
	no n & initiator and no n.rcvd => {
		let m = n.inbox | n.parent' = m.from
		// Envia ping
		one m2 : Message' | {
			m2 not in Message
			from' = from + (m2->n)
			type' = type + (m2->Ping)
			all n : n.adj - n.parent' |  n.inbox' = n.inbox + m2
			all n : Node - (n.adj - n.parent') | n.inbox' = n.inbox // assert no change
		}
	} else {
		// Foi um echo
		children' = children + (n->n.inbox.from)
		parent' = parent
		rcvd' = rcvd
		inbox' = inbox
	}
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

pred spanning_tree[n : Node]{
	// Só há um caminho para cada folha
	// Children tem de ser aciclico e conetado e todos os nodos estão ao longo dos filhos de children
	Node - n in n.^children 
	no ^children & iden
	all disj x,y : Node | x in y.^children
}

assert SpanningTree {
	// Quando todos os nós estão ready a relação children forma uma 
	// spanning tree com raiz no initiator.
	always (Node in ready implies always spanning_tree[initiator])
}
/*
// A mesa é redonda, ou seja, as coisas formam um anel
	all c : Coisa | Coisa in c.^prox
	// Os garfos e os filósofos estão intercalados
	all c : Coisa | c in Filosofo iff c.prox in Garfo
*/
run {
	all n : Node |  #n.adj = 2
} for exactly 5 Node, 10 Message
