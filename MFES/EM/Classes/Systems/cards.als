// Modelo abstracto de um sistema de emissão de cartões bancários

abstract sig Status {}
one sig Unissued, Issued, Cancelled extends Status {}

sig Card {
	var status : one Status
}

sig Client {
	var cards : set Card
}

// Functions to acess each set of different status cards
fun unissuedCards : set Card {
	{ c : Card | c.status = Unissued }
}

fun issuedCards : set Card {
	{ c : Card | c.status = Issued }
}

fun cancelledCards : set Card {
	{ c : Card | c.status = Cancelled }
}

// Algumas das propriedades desejadas para o sistema

assert NoUnissuedCards {
  	always Unissued not in Client.cards.status
}

assert NoSharedCards {
	always all disj x, y : Client, c : x.cards | always (c not in y.cards)
}

assert AllCancelled {
	all c : Card | always (c.status = Issued implies eventually c.status = Cancelled)
}

// Especifique as condições iniciais do sistema

fact Init {
	Card.status in Unissued
	no cards
}

// Especifique as operações do sistema por forma a garantir as propriedades
// de segurança

check NoUnissuedCards
check NoSharedCards

// Operação de emitir um cartão para um cliente
pred emit [c : Card, a : Client] {
	// o estado passa de unissued para issued
	c.status' = Issued
	c.status = Unissued
	// o cartao e associado ao cliente
	cards' = cards + a->c
	// tudo o resto se mantem
	status' - c->Status = status - c->Status
	cards' - a->c = cards
}

// Operação de cancelar um cartão
pred cancel [c : Card] {
	// o estado passa de issued para cancelled
	c.status = Issued
	c.status' = Cancelled
	// o cartao fica desassociado ao cliente
	cards' = cards - Client->c
	// tudo o resto de mantem
	status' - c->Status = status - c->Status
}

pred nop {
	status' = status
	cards' = cards
}

fact Traces {
	always (nop or some c : Card | cancel[c] or some a : Client | emit[c,a])
}

// Especifique um cenário onde 3 cartões são emitidos a pelo menos 2
// clientes e são todos inevitavelmente cancelados, usando os scopes
// para controlar a cardinalidade das assinaturas
// Tente também definir um theme onde os cartões emitidos são verdes
// e os cancelados são vermelhos, ocultando depois toda a informação que
// seja redundante 
// Pode introduzir definições auxiliares no modelo se necessário

run Exemplo {
	
} for exactly 2 Client, exactly 3 Card

// Especifique as condições de justiça estritamente necessárias para garantir a
// propriedade de animação

pred emmitedEnabled {
	some Client
	Unissued in Card.status
}

pred cancelEnabled {
	some Client
	Issued in Card.status
}

fact FairnessAndLiveness {
	always (cancelEnabled => eventually not cancelEnabled)
	always (emmitedEnabled => eventually not emmitedEnabled)
}

check AllCancelled
