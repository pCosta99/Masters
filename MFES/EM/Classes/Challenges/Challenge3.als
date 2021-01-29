sig Artigo {
	oferta : Pessoa -> Valor
}
sig Valor {}
sig Pessoa {
	leiloes : set Artigo
}

pred inv1 {
	// O mesmo artigo não pode ser leiloado por duas pessoas diferentes
  	//all a : Artigo, disj p1, p2 : Pessoa | a in p1.leiloes => a not in p2.leiloes
	//leiloes.~leiloes in iden
	//all a : Artigo | lone leiloes.a
  	all a : Artigo | lone a->Pessoa & ~leiloes
}


pred inv2 {
	// Uma pessoa não pode fazer ofertas em artigos que está a leiloar
	//all a : Artigo, p : Pessoa | p->a in leiloes => no p.(a.oferta)
	//no (oferta.Valor & ~leiloes) // esta sim, caputa
  	all p : Pessoa | no p.leiloes & (oferta.Valor).p
}


pred inv3 {
	// Não pode haver duas ofertas de igual valor para o mesmo artigo
	//all a : Artigo, v1, v2 : Valor | v1 in Pessoa.(a.oferta) and v2 in Pessoa.(a.oferta) => v1 not = v2
  	//all a : Artigo, disj v1, v2 : Pessoa.(a.oferta) | v1 != v2
  	//all a : Artigo, v1, v2 : Valor | v1 = v2 => v1 + v2 not in Pessoa.(a.oferta)
  	//all a : Artigo, v : Valor, disj p1, p2 : Pessoa | v in (p1.(a.oferta)) => v not in (p2.(a.oferta))
  	all a : Artigo, v : Valor | lone Pessoa->v & a.oferta
}

