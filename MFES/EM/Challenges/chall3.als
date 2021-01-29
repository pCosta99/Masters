sig Artigo {
	oferta : Pessoa -> Valor
}
sig Valor {}
sig Pessoa {
	leiloes : set Artigo
}

pred inv1 {
	// O mesmo artigo não pode ser leiloado por duas pessoas diferentes
	let l = leiloes | l.~l in iden
}


pred inv2 {
	// Uma pessoa não pode fazer ofertas em artigos que está a leiloar
	no oferta.Valor & ~leiloes
}


pred inv3 {
	// Não pode haver duas ofertas de igual valor para o mesmo artigo
	all a : Artigo | let r = a.oferta | r.~r in iden
}
