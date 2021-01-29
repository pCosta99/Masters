package Model;

import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.io.Serializable;

public class Encomendas implements IEncomendas, Serializable {

	private Map<String, Encomenda> encomendas;

	public Encomendas() {
		this.encomendas = new HashMap<>();
	}

	public Encomendas(Map<String, Encomenda> e) {
		setEncomendas(e);
	}

	public Encomendas(Encomendas et) {
		setEncomendas(et.getEncomendas());
	}

	public Map<String, Encomenda> getEncomendas() {
		Map<String, Encomenda> aux = new HashMap<>();
		for(Map.Entry<String, Encomenda> u : this.encomendas.entrySet())
			aux.put(u.getKey(), u.getValue().clone());
		return aux; 
	}

	public void setEncomendas(Map<String, Encomenda> e) {
		this.encomendas = new HashMap<>();
		e.entrySet().forEach(s -> this.encomendas.put(s.getKey(), s.getValue().clone()));
	}

	public Encomendas clone() {
		return new Encomendas(this);
	}

	public boolean equals(Object o) {
		if (o == this) return true;
		if (o == null || o.getClass() != this.getClass()) return false;
		Encomendas e = (Encomendas) o;
		return this.encomendas.equals(e.getEncomendas());
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Encomendas: " + this.encomendas + "\n");

		return sb.toString();
	}

	@Override
	public void put(String cod, Encomenda e) {
		this.encomendas.put(cod, e);
	}

	@Override
	public void criaEncomenda(String codEncomenda, String userName, String codLoja, double peso) {
		ArrayList<LinhaEncomenda> l = new ArrayList<>();
		Encomenda e = new Encomenda(codEncomenda,userName,codLoja,peso,l);
		this.encomendas.put(codEncomenda,e);
	}

	@Override
	public Encomenda extraiEncomenda(String codEncomenda) {
		return this.encomendas.get(codEncomenda);
	}

	@Override
	public void adicionaProdutos(String codEncomenda, String codProduto, String descricao, double quantidade, double valorUnitario) {
		LinhaEncomenda l = new LinhaEncomenda(codProduto,descricao,quantidade,valorUnitario);
		this.encomendas.get(codEncomenda).adicionaLinhaEncomenda(l);
	}
}