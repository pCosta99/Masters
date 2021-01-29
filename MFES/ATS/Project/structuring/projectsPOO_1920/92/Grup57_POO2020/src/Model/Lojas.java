package Model;

import java.util.*;
import java.io.Serializable;


public class Lojas implements ILojas, Serializable {

	private Map<String, Loja> lojas;

	public Lojas() {
		this.lojas = new HashMap<>();
	}

	public Lojas(Map<String, Loja> l) {
		setLojas(l);
	}

	public Lojas(Lojas lt) {
		setLojas(lt.getLojas());
	}

	public Map<String, Loja> getLojas() {
		Map<String, Loja> aux = new HashMap<>();
		for(Map.Entry<String, Loja> t : this.lojas.entrySet())
			aux.put(t.getKey(), t.getValue().clone());
		return aux;
	}

	public void setLojas(Map<String, Loja> l) {
		this.lojas = new HashMap<>();
		l.entrySet().forEach(s -> this.lojas.put(s.getKey(), s.getValue().clone()));
	}

	public Lojas clone() {
		return new Lojas(this);
	}

	public boolean equals(Object o) {
		if(o == this) return true;
		if(o == null || o.getClass() != this.getClass()) return false;
		Lojas lt = (Lojas) o;
		return this.lojas.equals(lt.getLojas());
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Lojas: " + this.lojas + "\n");

		return sb.toString();
	}

	@Override
	public void put(Loja l) {
		this.lojas.put(l.getCod(), l);
	}

	@Override
	public void put(String cod, Loja l) {
		this.lojas.put(cod,l);
	}

	@Override
	public String extractNameByUserName(String userName) {
		return this.lojas.get(userName).getNome();
	}

	@Override
	public boolean checkExisteLoja(String codLoja) {
		return this.lojas.containsKey(codLoja);
	}

	@Override
	public void adicionaEncomendaFilaDeEspera (String codLoja, Encomenda e) {
		this.lojas.get(codLoja).adicionaEncomendaFilaDeEspera(e);
	}

	@Override
	public Loja extraiLoja(String codLoja) {
		return this.lojas.get(codLoja);
	}

	@Override
	public List<String> exportKeyLojas() {
		ArrayList<String> result = new ArrayList<>();
		this.lojas.entrySet().forEach(s -> result.add(s.getKey()));
		return result;
	}

	@Override
	public List<Integer> encomendaPronta(String codLoja) {
		int encomendasNaFila = 0;
		int tempoMedioEncomendas = 0;
		int resultado = 0;
		List<Integer> listaFinal = new ArrayList<>();
		Random generator = new Random();
		encomendasNaFila = (generator.nextInt(20) + 1);
		tempoMedioEncomendas = (generator.nextInt(5) + 1);
		resultado = (encomendasNaFila * tempoMedioEncomendas);
		listaFinal.add(encomendasNaFila);
		listaFinal.add(tempoMedioEncomendas);
		listaFinal.add(resultado);
		return listaFinal;
	}

	@Override
	public void changeAceitaFila (String loja, boolean aceita) {
		this.lojas.get(loja).setAceitaFila(aceita);
	}
}