package Model;

import java.util.*;
import java.io.Serializable;


public class Transportadoras implements ITransportadoras, Serializable {

	private Map<String, Transportadora> transportadoras;

	public Transportadoras() {
		this.transportadoras = new HashMap<>();
	}

	public Transportadoras(Map<String, Transportadora> t) {
		setTransportadoras(t);
	}

	public Transportadoras(Transportadoras tt) {
		setTransportadoras(tt.getTransportadoras());
	}

	public Map<String, Transportadora> getTransportadoras() {
		Map<String, Transportadora> aux = new HashMap<>();
		for(Map.Entry<String, Transportadora> t : this.transportadoras.entrySet())
			aux.put(t.getKey(), t.getValue().clone());
		return aux;
	}

	public void setTransportadoras(Map<String, Transportadora> t) {
		this.transportadoras = new HashMap<>();
		t.entrySet().forEach(s -> this.transportadoras.put(s.getKey(), s.getValue().clone()));
	}

	public Transportadoras clone() {
		return new Transportadoras(this);
	}

	public boolean equals(Object o) {
		if(o == this) return true;
		if(o == null || o.getClass() != this.getClass()) return false;
		Transportadoras tt = (Transportadoras) o;
		return this.transportadoras.equals(tt.getTransportadoras());
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Transportadoras: " + this.transportadoras + "\n");

		return sb.toString();
	}

	@Override
	public void put(Transportadora t) {
		this.transportadoras.put(t.getCod(), t);
	}

	@Override
	public void put(String cod, Transportadora t) {
		this.transportadoras.put(cod,t);
	}

	@Override
	public String extractNameByUserName(String userName) {
		return this.transportadoras.get(userName).getNome();
	}

	@Override
	public List<String> extractTransportadoras() {
		List<String> l = new ArrayList<>();
		for (String key : this.transportadoras.keySet()) {
			l.add(key);
		}
		return l;
	}

	@Override
	public Transportadora extractTransportadora(String codTransportadora) {
		return this.transportadoras.get(codTransportadora);
	}

	@Override
	public boolean checkExisteTransportadora(String codTransportadora) {
		return this.transportadoras.containsKey(codTransportadora);
	}

	@Override
	public List<String> exportTransportadorasDentroRaio(double xUtilizador, double yUtilizador) {
		List<String> listTransp = new ArrayList<>();
		for (Map.Entry<String,Transportadora> t : this.transportadoras.entrySet()) {
			if (Auxiliar.calculadoraDistancia(xUtilizador,yUtilizador,(t.getValue().getCoordenadas().getX()),
					(t.getValue().getCoordenadas().getY())) < t.getValue().getRaio()) {
				listTransp.add(t.getKey());
			}
		}
		return listTransp;
	}

	@Override
	public double extractCustoKM(String transportadora) {
		return this.transportadoras.get(transportadora).getPreco();
	}

	@Override
	public void addClassificacao (String transportadoras, int classificacao) {
		this.transportadoras.get(transportadoras).addClassificacao(classificacao);
	}

	@Override
	public double exportClassMediaTransportadora (String transportadora) {
		return this.transportadoras.get(transportadora).mediaClassificacao();
	}

	@Override
	public void changeAceita (String transportadora, boolean aceita) {
		this.transportadoras.get(transportadora).setAceita(aceita);
	}
}