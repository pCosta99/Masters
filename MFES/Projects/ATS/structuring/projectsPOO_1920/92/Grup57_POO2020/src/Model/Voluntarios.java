package Model;

import java.util.*;
import java.io.Serializable;

public class Voluntarios implements IVoluntarios, Serializable {

	private Map<String,Voluntario> voluntarios;

	public Voluntarios() {
		this.voluntarios = new HashMap<>();
	}

	public Voluntarios(Map<String, Voluntario> v) {
		setVoluntarios(v);
	}

	public Voluntarios(Voluntarios vt) {
		setVoluntarios(vt.getVoluntarios());
	}

	public Map<String, Voluntario> getVoluntarios() {
		Map<String, Voluntario> aux = new HashMap<>();
		for (Map.Entry<String, Voluntario> v : this.voluntarios.entrySet())
			aux.put(v.getKey(), v.getValue().clone());
		return aux;
	}

	public void setVoluntarios(Map<String, Voluntario> v) {
		this.voluntarios = new HashMap<>();
		v.entrySet().forEach(s -> this.voluntarios.put(s.getKey(), s.getValue().clone()));
	}

	public Voluntarios clone() {
		return new Voluntarios(this);
	}

	public boolean equals(Object o) {
		if (o == this) return true;
		if (o == null || o.getClass() != this.getClass()) return false;
		Voluntarios vt = (Voluntarios) o;
		return this.voluntarios.equals(vt.getVoluntarios());
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Voluntários: " + this.voluntarios + "\n");

		return sb.toString();
	}

	@Override
	public void put(Voluntario v) {
		this.voluntarios.put(v.getCod(), v);
	}

	@Override
	public void put(String cod, Voluntario v) {
		this.voluntarios.put(cod,v);
	}

	@Override
	public String extractNameByUserName(String userName) {
		return this.voluntarios.get(userName).getNome();
	}

	@Override
	public List<String> extractVoluntarios() {
		List<String> l = new ArrayList<>();
		for (String key : this.voluntarios.keySet()) {
			l.add(key);
		}
		return l;
	}

	@Override
	public Voluntario extractVoluntario(String voluntario) {
		return this.voluntarios.get(voluntario);
	}

	@Override
	public boolean checkExisteVoluntario(String codVoluntario) {
		return this.voluntarios.containsKey(codVoluntario);
	}

	@Override
	public List<String> exportVoluntariosDentroRaio(double xUtilizador, double yUtilizador) {
		List<String> listVolun = new ArrayList<>();
		for (Map.Entry<String,Voluntario> v : this.voluntarios.entrySet()) {
			if (Auxiliar.calculadoraDistancia(xUtilizador,yUtilizador,(v.getValue().getCoordenadas().getX()),
					(v.getValue().getCoordenadas().getY())) < v.getValue().getRaio()) {
				listVolun.add(v.getKey());
			}
		}
		return listVolun;
	}

	@Override
	public void addClassificacao (String voluntario, int classificacao) {
		this.voluntarios.get(voluntario).addClassificacao(classificacao);
	}

	@Override
	public double exportClassMediaVoluntario (String voluntario) {
		return this.voluntarios.get(voluntario).mediaClassificacao();
	}

	@Override
	public void changeAceita (String voluntario, boolean aceita) {
		this.voluntarios.get(voluntario).setAceita(aceita);
	}

}