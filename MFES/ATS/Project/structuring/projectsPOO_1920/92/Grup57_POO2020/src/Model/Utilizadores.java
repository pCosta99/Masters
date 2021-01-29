package Model;

import java.util.*;
import java.io.Serializable;

public class Utilizadores implements IUtilizadores, Serializable {

	private Map<String,Utilizador> utilizadores;

	public Utilizadores() {
		this.utilizadores = new HashMap<>();
	}

	public Utilizadores(Map<String, Utilizador> u) {
		setUtilizadores(u);
	}

	public Utilizadores(Utilizadores ut) {
		setUtilizadores(ut.getUtilizadores());
	}

	public Map<String, Utilizador> getUtilizadores() {
		Map<String, Utilizador> aux = new HashMap<>();
		for(Map.Entry<String, Utilizador> u : this.utilizadores.entrySet())
			aux.put(u.getKey(), u.getValue().clone());
		return aux;
	}

	public void setUtilizadores(Map<String, Utilizador> u) {
		this.utilizadores = new HashMap<>();
		u.entrySet().forEach(s -> this.utilizadores.put(s.getKey(), s.getValue().clone()));
	}

	public Utilizadores clone() {
		return new Utilizadores(this);
	}

	public boolean equals(Object o) {
		if(o == this) return true;
		if(o == null || o.getClass() != this.getClass()) return false;
		Utilizadores ut = (Utilizadores) o;
		return this.utilizadores.equals(ut.getUtilizadores());
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Utilizadores: " + this.utilizadores + "\n");

		return sb.toString();
	}

	@Override
	public void put(Utilizador u) {
		this.utilizadores.put(u.getCod(), u);
	}

	@Override
	public void put(String cod, Utilizador u) {
		this.utilizadores.put(cod,u);
	}

	@Override
	public String extractNameByUserName (String userName) {
		return this.utilizadores.get(userName).getNome();
	}

	@Override
	public Double extractXByUserName(String userName) {
		return this.utilizadores.get(userName).getCoordenadas().getX();
	}

	@Override
	public Double extractYByUserName(String userName) {
		return this.utilizadores.get(userName).getCoordenadas().getY();
	}

	@Override
	public String extractEmailByUserName (String userName) {
		return this.utilizadores.get(userName).getEmail();
	}

	@Override
	public void changeName(String userName, String nome) {
		this.utilizadores.get(userName).setNome(nome);
	}

	@Override
	public void changeGPS(String userName, Double x, Double y) {
		GPS g = new GPS();
		g.setX(x);
		g.setY(y);
		this.utilizadores.get(userName).setCoordenadas(g);
	}

	@Override
	public void changeEmail(String userName, String email) {
		this.utilizadores.get(userName).setEmail(email);
	}

	@Override
	public List<String> exportUsers() {
		List<String> l = new ArrayList<>();
		for (String key : this.utilizadores.keySet()) {
			l.add(key);
		}
		return l;
	}
}