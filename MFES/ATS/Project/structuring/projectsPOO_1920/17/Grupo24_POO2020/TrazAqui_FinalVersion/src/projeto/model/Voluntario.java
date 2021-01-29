package projeto.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Random;

public class Voluntario extends EntidadeTransportadora implements Entregador, Serializable {
	
	//private boolean aceitaMedicamentos;

	// CONSTRUTORES
	public Voluntario() {
		super();
	}
	
	public Voluntario(Voluntario v) {
		super(v);
			
	}
	
	public Voluntario(String codigo, String email, String password, String nome, double coordX, double coordY) {
		super(codigo, email, password, nome, coordX, coordY);
	}
	
	public Voluntario(String codigo, String nome, double coordX, double coordY, double raio) {
		super(codigo, nome, coordX, coordY);
		ArrayList<String> nova = new ArrayList<String>();
		nova.add("->");
		this.setEntregasEfetuadas(nova);
	}
	
	public Voluntario(String nome, String email, String password) {
		super(nome,email,password);
		//this.setRaio(Gerador.doubleGenerator());
		ArrayList<String> nova = new ArrayList<String>();
		nova.add("->");
		this.setEntregasEfetuadas(nova);
	}
	
	// CLONE, TOSTRING, EQUALS
	
	public Voluntario clone() {
		return new Voluntario(this);
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder("Voluntario: ");
		sb.append("Nome ='").append(this.getNome()).append('\'');
		sb.append(", Email ='").append(this.getEmail()).append('\'');
		sb.append(" Codigo = '").append(this.getCodigo()).append('\'');
		return sb.toString();
	 }
	
	public boolean equals(Object o) {
		if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Voluntario u = (Voluntario) o;
        Voluntario em = (Voluntario) o;
		return (this.getCodigo().equals(u.getCodigo()) && this.getEmail().equals(em.getEmail()));
	}
	
	
	// GETTERS E SETTERS
	
	
	// OUTROS MÉTODOS
	
	public boolean recolher(Utilizador u, Loja loja, Encomenda enc) {
		
		return true;
	}
	

	
	
	
	

	

	

	
	
	

}

