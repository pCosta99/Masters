package Model;

import Model.GPS;
import java.io.Serializable;

public abstract class Account implements Serializable {

	private String cod;
	private String nome;
	private GPS coordenadas;
	private String email;


	public Account() {
		this.cod = new String();
		this.nome = new String();
		this.coordenadas = new GPS();
		this.email = new String();
	}


	public Account(String c, String n, GPS gps, String e) {
		setCod(c);
		setNome(n);
		setCoordenadas(gps);
		setEmail(e);
	}


	public Account(Account a) {
		setCod(a.getCod());
		setNome(a.getNome());
		setCoordenadas(a.getCoordenadas());
		setEmail(a.getEmail());
	}


	public abstract Account clone();


	public String getCod() {
		return this.cod;
	}


	public String getNome() {
		return this.nome;
	}


	public GPS getCoordenadas() {
		return this.coordenadas;
	}


	public String getEmail() {
		return this.email;
	}


	public void setCod(String cod) {
		this.cod = cod;
	}


	public void setNome(String nome) {
		this.nome = nome;
	}


	public void setCoordenadas(GPS coordenadas) {
		this.coordenadas = coordenadas;
	}


	public void setEmail(String email) {
		this.email = email;
	}

	public boolean equals(Object o) {
		if(o == this) return true;
		if(o == null || o.getClass() != this.getClass()) return false;
		Account a = (Account) o;

		return this.cod.equals(a.getCod()) &&
				this.nome.equals(a.getNome()) &&
				this.coordenadas.equals(a.getCoordenadas());
	}


	public String toString() {
		StringBuilder sb = new StringBuilder();

		sb.append(this.cod).append(' ')
				.append(this.nome).append(' ')
				.append(this.coordenadas).append(' ')
				.append(this.email).append(' ');

		return sb.toString();
	}


}