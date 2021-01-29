package Model;

import java.io.Serializable;


public class Aceite implements Serializable {

	private String codEncomenda;

	public Aceite() {
		this.codEncomenda = new String();
	}

	public Aceite(String codE) {
		setCodE(codE);
	}

	public Aceite(Aceite a) {
		setCodE(a.getCodE());
	}

	public String getCodE() {
		return this.codEncomenda;
	}

	public void setCodE(String codE) {
		this.codEncomenda = codE;
	}

	public Aceite clone() {
		return new Aceite(this);
	}

	public boolean equals(Object o) {
		if (o == this) return true;
		if (o == null || o.getClass() != this.getClass()) return false;
		Aceite a = (Aceite) o;
		return this.codEncomenda.equals(a.getCodE());
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Encomenda Aceite: " + this.codEncomenda + "\n");

		return sb.toString();
	}
}