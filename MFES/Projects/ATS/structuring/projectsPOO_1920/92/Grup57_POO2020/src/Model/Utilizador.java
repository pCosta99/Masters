package Model;

public class Utilizador extends Account {

	public Utilizador() {
		super();
	}

	public Utilizador(String codU, String nome, GPS coordenadas, String email) {
		super(codU, nome, coordenadas, email);
	}

	public Utilizador(Utilizador u) {
		super(u);
	}

	public Utilizador clone() {
		return new Utilizador(this);
	}

	public boolean equals(Object o) {
		if(o == this) return true;
		if(o == null || o.getClass() != this.getClass()) return false;
		Utilizador u = (Utilizador) o;
		return super.equals(u);
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Utilizador: " + super.toString()).append('\n');

		return sb.toString();
	}
}