package Model;

import java.util.List;
import java.util.ArrayList;

public class Voluntario extends Account implements Classificacao {

	private double raio;
	private List<Integer> classificacao;
	private boolean aceita;

	public Voluntario() {
		super();
		this.raio = 0.0;
	}

	public Voluntario (String codV, String nomeV, GPS coordenadas,
					   String email, double raio, List<Integer> classificacao, boolean aceita) {
		super(codV, nomeV, coordenadas, email);
		setRaio(raio);
		setClassificacao(classificacao);
		setAceita(aceita);
	}

	public Voluntario(Voluntario v) {
		super(v);
		setRaio(v.getRaio());
		setClassificacao(v.getClassificacao());
		setAceita(v.getAceita());
	}

	public double getRaio() {
		return this.raio;
	}

	public boolean getAceita() {
		return this.aceita;
	}

	public List<Integer> getClassificacao() {
		List<Integer> aux = new ArrayList();
		for (Integer i : this.classificacao)
			aux.add(i);
		return aux;
	}

	public void setRaio(double r) {
		this.raio = r;
	}

	public void setClassificacao(List<Integer> c) {
		this.classificacao = new ArrayList<>();
		classificacao.forEach(aux -> this.classificacao.add(aux));
	}

	public void setAceita(boolean aceita) {
		this.aceita = aceita;
	}

	public Voluntario clone() {
		return new Voluntario(this);
	}

	public boolean equals(Object o) {
		if(o == this) return true;
		if(o == null || o.getClass() != this.getClass()) return false;
		Voluntario v = (Voluntario) o;
		return (super.equals(v) && (this.raio == v.getRaio()) && (this.aceita == v.getAceita()));
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Voluntário: " + super.toString()).append(' ')
				.append(this.raio).append("\n")
				.append(mediaClassificacao()).append("\n")
				.append(this.aceita).append("\n");
		return sb.toString();
	}

	@Override
	public void addClassificacao(int classificacao) {
		this.classificacao.add(classificacao);
	}

	@Override
	public int mediaClassificacao() {
		int total = 0;
		int n = 0;
		for (Integer i : this.classificacao) {
			total += i;
			n++;
		}
		return total / n;
	}
}