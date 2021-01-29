package Model;

import java.util.List;
import java.util.ArrayList;

public class Transportadora extends Account implements Classificacao {

	private String nif;
	private double raio;
	private double precoPorKM;
	private List<Integer> classificacao;
	private boolean aceita;

	public Transportadora() {
		this.nif = new String();
		this.raio = 0.0;
		this.precoPorKM = 0.0;
		this.classificacao = new ArrayList<>();
		this.aceita = true;
	}

	public Transportadora(String codE, String nomeE, GPS coordenadas, String email,
						  String nif, double raio, double precoPorKM,
						  List<Integer> classificacao, boolean aceita) {
		super(codE, nomeE, coordenadas, email);
		setNif(nif);
		setRaio(raio);
		setPreco(precoPorKM);
		setClassificacao(classificacao);
		setAceita(aceita);
	}

	public Transportadora(Transportadora t) {
		super(t);
		setNif(t.getNif());
		setRaio(t.getRaio());
		setPreco(t.getPreco());
		setClassificacao(t.getClassificacao());
		setAceita(t.getAceite());
	}

	public String getNif() {
		return this.nif;
	}

	public double getRaio() {
		return this.raio;
	}

	public double getPreco() {
		return this.precoPorKM;
	}

	public List<Integer> getClassificacao() {
		List<Integer> aux = new ArrayList();
		for (Integer i : this.classificacao)
			aux.add(i);
		return aux;
	}

	public boolean getAceite() {
		return this.aceita;
	}

	public void setNif(String n) {
		this.nif = n;
	}

	public void setRaio(double r) {
		this.raio = r;
	}

	public void setPreco(double p) {
		this.precoPorKM = p;
	}

	public void setClassificacao(List<Integer> c) {
		this.classificacao = new ArrayList<>();
		classificacao.forEach(aux -> this.classificacao.add(aux));
	}

	public void setAceita(boolean aceita) {
		this.aceita = aceita;
	}

	public void setAceite(boolean aceita) {
		this.aceita = aceita;
	}

	public Transportadora clone() {
		return new Transportadora(this);
	}

	public boolean equals(Object o) {
		if(o == this) return true;
		if(o == null || o.getClass() != this.getClass()) return false;
		Transportadora t = (Transportadora) o;
		return (super.equals(t) && (this.nif == t.getNif()) && (this.raio == t.getRaio())
				&& (this.precoPorKM == t.getPreco()) && (this.aceita == t.getAceite()));
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Transportadora: " + super.toString()).append(' ')
				.append(this.nif).append("\n")
				.append(this.raio).append("\n")
				.append(this.precoPorKM).append("\n")
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