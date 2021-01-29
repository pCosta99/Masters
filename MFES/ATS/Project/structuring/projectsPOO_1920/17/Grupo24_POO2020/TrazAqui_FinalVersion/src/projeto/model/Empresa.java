package projeto.model;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class Empresa extends EntidadeTransportadora implements Entregador, Serializable{
	
	private double NIF;
	private double precoKm;
	private double faturamento;
	private double kmsPercorridos;

	

	public Empresa() {
		super();
		this.NIF = 0;
		this.precoKm = 0;
		this.faturamento = 0.0;
		this.kmsPercorridos = 0.0;
	}
	
	public Empresa(Empresa e) {
		super(e);
		this.NIF = e.getNIF();
		this.precoKm = e.getPrecoKm();
		this.faturamento = e.getFaturamento();
		this.kmsPercorridos = e.getKmsPercorridos();
	}
	
	public Empresa(String codigo, double NIF, String email, String password, String nome, double coordX, double coordY, double precoKm, double raio) {
		super(codigo, email, password, nome, coordX, coordY);
		this.precoKm = precoKm;
		this.NIF = NIF;
		
	}
	
	public Empresa(String codigo, double NIF, String nome, double coordX, double coordY, double precoKm, double raio) {
		super(codigo, nome, coordX, coordY);
		this.precoKm = precoKm;
		this.NIF = NIF;
		ArrayList<String> nova = new ArrayList<String>();
		nova.add("->");
		this.setEntregasEfetuadas(nova);
	}
	
	public Empresa(String codigo, String nome, double coordX, double coordY) {
		super(codigo,nome,coordX,coordY);
		this.precoKm = Gerador.doubleGenerator();
		this.NIF = Gerador.nifGenerator();
	}
	
	public Empresa(String nome, String mail, String pass) {
		super(nome,mail,pass);
		this.NIF = Gerador.nifGenerator();
		
	}
	
	// CLONE, TOSTRING, EQUALS
	
	public Empresa clone() {
		return new Empresa(this);
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder("Empresa: ");
		sb.append("Nome='").append(this.getNome()).append('\'');
		sb.append(", Email ='").append(this.getEmail()).append('\'');
		sb.append(" Codigo da Empresa = '").append(this.getCodigo()).append('\'');
		sb.append(" NIF = '").append(this.getNIF()).append('\'');
		return sb.toString();
	 }
	
	public boolean equals(Object o) {
		if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Empresa u = (Empresa) o;
        Empresa em = (Empresa) o;
		return (this.getCodigo().equals(u.getCodigo()) && this.getEmail().equals(em.getEmail()));
	}
	
	
	
	// GETTERS E SETTERS

	public double getNIF() {
		return NIF;
	}

	public void setNIF(double nIF) {
		NIF = nIF;
	}

	public double getPrecoKm() {
		return precoKm;
	}

	public void setPrecoKm(double precoKm) {
		this.precoKm = precoKm;
	}


	public double getFaturamento() {
		return faturamento;
	}

	public void setFaturamento(double faturamento) {
		this.faturamento = faturamento;
	}

	public double getKmsPercorridos() {
		return kmsPercorridos;
	}

	public void setKmsPercorridos(double kmsPercorridos) {
		this.kmsPercorridos = kmsPercorridos;
	}
	

	
	
	
	

	
	
	public double definirPrecoEntrega(double distKm, Loja loja, Encomenda enc) {
		int tempoMin = (int) duracaoEntregaMin(enc, loja); // 
		double precoFinal = 0;
		int fila = loja.tempoMediaFilaMin; // se a loja não fornecer, esse tempo é ZERO.
		double precoEncomenda = enc.precoFinal(); // definido na classe Encomenda
		int tempoAcumuladoMin = fila + tempoMin;
		
		precoFinal = precoEncomenda + (this.getPrecoKm() * distKm) + (0.1 * tempoAcumuladoMin);
		// preço final já com o valor da encomenda
		return precoFinal;
	}

	public double duracaoEntregaMin(Encomenda enc, Loja l) {
		Random rand = new Random();
		double d = 0;
		int fila = l.tempoMediaFilaMin;
		int rand_int = rand.nextInt(60);
		
		d = fila + rand_int;
		
		return d;
	}
	
	
	
	public int compareTo(Empresa o) {
		int r = Double.compare(this.getKmsPercorridos(), o.getKmsPercorridos());
		return r;
	}


	

	
	
	
	
}
