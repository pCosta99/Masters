package Model;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class Loja extends Account {

	private boolean aceitaFila;
	private List<Encomenda> filaDeEspera;

	public Loja() {
		super();
		this.aceitaFila = false;
		this.filaDeEspera = new ArrayList<>();
	}

	public Loja (String codL, String nomeL, GPS coordenadas, String email, boolean aceitaFila, ArrayList<Encomenda> q) {
		super(codL, nomeL, coordenadas, email);
		this.aceitaFila = aceitaFila;
		setFilaDeEspera(q);
	}

	public Loja (Loja l) {
		super(l);
		this.aceitaFila = l.getAceitaFila();
		setFilaDeEspera(l.getFilaDeEspera());
	}

	public boolean getAceitaFila() {
		return this.aceitaFila;
	}

	public List<Encomenda> getFilaDeEspera() {
		List<Encomenda> aux = new ArrayList<>();
		for (Encomenda e : this.filaDeEspera)
			aux.add(e.clone());
		return aux;
	}

	public void setAceitaFila(boolean aceitaFila) {
		this.aceitaFila = aceitaFila;
	}

	public void setFilaDeEspera(List<Encomenda> l) {
		this.filaDeEspera = new ArrayList<>();
		l.forEach(as -> this.filaDeEspera.add(as.clone()));
	}

	public Loja clone() {
		return new Loja(this);
	}

	public boolean equals(Object o) {
		if(o == this) return true;
		if(o == null || o.getClass() != this.getClass()) return false;
		Loja l = (Loja) o;
		return (super.equals(l) && (this.aceitaFila == l.getAceitaFila())
				&& (this.filaDeEspera.equals(l.getFilaDeEspera())));
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("Loja: " + super.toString()).append('\n').append("Aceita Fila?: ").append(aceitaFila)
		.append("\n").append("Fila de Espera: ").append(filaDeEspera);

		return sb.toString();
	}

	public void adicionaEncomendaFilaDeEspera (Encomenda e) {
		this.filaDeEspera.add(e);
	}

	public int extraiIndiceEncomenda(String codEncomenda) {
		int i = 0;
		for (Encomenda e : this.filaDeEspera) {
			if (codEncomenda == e.getCodE()) {
				return i;
			}
			i++;
		}
		return i;
	}

	public double calculaValorTotalEncomenda(String codEncomenda) {
		Encomenda e = new Encomenda();
		int i = 0;
		double valorTotalEncomenda = 0;
		i = extraiIndiceEncomenda(codEncomenda);
		e = this.filaDeEspera.get(i);
		for (LinhaEncomenda l : e.getLinhasEncomenda()) {
			valorTotalEncomenda += (l.getValorUnitario() * l.getQuantidade());
		}
		return valorTotalEncomenda;
	}

	public List<RegistoEncomenda> queueParaRegisto(String quemTransportou) {
		int valorAleatorio = 0;
		double valorTotal = 0;
		Random generator = new Random();
		List<RegistoEncomenda> conjuntoRegistos = new ArrayList<>();
		RegistoEncomenda r = new RegistoEncomenda();
		for (Encomenda e : this.filaDeEspera) {
			r = new RegistoEncomenda();
			valorAleatorio = generator.nextInt(50);
			r.setCodEncomenda(e.getCodE());
			r.setQuemTransportou(quemTransportou);
			r.setDataEncomenda(LocalDateTime.now());
			r.setDistanciaPercorrida(valorAleatorio);
			valorTotal = calculaValorTotalEncomenda(e.getCodE());
			r.setValorTotalEncomenda(valorTotal + 1); // Evitar a Distância Zero.
			r.setEncomenda(e.clone());
			conjuntoRegistos.add(r);
		}
		return conjuntoRegistos;
	}

}