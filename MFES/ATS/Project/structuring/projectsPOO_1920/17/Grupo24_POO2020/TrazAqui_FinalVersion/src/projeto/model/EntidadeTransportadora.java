package projeto.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class EntidadeTransportadora extends Entidade implements Serializable {

	private double raio;
	private boolean livre;
	private int qtClassificacoes;
	private double classificacao;
	private boolean aceitaMedicamentosAgora;
	private ArrayList<String> entregasEfetuadas;
	

	private Gestao g;
	

	
	public EntidadeTransportadora() {
		super();
		this.raio = Gerador.doubleGenerator();
		this.livre = true;
		this.classificacao = 5;
		this.qtClassificacoes = 0;
		this.aceitaMedicamentosAgora = false;
	}
	
	public EntidadeTransportadora(EntidadeTransportadora e) {
		super(e);
		this.raio = e.getRaio();
		this.livre = e.isLivre();
		this.classificacao = e.getClassificacao();
		this.qtClassificacoes = e.getQtClassificacoes();
		this.aceitaMedicamentosAgora = e.aceitaMedicamentosAgora;
	}
	
	public EntidadeTransportadora(String codigo, String nome, double coordX, double coordY, double raio) {
		super(codigo, nome, coordX, coordY);
		this.raio = raio;
		this.livre = true;
		this.aceitaMedicamentosAgora = Gerador.booleanGenerator();
		this.classificacao = 5;
		this.qtClassificacoes = 0;
		
	}
	
	public EntidadeTransportadora(String codigo, String email, String password, String nome, double coordX, double coordY) {
		super(codigo, email, password, nome, coordX, coordY);
		this.raio = Gerador.doubleGenerator();
		this.livre = true;
		this.aceitaMedicamentosAgora = Gerador.booleanGenerator();
		this.classificacao = 5;
		this.qtClassificacoes = 0;
	}
	
	public EntidadeTransportadora(String codigo, String nome, double coordX, double coordY) {
		super(codigo, nome, coordX, coordY);
		this.raio = Gerador.doubleGenerator();
		this.livre = true;
		this.aceitaMedicamentosAgora = Gerador.booleanGenerator();
		this.classificacao = 5;
		this.qtClassificacoes = 0;
	}
	
	public EntidadeTransportadora(String nome, String email, String password) {
		super(nome,email,password);
		this.raio = Gerador.doubleGenerator();
		this.livre = true;
		this.aceitaMedicamentosAgora = Gerador.booleanGenerator();
		this.classificacao = 5;
		this.qtClassificacoes = 0;
		this.setCodigo(Gerador.stringGenerator());
		this.setCoordX(Gerador.doubleGenerator());
		this.setCoordY(Gerador.doubleGenerator());
	
	}

	
	
	
	
	public double getRaio() {
		return raio;
	}

	public void setRaio(double raio) {
		this.raio = raio;
	}

	public boolean isLivre() {
		return livre;
	}

	public void setLivre(boolean livre) {
		this.livre = livre;
	}

	public int getQtClassificacoes() {
		return qtClassificacoes;
	}

	public void setQtClassificacoes(int qtClassificacoes) {
		this.qtClassificacoes = qtClassificacoes;
	}
	
	public double getClassificacao() {
		return classificacao;
	}

	public void setClassificacao(double classificacao) {
		this.classificacao = classificacao;
	}

	
	public double distanciaAteDestino(double pontoX, double pontoY) {
		double gX = this.getCoordX();
		double gY = this.getCoordY();	
		return Math.sqrt((gY - pontoY) * (gY - pontoY) + (gX - pontoX) * (gX - pontoX));
	}

	
	public boolean estaDentroDoRaio(double dist) {
		// dist é a distância da loja até o ponto inicial do entregador
		if(dist <= this.getRaio()){
			return true;
		}
		
		return false;
	}
	
	public boolean possoRecolher() {
		this.livre = true;
		return true;
	}
	
	public boolean aceitoTransporteMedicamentos() {
		this.aceitaMedicamentosAgora = true;
		return true;
	}
	
	public void aceitaMedicamentos(boolean state) {
		this.aceitaMedicamentosAgora = state;
	}
	
	public double duracaoEntregaMin(Encomenda enc, Loja l) {
		Random rand = new Random();
		double d = 0;
		int fila = l.tempoMediaFilaMin;
		int rand_int = rand.nextInt(60);
		
		d = fila + rand_int;
		
		return d;
	}
	
	public List<String> verHistoricoEntregas() {
		
		List<String> l = new ArrayList<String>();
		
		for(Encomenda e : this.g.encomendas.values()) {
			if(e.getEntregador().equals(this.getCodigo())) {
				l.add(e.toString());
			}
		}
		
		return l;	
	}
	
	public ArrayList<String> getEntregasEfetuadas() {
		return entregasEfetuadas;
	}

	public void setEntregasEfetuadas(ArrayList<String> entregasEfetuadas) {
		this.entregasEfetuadas = entregasEfetuadas;
	}
	

}
