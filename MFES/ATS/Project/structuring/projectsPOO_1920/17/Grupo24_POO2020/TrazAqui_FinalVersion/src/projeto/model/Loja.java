package projeto.model;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Set;

public class Loja extends Entidade implements Serializable{

	public int tempoMediaFilaMin = 0;
	private Gestao g;
	private ArrayList<String> encomendasEfetuadas;

	

	// CONSTRUTORES
	public Loja(){
		super();
		this.tempoMediaFilaMin = 0;
		
	}
	
	public Loja(Loja l) {
		super(l);
		this.tempoMediaFilaMin = l.getTempoMediaFilaMin();
	}
	
	public Loja(String codigo, String nome) {
		super(codigo, nome);
		this.tempoMediaFilaMin = 0;
		ArrayList<String> nova = new ArrayList<String>();
		nova.add("->");
		this.setEncomendasEfetuadas(nova);
	}
	
	public Loja(String codigo, String nome, double coordX, double coordY) {
		super(codigo, nome, coordX, coordY);
		this.tempoMediaFilaMin = 0;
		ArrayList<String> nova = new ArrayList<String>();
		nova.add("->");
		this.setEncomendasEfetuadas(nova);
	}
	
	public Loja(String nome, String email, String pass) {
		super(nome,email,pass);
		this.setCodigo(Gerador.stringGenerator());
		ArrayList<String> nova = new ArrayList<String>();
		nova.add("->");
		this.setEncomendasEfetuadas(nova);
	}
	
	// CLONE, TOSTRING E EQUALS
	
	public Loja clone() {
		return new Loja(this);
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder("");
		sb.append("Nome: '").append(this.getNome()).append('\'');
		return sb.toString();
	}
	
	public boolean equals(Object o) {
		if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Utilizador u = (Utilizador) o;
		return (this.getCodigo().equals(u.getCodigo()));
	}
	
	
	// GETTERS E SETTERS
	
	public int getTempoMediaFilaMin() {
		return tempoMediaFilaMin;
	}

	public void setTempoMediaFilaMin(int tempoMediaFilaMin) {
		this.tempoMediaFilaMin = tempoMediaFilaMin;
	}
	
	public ArrayList<String> getEncomendasEfetuadas() {
		return encomendasEfetuadas;
	}

	public void setEncomendasEfetuadas(ArrayList<String> encomendasEfetuadas) {
		this.encomendasEfetuadas = encomendasEfetuadas;
	}
	
	// OUTROS MÉTODOS
	
	// Checa se existe uma encomenda feita à loja
	public boolean existeEncomendaASerEntregue() {
		
		for(Encomenda enc: this.g.encomendas.values()) {
			//System.out.println(enc);
			if(enc.getLoja().equals(this.getCodigo())) {
				return true;
			}
		}
		
		return false;
	}
	
	public EntidadeTransportadora buscarPorEntregador(Encomenda enc) {
		this.g = new Gestao();
		double dist_min = 400;
		double dist = 0;
		EntidadeTransportadora selecionado = null;
		double lojaX = this.getCoordX();
		double lojaY = this.getCoordY();
		for(EntidadeTransportadora e : this.g.entregadores.values()) {
			if(e.isLivre()) {
				dist = e.distanciaAteDestino(lojaX, lojaY);
				/*if(dist >= e.getRaio()) {
					System.out.println("Ué");
				}*/
				if(dist <= dist_min) {
					dist_min = dist;
					selecionado = e;
				}
				
			}
		}
		
		
		//System.out.println(selecionado.getClass());
		
		
		
		
		return selecionado;
	}
	
	
	public void definirPropriedadesEncomenda(Encomenda enc) {		
		enc.setPesoKg(Gerador.doubleGenerator());
		enc.setData(LocalDate.now());
	}
	
	// busca pelo entregador mais perto e que aceite encomendas médicas
	public EntidadeTransportadora buscarPorEntregadorMedico(Encomenda enc) {
		this.g = new Gestao();
		double dist_min = 400;
		double dist = 0;
		EntidadeTransportadora selecionado = null;
		double lojaX = this.getCoordX();
		double lojaY = this.getCoordY();
		for(EntidadeTransportadora e : this.g.entregadores.values()) {
			if(e.aceitoTransporteMedicamentos()) {
				if(e.isLivre()) {
					dist = e.distanciaAteDestino(lojaX, lojaY);
					if(dist <= dist_min) {
						dist_min = dist;
						selecionado = e;
					}		
				}
			}
		}
		// se o entregador mais perto for um voluntário, já é automaticamente aceite.
		if(selecionado instanceof Voluntario) {
			System.out.println("É voluntario");
			this.g.aceites.put(enc.getCodigo(), enc);
			//System.out.println(Gerador.aceites.values());
		}
		else {
			System.out.println("Não é voluntário");
			// TODO : perguntar ao utilizador para ver se ele aceita
		}
		
		
		if(enc != null && selecionado != null) {
			enc.setEntregador(selecionado.getCodigo());
			System.out.println("Encomenda já tem entregador: " + selecionado);
		}
		
		return selecionado;
		
	}

	
	
	
	
	
	//
	
	
	
	
	
	
	
	
}
