package projeto.model;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.ArrayList;

public class EncomendasAceites extends Encomenda implements Serializable{

	private String transportador;
	
	public EncomendasAceites() {
		super();
	}
	
	public EncomendasAceites(EncomendasAceites enc) {
		super(enc);
	}
	
	public EncomendasAceites(String codigo, String dest, String loja, double peso, ArrayList<LinhaEncomenda> prod, String transportador, LocalDate d) {
		super(codigo, dest, loja, peso, prod, d);
		this.transportador = transportador;
	}
	
	public EncomendasAceites(String codigo) {
		super(codigo);
	}
	
	
	public EncomendasAceites clone() {
		return new EncomendasAceites(this);
	}
	
	// GETTERS E SETTERS
	
	public String getTransportador() {
		return transportador;
	}
	public void setTransportador(String transportador) {
		this.transportador = transportador;
	}
	
	
	public String toString() {
		StringBuilder sb = new StringBuilder("Encomenda Aceite{ ");
		sb.append("Codigo Encomenda ='").append(this.getCodigo()).append('\'');
		sb.append('}');
		return sb.toString();
	}
	
	
	
}