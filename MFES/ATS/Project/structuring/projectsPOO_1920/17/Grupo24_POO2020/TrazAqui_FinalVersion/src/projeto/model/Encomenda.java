package projeto.model;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Encomenda implements Serializable{

	private String codigo;
	private String destinatario;
	private String loja;
	private double pesoKg;
	private ArrayList<LinhaEncomenda> produtos;
	private LocalDate data;
	private String entregador;
	private boolean encomendaMedica;
	private Gestao g;
	
	//addded later
	//private double precoFinal;
	
	
	// CONSTRUTORES
	
	public Encomenda() {
		this.codigo = "";
		this.destinatario = "";
		this.loja = "";
		this.pesoKg = 0;
		this.produtos = null;
		this.data = null;
		this.entregador = null;
	}
	
	public Encomenda(Encomenda e) {
		this.codigo = e.getCodigo();
		this.destinatario = e.getDestinatario();
		this.loja = e.getLoja();
		this.pesoKg = e.getPesoKg();
		this.produtos = e.getProdutos();
		this.data = e.getData();
		this.entregador = e.getEntregador();
	}


	public Encomenda(String codigo, String dest, String loja, double peso, ArrayList<LinhaEncomenda> prod, LocalDate d) {
		this.codigo = codigo;
		this.destinatario = dest;
		this.loja = loja;
		this.pesoKg = peso;
		this.produtos = prod;
		this.data = d;
	}
	
	public Encomenda(String codigo, String dest, String loja, double peso, ArrayList<LinhaEncomenda> prod) {
		this.codigo = codigo;
		this.destinatario = dest;
		this.loja = loja;
		this.pesoKg = peso;
		this.produtos = prod;
	}
	
	public Encomenda(String codigo, String dest, String loja, double peso) {
		this.codigo = codigo;
		this.destinatario = dest;
		this.loja = loja;
		this.pesoKg = peso;

	}
	
	public Encomenda(String codigo) {
		this.codigo = codigo;
	}
	
	public Encomenda(String dest, String loja) {
		this.destinatario = dest;
		this.loja = loja;
		this.codigo = Gerador.stringGenerator();
	}
	
	public Encomenda(String dest, String loja, ArrayList<LinhaEncomenda> prod) {
		this.destinatario = dest;
		this.loja = loja;
		this.produtos = prod;
		this.codigo = Gerador.stringGenerator();
		this.data = LocalDate.now();
	}
	
	// CLONE, TOSTRING, EQUALS
	
	public Encomenda clone() {
		return new Encomenda(this);
	}
	
	public boolean equals(Object o) {
		if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Encomenda u = (Encomenda) o;
		return (this.getCodigo().equals(u.getCodigo()));
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder("Encomenda { ");
		sb.append("Código do Utilizador ='").append(this.destinatario).append('\'');
		sb.append(" Data = ").append(this.data).append('\'');
		sb.append(", Loja ='").append(this.loja).append('\'');
		sb.append(", Produtos ='").append(this.produtos).append('\'');
		sb.append(", Empresa/Voluntário ='").append(this.entregador).append('\'');
		sb.append('}');
		return sb.toString();
	}
	
	
	// GETTERS E SETTERS

	public String getCodigo() {
		return codigo;
	}

	public void setCodigo(String codigo) {
		this.codigo = codigo;
	}

	public String getDestinatario() {
		return destinatario;
	}

	public void setDestinatario(String destinatario) {
		this.destinatario = destinatario;
	}

	public String getLoja() {
		return loja;
	}

	public void setLoja(String loja) {
		this.loja = loja;
	}

	public double getPesoKg() {
		return pesoKg;
	}

	public void setPesoKg(double pesoKg) {
		this.pesoKg = pesoKg;
	}

	public ArrayList<LinhaEncomenda> getProdutos() {
		return (ArrayList<LinhaEncomenda>) produtos;
	}

	public void setProdutos(ArrayList<LinhaEncomenda> produtos) {
		this.produtos = produtos;
	}
	
	public LocalDate getData() {
		return data;
	}

	public void setData(LocalDate data) {
		this.data = data;
	}
	
	public String getEntregador() {
		return entregador;
	}

	public void setEntregador(String entregador) {
		this.entregador = entregador;
	}
	
	public boolean isEncomendaMedica() {
		return encomendaMedica;
	}

	public void setEncomendaMedica(boolean encomendaMedica) {
		this.encomendaMedica = encomendaMedica;
	}
	
	public double precoFinal() {
		double precoFinal = 0;
		ArrayList<LinhaEncomenda> prods = (ArrayList<LinhaEncomenda>) this.getProdutos();
		for(LinhaEncomenda le : prods) {
			double p = le.getValorUnitario() * le.getQuantidade();
			precoFinal += p;
		}
		
		return precoFinal;
	}
	
	//OUTROS MÉTODOS
	
	
	public List<Encomenda> encomendasFiltro(String transp){
		return this.g.encomendas.values().
				                 stream().
				                 filter(e -> e.getEntregador().equals(transp)).
				                 collect(Collectors.toList());				                
	}


	
	
	
	
	
}