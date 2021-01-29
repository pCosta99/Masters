package projeto.model;

import java.io.Serializable;

public class LinhaEncomenda implements Serializable{

	private String codigoProduto;
	private String descricao;
	private double quantidade;
	private double valorUnitario;
	
	public LinhaEncomenda() {
		this.codigoProduto = "";
		this.descricao = "";
		this.quantidade = 0;
		this.valorUnitario = 0.00;
	}
	
	public LinhaEncomenda(LinhaEncomenda l) {
		this.codigoProduto = l.getCodigoProduto();
		this.descricao = l.getDescricao();
		this.quantidade = l.getQuantidade();
		this.valorUnitario = l.getValorUnitario();
	}
	
	public LinhaEncomenda(String cod, String desc, double qt, double unit) {
		this.codigoProduto = cod;
		this.descricao = desc;
		this.quantidade = qt;
		this.valorUnitario = unit;
	}
	
	public LinhaEncomenda(String descricao) {
		this.codigoProduto = Gerador.stringGenerator();
		this.descricao = descricao;
		this.quantidade = Gerador.doubleGenerator();
		this.valorUnitario = Gerador.doubleGenerator();
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder("");
		sb.append("Codigo do Produto ='").append(this.codigoProduto).append('\'');
		sb.append(", Nome ='").append(this.descricao).append('\'');
		sb.append(", Quantidade ='").append(this.quantidade).append('\'');
		sb.append(", Preço unitário ='").append(this.valorUnitario).append('\'');
		return sb.toString();
	}
	
	// GETTERS E SETTERS
	public String getCodigoProduto() {
		return codigoProduto;
	}

	public void setCodigoProduto(String codigoProduto) {
		this.codigoProduto = codigoProduto;
	}

	public String getDescricao() {
		return descricao;
	}

	public void setDescricao(String descricao) {
		this.descricao = descricao;
	}

	public double getQuantidade() {
		return quantidade;
	}

	public void setQuantidade(double quantidade) {
		this.quantidade = quantidade;
	}

	public double getValorUnitario() {
		return valorUnitario;
	}

	public void setValorUnitario(double valorUnitario) {
		this.valorUnitario = valorUnitario;
	}

}
