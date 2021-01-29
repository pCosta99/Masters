package Model;

import java.io.Serializable;

public class LinhaEncomenda implements ILinhaEncomenda, Serializable {

	private String codProduto;
	private String descricao;
	private double quantidade;
	private double valorUnitario;

	public LinhaEncomenda() {
		this.codProduto = new String();
		this.descricao = new String();
		this.quantidade = 0.0;
		this.valorUnitario = 0.0;
	}

	public LinhaEncomenda(String codP, String d, double q, double vU) {
		setCodP(codP);
		setDescricao(d);
		setQuantidade(q);
		setValorUnitario(vU);
	}

	public LinhaEncomenda(LinhaEncomenda le) {
		setCodP(le.getCodP());
		setDescricao(le.getDescricao());
		setQuantidade(le.getQuantidade());
		setValorUnitario(le.getValorUnitario());
	}

	public String getCodP() {
		return this.codProduto;
	}

	public String getDescricao() {
		return this.descricao;
	}

	public double getQuantidade() {
		return this.quantidade;
	}

	public double getValorUnitario() {
		return this.valorUnitario;
	}

	public void setCodP(String codP) {
		this.codProduto = codP;
	}

	public void setDescricao(String d) {
		this.descricao = d;
	}

	public void setQuantidade(double q) {
		this.quantidade = q;
	}

	public void setValorUnitario(double vU) {
		this.valorUnitario = vU;
	}

	public LinhaEncomenda clone() {
		return new LinhaEncomenda(this);
	}

	public boolean equals(Object o) {
		if (o == this) return true;
		if (o == null || o.getClass() != this.getClass()) return false;
		LinhaEncomenda le = (LinhaEncomenda) o;
		return this.codProduto.equals(le.getCodP())
			&& this.descricao.equals(le.getDescricao());
	}

	public String toString() {
		String formattedString = new String();
		StringBuilder sb = new StringBuilder();
		sb.append("Código do Produto: ").append(codProduto).append("\n")
		  .append("Descrição: ").append(descricao).append("\n")
		  .append("Quantidade: ").append(quantidade).append("\n")
		  .append("Valor Unitário: ").append(valorUnitario).append("\n").append("\n");
		formattedString = sb.toString().replace(",","").replace("[","").
				replace("]","");
		return formattedString;
	}

}