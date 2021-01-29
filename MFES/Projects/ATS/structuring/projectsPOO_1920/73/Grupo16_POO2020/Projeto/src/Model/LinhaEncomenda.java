package Model;

import java.io.Serializable;

public class LinhaEncomenda implements Serializable {

    private String codProd;
    private String descricao;
    private Produto produto; //FIXME
    private double quantidade;
    private double valorUnitario;

    public LinhaEncomenda(String codProd, String descricao, double quantidade, double valorUnitario) {
        this.codProd = codProd;
        this.descricao = descricao;
        this.quantidade = quantidade;
        this.valorUnitario = valorUnitario;
        this.produto = new Produto(codProd,descricao);
    }

    public LinhaEncomenda(Produto produto, double quantidade, double valorUnitario) {
        this.produto = produto;
        this.quantidade = quantidade;
        this.valorUnitario = valorUnitario;
    }

    public Produto getProduto() {
        return produto;
    }



    public void setProduto(Produto produto) {
        this.produto = produto;
    }

    public double getQuantidade() {
        return quantidade;
    }

    public void setQuantidade(int quantidade) {
        this.quantidade = quantidade;
    }

    public double getValorUnitario() {
        return valorUnitario;
    }

    public void setValorUnitario(float valorUnitario) {
        this.valorUnitario = valorUnitario;
    }

    @Override
    public String toString() {
        return "LinhaEncomenda{" +
                "codProd='" + codProd + '\'' +
                ", descricao='" + descricao + '\'' +
                ", produto=" + produto +
                ", quantidade=" + quantidade +
                ", valorUnitario=" + valorUnitario +
                '}';
    }
}
