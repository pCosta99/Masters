package model;

import java.io.Serializable;

public class Linha_Encomenda implements Serializable {
    private String codProduto;
    private String descricao;
    private double quantidade;
    private double preco;


    public Linha_Encomenda() {
        this.codProduto = "";
        this.descricao = new String();
        this.quantidade = 0.0;
        this.preco = 0.0;
    }

    public Linha_Encomenda(String codProduto, String descricao, double quantidade, double preco){
        this.codProduto = codProduto;
        this.descricao = descricao;
        this.quantidade = quantidade;
        this.preco = preco;
    }

    public Linha_Encomenda(Linha_Encomenda e) {
        this.codProduto = e.getCodProduto();
        this.descricao = e.getDescricao();
        this.preco = e.getPreco();
        this.quantidade = e.getQuantidade();
    }

    public String getCodProduto() {
        return this.codProduto;
    }

    public String getDescricao() {
        return this.descricao;
    }


    public double getQuantidade() {
        return this.quantidade;
    }
    public double getPreco() {
        return this.preco;
    }

    public void setCodProduto(String codProduto) {
        this.codProduto = codProduto;
    }

    public void setDesc(String descricao) {
        this.descricao = descricao;
    }

    public void setQtd(double quantidade) {
        this.quantidade = quantidade;
    }

    public void setPreco(double preco) {
        this.preco = preco;
    }

    public double calculaValorLinhaEnc(){ return this.preco * this.quantidade; }

    public Linha_Encomenda clone(){
        return new Linha_Encomenda(this);
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        Linha_Encomenda le = (Linha_Encomenda) o;
        return (this.codProduto.equals(le.getCodProduto()) &&
                this.descricao.equals(le.getDescricao()) &&
                this.preco == le.getPreco() &&
                this.quantidade == le.getQuantidade());
    }

    @Override
    public String toString() {
        return  "CodProduto= " + this.codProduto +
                ", Descricao= " + this.descricao +
                ", Preco=" + this.preco +
                ", Quantidade=" + this.quantidade;
    }
}