package trazaqui;

import java.io.Serializable;
import java.util.ArrayList;

public class LinhaEncomenda implements Serializable {
    private String codProd;
    private String descricao;
    private double preco;
    private double quantidade;


    public LinhaEncomenda() {
        this.codProd = "n/a";
        this.descricao = "n/a";
        this.preco = 0.0;
        this.quantidade=0;
    }

    public LinhaEncomenda(String cod, String descricao, double preco, double qnt) {
        this.codProd = cod;
        this.descricao = descricao;
        this.preco = preco;
        this.quantidade=qnt;
    }

    public LinhaEncomenda(LinhaEncomenda linha) {
        this.codProd = linha.getCodProd();
        this.descricao = linha.getDescricao();
        this.preco = linha.getPreco();
        this.quantidade=linha.getQuantidade();
    }

    public String getCodProd() {
        return this.codProd;
    }

    public void setCodProd(String cod) {
        this.codProd= cod;
    }

    public String getDescricao() {
        return this.descricao;
    }

    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    public double getPreco() {
        return this.preco;
    }

    public void setPreco(double preco) {
        this.preco = preco;
    }

    public double getQuantidade() {
        return this.quantidade;
    }

    public void setQuantidade(double qnt) { this.quantidade = qnt;
    }

    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }

    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        LinhaEncomenda le = (LinhaEncomenda) obj;
        return le.getCodProd().equals(this.codProd) &&
                le.getDescricao().equals(this.descricao) &&
                le.getPreco() == this.preco;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("\n").append("Codigo Produto:").append(this.codProd).append("\n").append("Descriçao:")
                .append(this.descricao).append("\n").append("Preco:").append(this.preco).append("€")
                .append("\n").append("Quantidade:").append(this.quantidade).append("Kg");
        return sb.toString();
    }
}