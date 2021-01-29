package trazaqui;


import java.io.Serializable;

public class Produto implements Serializable {
    private String codProd;
    private String descricao;
    private double precoUnitario;
    private double stock;


    public Produto() {
        this.codProd = "n/a";
        this.descricao = "n/a";
        this.precoUnitario = 0.0;
        this.stock=0.0;
    }

    public Produto(String cod, String descricao, double preco, double qnt) {
        this.codProd = cod;
        this.descricao = descricao;
        this.precoUnitario = preco;
        this.stock=qnt;
    }

    public Produto(Produto linha) {
        this.codProd = linha.getCodProd();
        this.descricao = linha.getDescricao();
        this.precoUnitario = linha.getPreco();
        this.stock=linha.getStock();
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
        return this.precoUnitario;
    }

    public void setPreco(double preco) {
        this.precoUnitario = preco;
    }

    public double getStock() {
        return this.stock;
    }

    public void setStock(double qnt) { this.stock = qnt;
    }

    public Produto clone() {
        return new Produto(this);
    }

    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Produto le = (Produto) obj;
        return le.getCodProd().equals(this.codProd) &&
                le.getDescricao().equals(this.descricao) &&
                le.getPreco() == this.precoUnitario &&
                le.getStock() == this.stock;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Codigo Produto:").append(this.codProd).append("\n").append("Descriçao:")
                .append(this.descricao).append("\n").append("Preco:").append(this.precoUnitario).append("\n")
                .append("Quantidade:").append(this.stock).append("\n")
                .append("\n\n");
        return sb.toString();
    }
}
