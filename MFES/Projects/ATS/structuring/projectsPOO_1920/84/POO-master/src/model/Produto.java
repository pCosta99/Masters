package model;

import java.io.Serializable;

public abstract class Produto implements Serializable {

    private double preco; // mudei para double
    private String idLoja;
    private double peso;
    private int quantidade;


    public Produto(){
        this.preco = 0;
        this.peso = 0;
        this.quantidade = 0;
    }

    public Produto(double preco,String idloja,double peso,int quantidade){
        this.preco = preco;
        this.idLoja = idloja;
        this.peso = peso;
        this.quantidade = quantidade;
    }

    public Produto(Produto prod){
        setPeso(prod.peso);
        setPreco(prod.preco);
        setIdLoja(prod.idLoja);
        setQuantidade(prod.getQuantidade());

    }

    public String getIdLoja() {
        return idLoja;
    }

    public void setIdLoja(String idLoja) {
        this.idLoja = idLoja;
    }

    public int getQuantidade() {
        return quantidade;
    }

    public void setQuantidade(int quantidade) {
        this.quantidade = quantidade;
    }

    public double getPeso() {
        return peso;
    }

    public void setPeso(double peso) {
        this.peso = peso;
    }

    public double getPreco() {
        return preco;
    }

    public void setPreco(double preco) {
        this.preco = preco;
    }

    public boolean equals(Object obj) {
        if (obj == this) return true;

        if (obj == null || obj.getClass() != this.getClass()) return false;

        Produto prod = (Produto) obj;

        return this.getPeso() == prod.getPeso()
                && this.getIdLoja().equals(prod.getIdLoja())
                && this.getPreco() == prod.getPreco()
                && this.getQuantidade() == prod.getQuantidade();
    }


    public abstract Produto clone();


    public String toString(){
        StringBuilder sb = new StringBuilder();

        sb.append("Preço: ");
        sb.append(this.preco);
        sb.append("\n");
        sb.append("Peso: ");
        sb.append(this.peso);
        sb.append("\n");
        sb.append("Quantidade");
        sb.append(this.quantidade);
        sb.append("\n");
        sb.append("Id de Loja");
        sb.append(this.idLoja);
        sb.append("\n");

        return sb.toString();
    }

}