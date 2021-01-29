package Model;

import java.io.Serializable;
import java.text.DecimalFormat;


/**
 * Classe Produto
 * @author grupo 115
 */


public class Produto implements Serializable {

    private String referencia;
    private String nome;
    private double quantidade;
    private double preco;
    private double peso;

    public Produto(){
        this.referencia = "";
        this.nome= "";
        this.quantidade= 0;
        this.preco = 0;
        this.peso = 0;
    }


    public Produto(String referencia, String nome, double quantidade, double preco ,boolean medicinal, double peso) {
        this.referencia = referencia;
        this.nome = nome;
        this.quantidade = quantidade;
        this.preco = preco;
        this.peso = peso;
    }

    public Produto(Produto a){
        this.referencia = a.getReferencia();
        this.nome = a.getNome();
        this.quantidade = a.getQuantidade();
        this.preco = a.getPreco();
        this.peso = a.getPeso();


    }

    //GETTER AND SETTER


    public String getReferencia() {
        return referencia;
    }

    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public double getQuantidade() {
        return quantidade;
    }

    public void setQuantidade(double quantidade) {
        this.quantidade = quantidade;
    }

    public double getPreco() {
        return preco;
    }

    public void setPreco(double preco) {
        this.preco = preco;
    }




    public void setReferencia(String referencia) {
        this.referencia = referencia;
    }

    public double getPeso() {
        return peso;
    }

    public void setPeso(double peso) {
        this.peso = peso;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Produto produto = (Produto) o;
        return quantidade == produto.quantidade &&
                nome.equals(produto.nome);
    }

    public String navString() {
        DecimalFormat df = new DecimalFormat("####0.00");
        StringBuilder sb = new StringBuilder();
        sb.append(this.getReferencia() + " " + this.getNome() + " " + df.format(this.getPreco()) + "$");
        return sb.toString();
    }

    @Override
    public String toString() {
        return "Produto{" +
                "referencia='" + referencia + '\'' +
                ", nome='" + nome + '\'' +
                ", quantidade=" + quantidade +
                ", preco=" + preco +
                '}';
    }

    public Produto clone()
    {
        return new Produto(this);
    }

}


