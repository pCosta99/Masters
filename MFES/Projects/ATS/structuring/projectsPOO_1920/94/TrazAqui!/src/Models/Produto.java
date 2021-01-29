package Models;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;

/**
 * Classe que guarda a informação de um produto
 *
 */
public class Produto implements Serializable {
    private String cod, nome;
    private double preco, quantidade, peso;

    /**
     * Construtor da classe
     * @param c código do produto
     * @param n nome do produto
     * @param p preço do produto
     * @param q quantidade do produto
     * @param peso peso do produto
     */
    public Produto(String c, String n, double p, double q, double peso) {
        this.cod = c;
        this.nome = n;
        this.preco = p;
        this.quantidade = q;
        this.peso = peso;
    }

    /**
     * Construtor de classe
     * @param l
     */
    public Produto(Produto l) {
        this.cod = l.getCod();
        this.nome = l.getNome();
        this.preco = l.getPreco();
        this.quantidade = l.getQuantidade();
        this.peso = l.peso;
    }

    /**
     * Construtor de classe
     * @param l
     * @param quantidade
     */
    public Produto(Produto l,double quantidade){
        this.cod = l.getCod();
        this.nome = l.getNome();
        this.preco = l.getPreco();
        this.quantidade = quantidade;
        this.peso = l.peso;
    }

    /**
     * Verifica se um produto existe numa lista de produtos
     * @param produto um produto
     * @param produtos lista de produtos
     * @return true caso exista false caso contrario
     */
    public static boolean is_valid_product(String produto, List<Produto> produtos) {
        for (Produto e : produtos) {
            if (e.getCod().equals(produto)) return true;
        }
        return false;
    }

    /**
     * Devolve um produto a partir do nome
     * @param produto nome de um produto
     * @param produtos lista de produtos
     * @return retorna o produto com esse nome
     */
    public static Produto get_by_name(String produto, List<Produto> produtos) {
        for (Produto e : produtos) {
            if (e.getNome().equals(produto)) return e.clone();
        }

        return null; //nao existe
    }

    public double getPeso() {
        return peso;
    }

    public void setPeso(double peso) {
        this.peso = peso;
    }

    public String getCod() {
        return cod;
    }

    public void setCod(String cod) {
        this.cod = cod;
    }

    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public double getPreco() {
        return preco;
    }

    public void setPreco(double preco) {
        this.preco = preco;
    }

    public double getQuantidade() {
        return quantidade;
    }

    public void setQuantidade(double quantidade) {
        this.quantidade = quantidade;
    }

    public void setQuantidade(int quantidade) {
        this.quantidade = quantidade;
    }

    public Produto clone() {
        return new Produto(this);
    }

    public String toString() {
        return "\n* " + this.nome + " - Código " + this.cod +
                "\n  Quantidade: " + this.quantidade +
                "\n  Preço: " + this.preco + "\n  Peso: " + this.peso ;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Produto)) return false;
        Produto produto = (Produto) o;
        return Double.compare(produto.preco, preco) == 0 &&
                Double.compare(produto.quantidade, quantidade) == 0 &&
                Double.compare(produto.peso, peso) == 0 &&
                Objects.equals(cod, produto.cod) &&
                Objects.equals(nome, produto.nome);
    }

    @Override
    public int hashCode() {
        return Objects.hash(cod, nome, preco, quantidade, peso);
    }
}
