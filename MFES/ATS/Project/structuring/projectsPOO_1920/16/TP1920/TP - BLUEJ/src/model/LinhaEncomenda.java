package src.model;

import java.io.Serializable;

/**
 * Classe que cria as Linhas de model.Encomenda que constituem uma encomenda
 */

public class LinhaEncomenda implements Serializable {

    //Variáveis de Instância
    private Produto produto;
    private double quantidade;
    private double preco;
    private double peso;

    /**
     * Construtores da classe model.LinhaEncomenda.
     * Declaração dos construtores por omissão (vazio),
     * parametrizado e de cópia.
     */

    /**
     * Construtor por omissão de model.LinhaEncomenda.
     */
    public LinhaEncomenda(){
        this.produto = new Produto();
        this.quantidade = 0;
        this.preco = 0;
        this.peso = 0;
    }

    /**
     * Construtor parametrizado de model.Classificacao.
     * Aceita como parâmetros os valores para cada variável de instância.
     */
    public LinhaEncomenda(Produto p, double quantidade){
        this.produto = p.clone();
        this.quantidade = quantidade;
        this.preco = p.getCusto() * quantidade;
        this.peso = p.getPeso() * quantidade;
    }

    /**
     * Construtor de cópia de model.Classificacao.
     * Aceita como parâmetro outra model.Classificacao e utiliza os métodos
     * de acesso aos valores das variáveis de instância.
     */
    public LinhaEncomenda(LinhaEncomenda l){
        this.produto = l.getProduto();
        this.quantidade = l.getQuantidade();
        this.preco = l.getPreco();
        this.peso = l.getPeso();
    }

    /**
     * Métodos de Instância
     */

    /**
     * Devolve o preço da model.LinhaEncomenda
     * @return double preço da model.LinhaEncomenda
     */

    public double getPreco() {
        return this.preco;
    }

    /**
     * Devolve o peso da model.LinhaEncomenda
     * @return double peso da model.LinhaEncomenda
     */

    public double getPeso() {
        return this.peso;
    }

    /**
     * Devolve a quantidade do produto da model.LinhaEncomenda
     * @return double quantidade do produto da model.LinhaEncomenda
     */

    public double getQuantidade() {
        return this.quantidade;
    }

    /**
     * Devolve uma cópia do model.Produto da model.LinhaEncomenda
     * @return Objeto model.Produto que é uma cópia exata do model.Produto presente na model.LinhaEncomenda
     */

    public Produto getProduto() {
        return this.produto.clone();
    }

    /**
     * Atualiza o preço da model.LinhaEncomenda
     * @param preco novo preço da model.LinhaEncomenda
     */

    public void setPreco(double preco) {
        this.preco = preco;
    }

    /**
     * Atualiza o peso da model.LinhaEncomenda
     * @param peso novo peso da model.LinhaEncomenda
     */

    public void setPeso(double peso) {
        this.peso = peso;
    }

    /**
     * Atualiza o model.Produto de uma model.LinhaEncomenda
     * @param produto novo produto da model.LinhaEncomenda
     */

    public void setProduto(Produto produto) {
        this.produto = produto.clone();
    }

    /**
     * Atualiza a quantidade do model.Produto da model.LinhaEncomenda
     * @param quantidade nova quantidade do model.Produto
     */

    public void setQuantidade(double quantidade) {
        this.quantidade = quantidade;
    }

    /**
     * Método que determina se uma model.LinhaEncomenda é igual a um Objeto
     * @param o Object
     * @return true caso o seja igual a model.LinhaEncomenda, false caso contrário
     */

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        LinhaEncomenda l = (LinhaEncomenda) o;
        return l.getQuantidade() == this.quantidade &&
                l.getPreco() == this.preco &&
                l.getPeso() == this.peso &&
                this.produto.equals( l.getProduto());
    }

    /**
     * Método que transforma uma model.LinhaEncomenda numa String
     * @return String com toda a informação do objeto model.LinhaEncomenda
     */

    public String toString() {
        final StringBuilder sb = new StringBuilder("[ ");
        sb.append("produto= ").append(this.produto.toString());
        sb.append("; quantidade= ").append(this.quantidade);
        sb.append("; peso= ").append(this.peso);
        sb.append("; preco= ").append(this.preco);
        sb.append('\n');
        return sb.toString();
    }


    /**
     * Método que retorna uma cópia do objeto model.LinhaEncomenda
     * @return Objeto model.LinhaEncomenda "clonado"
     */

    public LinhaEncomenda clone(){
        return new LinhaEncomenda(this);
    }
}
