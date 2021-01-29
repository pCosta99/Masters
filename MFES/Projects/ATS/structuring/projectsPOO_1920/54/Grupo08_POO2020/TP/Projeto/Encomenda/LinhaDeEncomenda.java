package Projeto.Encomenda;
import Projeto.Entidades.Produto;
import java.io.Serializable;
import java.util.Objects;

/**
 * Classe que implementa uma LinhaDeEncomenda.
 * Uma linha de encomenda serve para diferenciar os diferentes produtos dentro de uma Encomenda.
 */
public class LinhaDeEncomenda implements Serializable {
    private Produto produto;
    private int quantidade;

    /*
     * Construtores da Classe LinhaDeEncomenda.
     * Declaracao dos construtores por omissao, parametrizado e de copia.
     */
    /**
     * Construtor por omissao de LinhaDeEncomenda
     */
    public LinhaDeEncomenda() {
        this.produto = new Produto();
        this.quantidade = 0;
    }

    /**
     * Construtor parametrizado de Projeto.Encomenda.LinhaDeEncomenda.
     * Aceita como parametros um Produto e a respetiva quantidade
     */
    public LinhaDeEncomenda(Produto prod, int quantidade) {
        this.setProduto(prod);
        this.quantidade = quantidade;
    }

    /**
     * Construtor por copia de LinhaDeEncomenda.
     * Aceita como parametro outra LinhaDeEncomenda e utiliza os metodos de acesso aos valores das variaveis de instancia.
     */
    public LinhaDeEncomenda(LinhaDeEncomenda l) {
        this.produto = l.getProduto();
        this.quantidade = l.getQuantidade();
    }

    /*
     * Getters e Setters
     */

    public Produto getProduto() {
        return produto.clone();
    }

    public void setProduto(Produto produto) {
        this.produto = produto.clone();
    }

    public int getQuantidade() {
        return quantidade;
    }

    public void setQuantidade(int quantidade) {
        this.quantidade = quantidade;
    }

    /*
     * Restantes Metodos de Instancia
     */

    /**
     * Metodo que faz uma copia do objeto receptor da mensagem.
     *
     * @return objeto clone do objeto que recebe a mensagem.
     */
    public LinhaDeEncomenda clone() {
        return new LinhaDeEncomenda(this);
    }

    /**
     * Metodo que devolve a representaçao em String da LinhaDeEncomenda.
     *
     * @return String com as variaveis desta instancia.
     */
    @Override
    public String toString() {
        return "\nLinhaDeEncomenda: " +
                "\n\tQuantidade: " + this.quantidade +
                "\n\tProduto => " + this.produto.toString() +
                "\n";
    }

    @Override
    public int hashCode() {
        return Objects.hash(produto, quantidade);
    }

    /**
     * Metodo que determina se duas LinhasDeEncomenda sao iguais.
     *
     * @return boolean verdadeiro caso o produto seja igual, falso caso contrário
     * A quantidade é irrelevante neste caso na medida em que, caso precisemos de um Set de Linhas de Encomenda,
     * asseguramo-nos de que não existem produtos repetidos
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        LinhaDeEncomenda that = (LinhaDeEncomenda) o;
        return Objects.equals(produto, that.produto);
    }

    /**
     * Verifica se o produto é medicinal
     *
     * @return true caso seja, false em caso contrário
     */
    public boolean isMedicinal() {
        return this.produto.getMedicinal();
    }

    /**
     * Devolve o preço unitário do produto
     *
     * @return preço unitário do produto
     */
    public double getPreco() {
        return this.produto.getPreco();
    }
}