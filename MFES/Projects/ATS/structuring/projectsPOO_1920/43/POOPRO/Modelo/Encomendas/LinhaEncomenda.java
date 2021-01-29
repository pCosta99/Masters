package Modelo.Encomendas;

import Modelo.Produtos.Produto;

import java.io.Serializable;
import java.util.Objects;

public class LinhaEncomenda implements Serializable {

    /**
     * VARIÁVEIS DE INSTÂNCIA
     */

    private Produto produto;
    private int quantidade;

    /**
     * CONSTRUTOR VAZIO
     */

    public LinhaEncomenda() {
        this.produto = new Produto();
        this.quantidade = 0;
    }

    /**
     * CONSTRUTOR PARAMETRIZADO
     */

    public LinhaEncomenda(Produto nProduto, int nQuantidade) {
        this.produto = nProduto.clone();
        this.quantidade = nQuantidade;
    }

    /**
     * CONSTRUTOR POR CÓPIA
     */

    public LinhaEncomenda(LinhaEncomenda nLinhaEncomenda) {
        this.produto = nLinhaEncomenda.getProduto();
        this.quantidade = nLinhaEncomenda.getQuantidade();
    }


    /**
     * GETTERS
     */

    public Produto getProduto() {
        return this.produto.clone();
    }

    public int getQuantidade() {
        return this.quantidade;
    }

    /**
     * SETTERS
     */

    public void setProduto(Produto nProduto) {
        this.produto = nProduto.clone();
    }

    public void setQuantidade(int quantidade) {
        this.quantidade = quantidade;
    }

    /**
     * MÉTODO CLONE
     */

    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }

    /**
     * MÉTODO EQUALS
     */

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        LinhaEncomenda that = (LinhaEncomenda) o;
        return quantidade == that.quantidade &&
                Objects.equals(produto, that.produto);
    }

    /**
     * MÉTODO TOSTRING
     */

    public String toString() {
        return "LinhaEncomenda{" +
                "produto=" + produto +
                ", quantidade=" + quantidade +
                '}';
    }
}
