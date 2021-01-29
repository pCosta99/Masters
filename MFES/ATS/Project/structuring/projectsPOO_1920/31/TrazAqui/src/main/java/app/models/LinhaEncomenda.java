package app.models;

import java.io.Serializable;
import java.util.Objects;

public class LinhaEncomenda implements Serializable {

    /**
    *
    */
    private static final long serialVersionUID = -5588393044508931401L;
    // #region variables
    private Produto produto;
    private double quantidade;

    // #endregion

    // #region Construtores

    public LinhaEncomenda() {
        this.produto = new Produto();
        this.quantidade = 0;
    }

    /**
     * @param produto
     * @param quantidade
     */
    public LinhaEncomenda(Produto produto, double quantidade) {
        this.setProduto(produto);
        this.quantidade = quantidade;
    }

    /**
     * @param l
     */
    public LinhaEncomenda(LinhaEncomenda l) {
        this.setProduto(l.produto);
        this.quantidade = l.quantidade;
    }

    // #endregion

    // #region getters setter
    /**
     * @return the produto
     */
    public Produto getProduto() {
        return produto.clone();
    }

    /**
     * @return the quantidade
     */
    public double getQuantidade() {
        return quantidade;
    }

    /**
     * @param quantidade the quantidade to set
     */
    public void setQuantidade(double quantidade) {
        this.quantidade = quantidade;
    }

    /**
     * @param produto the produto to set
     */
    public void setProduto(Produto produto) {
        this.produto = produto.clone();
    }

    // #endregion

    // #region Overrrides
    @Override
    public boolean equals(Object o) {
        // self check
        if (this == o) {
            return true;
        }
        // null check
        if (o == null) {
            return false;
        }
        // type check and cast
        if (getClass() != o.getClass()) {
            return false;
        }
        LinhaEncomenda linha = (LinhaEncomenda) o;
        // field comparison
        return Objects.equals(this.produto, linha.produto)
                && Objects.equals(this.quantidade, linha.quantidade);
    }

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Produto: ");
        sb.append(this.produto.toString());
        sb.append("Quantidade: ");
        sb.append(this.quantidade);
        sb.append('\n');
        return sb.toString();
    }

    @Override
    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }
    // #endregion

    // #region Methods
    // #endregion

}
