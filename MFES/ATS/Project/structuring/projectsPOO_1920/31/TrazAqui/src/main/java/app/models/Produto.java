package app.models;

import java.io.Serializable;
import java.util.Objects;

public class Produto implements Serializable {

    /**
    *
    */
    private static final long serialVersionUID = 4621560243183098840L;
    // #region variables
    private String codProduto;
    private String descricao;
    private double precoUni;

    // #endregion

    // #region Construtores

    public Produto() {
        this.codProduto = "";
        this.descricao = "";
        this.precoUni = 0;
    }

    /**
     * @param codProduto
     * @param descricao
     * @param precoUni
     */
    public Produto(String codProduto, String descricao, double precoUni) {
        this.codProduto = codProduto;
        this.descricao = descricao;
        this.precoUni = precoUni;
    }

    /**
     * @param p
     */
    public Produto(Produto p) {
        this.codProduto = p.codProduto;
        this.descricao = p.descricao;
        this.precoUni = p.precoUni;
    }

    // #endregion

    // #region getters setter

    /**
     * @return the precoUni
     */
    public double getPrecoUni() {
        return precoUni;
    }

    /**
     * @param precoUni the precoUni to set
     */
    public void setPrecoUni(double precoUni) {
        this.precoUni = precoUni;
    }

    /**
     * @return the codProduto
     */
    public String getCodProduto() {
        return codProduto;
    }

    /**
     * @param codProduto the codProduto to set
     */
    public void setCodProduto(String codProduto) {
        this.codProduto = codProduto;
    }

    /**
     * @return the descricao
     */
    public String getDescricao() {
        return descricao;
    }

    /**
     * @param descricao the descricao to set
     */
    public void setDescricao(String descricao) {
        this.descricao = descricao;
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
        Produto produto = (Produto) o;
        // field comparison
        return Objects.equals(this.codProduto, produto.codProduto)
                && Objects.equals(this.descricao, produto.descricao)
                && Objects.equals(this.precoUni, produto.precoUni);
    }

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Código: ");
        sb.append(this.codProduto);
        sb.append('\n');
        sb.append("Descricao: ");
        sb.append(this.descricao);
        sb.append('\n');
        sb.append("Preço Unitário: ");
        sb.append(this.precoUni);
        sb.append('\n');
        return sb.toString();
    }

    @Override
    public Produto clone() {
        return new Produto(this);
    }
    // #endregion

    // #region Methods
    // #endregion

}
