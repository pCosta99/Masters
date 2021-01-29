import java.io.Serializable;
import java.util.Objects;


public class Produto implements Serializable {
    private String codProduto;
    private String descricao;
    private double quantidade;
    private double precoUnitario;

    /**
     * Construtores
     */

    /**
     * Omissão
     */
    public Produto() {
        this.codProduto = "n/a";
        this.descricao = "n/a";
        this.quantidade = 0;
        this.precoUnitario = 0;
    }

    /**
     * Parametrizado
     */
    public Produto(String codProduto, String descricao, double quantidade, double precoUnitario) {
        this.codProduto = codProduto;
        this.descricao = descricao;
        this.quantidade = quantidade;
        this.precoUnitario = precoUnitario;
    }

    /**
     * Cópia
     */
    public Produto(Produto linha) {
        this.codProduto = linha.getReferencia();
        this.descricao = linha.getDescricao();
        this.quantidade = linha.getQuantidade();
        this.precoUnitario = linha.getPreco();
    }

    /**
     * Gets
     */
    public String getReferencia() {
        return this.codProduto;
    }

    public String getDescricao() {
        return this.descricao;
    }

    public double getQuantidade() {
        return this.quantidade;
    }

    public double getPreco() {
        return this.precoUnitario;
    }



    /**
     * Sets
     */
    public void setReferencia(String codProduto) {
        this.codProduto = codProduto;
    }

    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    public void setQuantidade(double quantidade) {
        this.quantidade = quantidade;
    }

    public void setPreco(double precoUnitario) {
        this.precoUnitario = precoUnitario;
    }



    /**
     * Manipulação
     */

    public Produto clone() {
        return new Produto(this);
    }

    /**
     * Metodo equals
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Produto produto = (Produto) o;
        return quantidade == produto.quantidade &&
                Double.compare(produto.precoUnitario, precoUnitario) == 0 &&
                Objects.equals(codProduto, produto.codProduto) &&
                Objects.equals(descricao, produto.descricao);
    }

    /**
     * Metodo hashcode
     */
    public int hashCode() {
        return Objects.hash(codProduto, descricao, quantidade, precoUnitario);
    }

    /**
     * Metodo toString
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("\nReferencia: ").append(this.codProduto).append(",");
        sb.append("\nDescrição: ").append(this.descricao).append(",");
        sb.append("\nQuantidade: ").append(this.quantidade).append(",");
        sb.append("\nPreço unitário: ").append(this.precoUnitario).append("\n");

        return sb.toString();
    }

    /**
     * Funções
     */
    public double valorTotal() {
        double valor = this.quantidade * this.precoUnitario;
        return valor;
    }
    
}
