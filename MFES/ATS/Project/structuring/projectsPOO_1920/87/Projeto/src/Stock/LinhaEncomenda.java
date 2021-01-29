package Stock;

import java.io.Serializable;

public class LinhaEncomenda implements Serializable {
    private String codProduto;
    private String descricao;
    private double quantidade;
    private double valorUnitario;

    /**
     * Construtor por omissão.
     */
    public LinhaEncomenda() {
        this.codProduto = "";
        this.descricao = "";
        this.quantidade = 0;
        this.valorUnitario = 0;
    }

    /**
     * Construtor por parâmetros.
     * @param codProduto Código de produto.
     * @param descricao Descrição do produto.
     * @param quantidade Quantidade do produto.
     * @param valorUnitario Preço por unidade do produto.
     */
    public LinhaEncomenda(String codProduto, String descricao, double quantidade, double valorUnitario) {
        this.codProduto = codProduto;
        this.descricao = descricao;
        this.quantidade = quantidade;
        this.valorUnitario = valorUnitario;
    }

    /**
     * Construtor por cópia.
     * @param l LinhaEncomenda a ser copiada.
     */
    public LinhaEncomenda(LinhaEncomenda l) {
        this.codProduto = l.getCodProduto();
        this.descricao = l.getDescricao();
        this.quantidade = l.getQuantidade();
        this.valorUnitario = l.getValorUnitario();
    }

    /**
     * Método que devolve o código de um produto
     * @return Código do produto.
     */
    public String getCodProduto() {
        return this.codProduto;
    }

    /**
     * Método que define o código de um produto.
     * @param codProduto que é o código.
     */
    public void setCodProduto(String codProduto) {
        this.codProduto = codProduto;
    }

    /**
     * Método que devolve a descrição de um produto.
     * @return Descrição do produto.
     */
    public String getDescricao() {
        return this.descricao;
    }

    /**
     * Método que define a descrição de um produto.
     * @param descricao que é a descrição.
     */
    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    /**
     * Método que devolve a quantidade de um produto.
     * @return Quantidade do produto.
     */
    public double getQuantidade() {
        return this.quantidade;
    }

    /**
     * Método que define a quantidade de um produto.
     * @param quantidade que é a quantidade.
     */
    public void setQuantidade(double quantidade) {
        this.quantidade = quantidade;
    }

    /**
     * Método que devolve o valor unitário de um produto.
     * @return Valor unitário.
     */
    public double getValorUnitario() {
        return this.valorUnitario;
    }

    /**
     * Método que define o valor unitário de um produto.
     * @param valorUnitario que é o valor unitário.
     */
    public void setValorUnitario(double valorUnitario) {
        this.valorUnitario = valorUnitario;
    }

    /**
     * Método que averigua se um dado objeto é igual a uma dada LinhaEncomenda.
     * @param o Objeto a ser avaliado.
     * @return Resultado da comparação.
     */
    public boolean equals(Object o) {
        if (o == this) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        LinhaEncomenda l = (LinhaEncomenda) o;
        return (l.getCodProduto().equals(this.codProduto) &&
                l.getDescricao().equals(this.descricao) &&
                l.getQuantidade() == (this.quantidade) &&
                l.getValorUnitario() == (this.valorUnitario));
    }

    /**
     * Método que converte numa String a informação sobre uma LinhaEncomenda.
     * @return String com a informação.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(this.codProduto).append(",").append(this.descricao).append(",").append(this.quantidade).append(",").append(this.valorUnitario);
        return sb.toString();
    }

    /**
     * Método que copia uma LinhaEncomenda.
     * @return Cópia.
     */
    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }
}