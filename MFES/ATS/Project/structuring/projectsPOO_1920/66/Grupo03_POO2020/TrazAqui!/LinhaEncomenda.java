import java.io.Serializable;
import java.util.ArrayList;
import java.util.stream.Collectors;

/**
 * Classe que contém as informações de uma linha de encomenda
 */
public class LinhaEncomenda implements Serializable {
    private String codProduto;
    private String descricao;
    private double quantidade;
    private double valorUnitario;

    /**
     * Contrutor vazio
     */
    public LinhaEncomenda() {
        this.codProduto = "";
        this.descricao = "";
        this.quantidade = 0;
        this.valorUnitario = 0;
    }

    /**
     * Contrutor com argumentos
     * @param c Código do Produto
     * @param d Descrição
     * @param q Quantidade
     * @param v Valor unitário
     */
    public LinhaEncomenda(String c, String d, double q, double v) {
        this.codProduto = c;
        this.descricao = d;
        this.quantidade = q;
        this.valorUnitario = v;
    }

    /**
     * Contrutor com uma LinhaEncomenda
     * @param l LinhaEncomenda
     */
    public LinhaEncomenda(LinhaEncomenda l) {
        this.codProduto = l.codProduto;
        this.descricao = l.descricao;
        this.quantidade = l.quantidade;
        this.valorUnitario = l.valorUnitario;
    }

    /**
     * Devolve o Código do Produto
     * @return Código do Produto
     */
    public String getCodProd () {return this.codProduto;}

    /**
     * Devolve a Descrição
     * @return Descrição
     */
    public String getDescricao () {return this.descricao;}

    /**
     * Devolve a quantidade
     * @return Quantidade
     */
    public double getQuantidade () {return this.quantidade;}

    /**
     * Devolve o Valor Unitário
     * @return Valor Unitário
     */
    public double getValorUnitario () {return this.valorUnitario;}

    /**
     * Introduz o Código do Produto
     * @param c Código do Produto
     */
    public void setCodProd (String c) {this.codProduto = c;}

    /**
     * Introduz a Descrição
     * @param c Descrição
     */
    public void setDescricao (String c) {this.descricao = c;}

    /**
     * Introduz a quantidade
     * @param c Quantidade
     */
    public void setQuantidade (Double c) {this.quantidade = c;}

    /**
     * Introduz o Valor Unitário
     * @param c Valor Unitário
     */
    public void setValorUnitario (double c) {this.valorUnitario = c;}

    /**
     * Método equals
     * @param o Object
     * @return true se as Linhas de encomenda forem iguais ou false caso contrário
     */
    public boolean equals (Object o) {
        if (this == o) return true;
        if (o == null || this.getClass()!= o.getClass()) return false;
        LinhaEncomenda p = (LinhaEncomenda) o;
        return (this.codProduto.equals(p.getCodProd()) && this.descricao.equals(p.getDescricao())
                && this.quantidade == p.getQuantidade() && this.valorUnitario == p.getValorUnitario());
    }

    /**
     * Método clone
     * @return Clone de uma Linha de encomenda
     */
    public LinhaEncomenda clone () { return new LinhaEncomenda(this); }

    /**
     * Método toString
     * @return String com informação relativa a uma Linha de encomenda
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Código do produto: ").append(this.codProduto).append(", Descrição: ").append(this.descricao)
                .append(", Quantidade: ").append(this.quantidade)
                .append(", Valor unitário: ").append(this.valorUnitario).append("|");
        return sb.toString();
    }

    /**
     * Método que calcula o preço de uma linha de encomenda
     * @return preço de uma linha de encomenda
     */
    public double getPrecoLinha () {
        return this.quantidade * this.valorUnitario;
    }
}
