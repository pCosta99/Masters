import java.io.Serializable;

/**
 * Classe que lida com a informação de uma LinhaEncomenda.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */
public class LinhaEncomenda implements Serializable {
    // Instance Variables
    private String codProduto;
    private String descricao;
    private double quant;
    private double valorUnit;

    // Constructors

    /**
     * Construtor de uma LinhaEncomenda.
     */
    public LinhaEncomenda() {
        this.codProduto = "n/d";
        this.descricao  = "n/d";
        this.quant = 0.0;
        this.valorUnit = 0.0;
    }

    /**
     * Construtor de uma LinhaEncomenda.
     * @param codProduto, Codigo do produto da LinhaEncomenda a construir.
     * @param descricao, Descrição da LinhaEncomenda a construir.
     * @param quant, Quantidade da LinhaEncomenda a construir.
     * @param valorUnit, Valor unitário da LinhaEncomenda a construir.
     */
    public LinhaEncomenda(String codProduto, String descricao, double quant, double valorUnit) {
        this.codProduto = codProduto;
        this.descricao = descricao;
        this.quant = quant;
        this.valorUnit = valorUnit;
    }

    /**
     * Construtor de uma LinhaEncomenda.
     * @param ol, LinhaEncomenda a construir.
     */
    public LinhaEncomenda(LinhaEncomenda ol) {
        this.codProduto = ol.getCodProduto();
        this.descricao = ol.getDescricao();
        this.quant = ol.getQuant();
        this.valorUnit = ol.getValorUnit();
    }

    // Gets

    /**
     * Função que retorna o código do produto de uma linha de encomenda.
     * @return Código do produto.
     */
    public String getCodProduto() {
        return this.codProduto;
    }

    /**
     * Função que retorna a descrição de uma linha de encomenda.
     * @return Descrição.
     */
    public String getDescricao() {
        return this.descricao;
    }

    /**
     * Função que retorna a quantidade de uma linha de encomenda.
     * @return Quantidade.
     */
    public double getQuant() {
        return this.quant;
    }

    /**
     * Função que retorna o valor unitário de uma linha de encomenda.
     * @return Valor Unitário.
     */
    public double getValorUnit() {
        return this.valorUnit;
    }

    /**
     * Função que cria um clone de uma LinhaEncomenda.
     * @return LinhaEncomenda clonada.
     */
    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }

    /**
     * Função que compara parametros da LinhaEncomenda.
     * @param o, Objeto a comparar.
     * @return 'true' se forem iguais.
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        LinhaEncomenda ol = (LinhaEncomenda) o;
        return ol.getCodProduto().equals(this.codProduto) &&
               ol.getDescricao().equals(this.descricao) &&
               ol.getQuant() == this.quant &&
               ol.getValorUnit() == this.valorUnit;
    }

    /**
     * Função que converte os parametros da LinhaEncomenda para String.
     * @return String com os parametros da LinhaEncomenda.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Codigo: ").append(this.codProduto).append("; ");
        sb.append("Descricao: ").append(this.descricao).append("; ");
        sb.append("Quantidade: ").append(this.quant).append("; ");
        sb.append("Valor unidade: ").append(this.valorUnit);
        return sb.toString();
    }
}
