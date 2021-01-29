package Modelo;

import java.io.Serializable;
import java.util.Objects;

/**
 * Classe que contém a implementação da estrutura de LinhaEncomenda
 */
public class LinhaEncomenda implements Serializable {

    private String codProduto;
    private String descricao;
    private double pesoU;
    private double precoU;


    // --------------------------- Constructor -------------------------

    /**
     * Construtor por omissão
     */
    public LinhaEncomenda(){
        this.codProduto = "";
        this.descricao = "";
        this.pesoU = 0;
        this.precoU = 0;
    }

    /**
     * Construtor parametrizado
     * @param codProduto            Codigo
     * @param descricao             Descrição
     * @param pesoU                 Peso unidade
     * @param precoU                Preço unidade
     */
    public LinhaEncomenda(String codProduto, String descricao,double pesoU, double precoU) {
        this.codProduto = codProduto;
        this.descricao = descricao;
        this.pesoU  = pesoU;
        this.precoU = precoU;
    }

    /**
     * Construtor por cópia
     * @param l             Linha de encomenda
     */
    public LinhaEncomenda(LinhaEncomenda l){
        this.codProduto = l.getCodProduto();
        this.descricao = l.getDescricao();
        this.pesoU = l.getPesoU();
        this.precoU = l.getPrecoU();
    }

    // --------------------------- Getters & Setters -------------------------

    /**
     * Devolve o codigo do produto
     * @return String
     */
    public String getCodProduto() {
        return codProduto;
    }

    /**
     * Devolve a descrição
     * @return String
     */
    public String getDescricao() {
        return descricao;
    }

    /**
     * Devolve o peso por unidade
     * @return double
     */
    public double getPesoU() {
        return pesoU;
    }

    /**
     * Devolve o preço por unidade
     * @return double
     */
    public double getPrecoU() {
        return precoU;
    }

     // --------------------------- Auxiliaries -------------------------

    /**
     * Devolve uma cópia da instância
     * @return LinhaEncomenda       this
     */
    public LinhaEncomenda clone(){
        return new LinhaEncomenda(this);
    }

    /**
     * Verifica a igualdade com outro objeto.
     * @param o          Objeto a comparar
     * @return boolean
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LinhaEncomenda)) return false;
        LinhaEncomenda that = (LinhaEncomenda) o;
        return Double.compare(that.getPesoU(), getPesoU()) == 0 &&
                Double.compare(that.getPrecoU(), getPrecoU()) == 0 &&
                Objects.equals(getCodProduto(), that.getCodProduto()) &&
                Objects.equals(getDescricao(), that.getDescricao());
    }

    /**
     * Método hashCode do objeto
     * @return hash do objeto
     */
    @Override
    public int hashCode() {
        return Objects.hash(getCodProduto(), getDescricao(), getPesoU(), getPrecoU());
    }

    /**
     * Método toString do objeto
     * @return Objeto em modo string
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Modelo.LinhaEncomenda{");
        sb.append("codProduto='").append(codProduto).append('\'');
        sb.append(", descricao='").append(descricao).append('\'');
        sb.append(", pesoU=").append(pesoU);
        sb.append(", precoU=").append(precoU);
        sb.append('}');
        return sb.toString();
    }
}

