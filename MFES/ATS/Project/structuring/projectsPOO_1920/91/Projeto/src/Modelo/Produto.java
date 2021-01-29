/**
 * Classe que representa o produto que o utilizador deseja
 */
package Modelo;

import java.io.Serializable;

public class Produto implements Comparable<Produto>, Serializable {
    private String codProduto;
    private String descricao;
    private double valorUnitario;
    private boolean medico;

    //CONTRUTORES

    /**
     * Construtor nulo
     */
    public Produto(){
        this.codProduto = "Invalid";
        this.descricao = "Invalid";
        this.valorUnitario = 0;
        this.medico = false;
    }

    /**
     * Construtor Parametrizado
     * @param codProduto codigo que representa o produto
     * @param descricao descrição do produto
     * @param valorUnitario preço por unidade
     * @param medico boolean que demontra se o produto é medico, ou não
     */
    public Produto(String codProduto, String descricao, double valorUnitario , boolean medico) {
        this.codProduto = codProduto;
        this.descricao = descricao;
        this.valorUnitario = valorUnitario;
        this.medico = medico;
    }

    /**
     * Construtor cópia
     * @param outro Produto que deseja copiar
     */
    public Produto(Produto outro){
        this.codProduto = outro.getCodProduto();
        this.descricao = outro.getDescricao();
        this.valorUnitario = outro.getValorUnitario();
        this.medico = outro.isMedico();
    }

    //GETTERS E SETTERS

    /**
     * Construtor que devolve o codigo do produto
     * @return
     */
    public String getCodProduto() {
        return this.codProduto;
    }

    /**
     * Método que define o codigo do produto
     * @param codProduto
     */
    public void setCodProduto(String codProduto) {
        this.codProduto = codProduto;
    }

    /**
     * Método que devolve a descrição do produto
     * @return
     */
    public String getDescricao() {
        return this.descricao;
    }

    /**
     * Método que modifica a descrição de um produto
     * @param descricao
     */
    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    /**
     * Método que devolve o valor unitario de um produto
     * @return
     */
    public double getValorUnitario() {
        return this.valorUnitario;
    }

    /**
     * Método que permite establecer o preço por unidade de um produto
     * @param valorUnitario
     */
    public void setValorUnitario(double valorUnitario) {
        this.valorUnitario = valorUnitario;
    }

    /**
     * Método que devolve um boolean que representa se o voluntario transporta produtos medicos
     * @return
     */
    public boolean isMedico() {
        return this.medico;
    }

    /**
     * Método que modifica o parametro "medico"
     * @param medico
     */
    public void setMedico(boolean medico) {
        this.medico = medico;
    }

    /**
     * Método equals
     * @param obj
     * @return
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        Produto a = (Produto) obj;
        return this.codProduto.equals(a.getCodProduto())
                && this.descricao.equals(a.getDescricao())
                && this.valorUnitario == a.getValorUnitario()
                && this.medico == a.isMedico();
    }

    /**
     * Método clone
     * @return
     */
    @Override
    public Produto clone() {
        return new Produto(this);
    }

    /**
     * Método toString
     * @return
     */
    @Override
    public String toString() {
        return "Código de produto : " + this.codProduto
                + " Descrição : " + this.descricao
                + " Valor unitário: " + this.valorUnitario;
    }


    @Override
    public int compareTo(Produto produto) {
        return this.codProduto.compareTo(produto.codProduto);
    }
}
