/**
 * Classe que representa uma linha de encomenda
 */
package Modelo;

import java.io.Serializable;

public class LinhaEncomenda implements Serializable {
    private String codProduto;
    private String descricao;
    private double quantidade;
    private double valorUnitario;
    private double peso;
    private boolean isMedico;

    /**
     * Contrutor vazio
     */
    public LinhaEncomenda (){
        this.codProduto = "Invalid";
        this.descricao = "Invalid";
        this.quantidade = 0;
        this.valorUnitario = 0 ;
        this.peso = 0;
    }

    /**
     * Contrutor parametrizado
     * @param codProduto codigo do produto
     * @param descricao nome do produto
     * @param quantidade quantidade de produtos
     * @param valorUnitario preço do produto por unidade
     * @param peso peso
     * @param isMedico boolean que representa se o produto é medico ou não
     */
    public LinhaEncomenda(String codProduto, String descricao, double quantidade, double valorUnitario, double peso, boolean isMedico ) {
        this.codProduto = codProduto;
        this.descricao = descricao;
        this.quantidade = quantidade;
        this.valorUnitario = valorUnitario;
        this.peso = peso;
        this.isMedico = isMedico;
    }

    /**
     *Construtor cópia
     * @param outro
     */
    public LinhaEncomenda( LinhaEncomenda outro){
        this.codProduto = outro.getCodProduto();
        this.descricao = outro.getDescricao();
        this.quantidade = outro.getQuantidade();
        this.valorUnitario = outro.getValorUnitario();
        this.peso = outro.getPeso();
        this.isMedico = outro.isMedico();
    }

    /**
     * Método que devolve o codigo do produto
     * @return
     */
    public String getCodProduto() {
        return this.codProduto;
    }

    /**
     * Método que modifica o codigo do produto
     * @param codProduto
     */
    public void setCodProduto(String codProduto) {
        this.codProduto = codProduto;
    }

    /**
     * Método que devolve a descrição(nome) do produto
     * @return
     */
    public String getDescricao() {
        return this.descricao;
    }

    /**
     * Método que define a descrição da linha encomenda
     * @param descricao
     */
    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    /**
     * Método que devolve a quantidade do produto na linha encomenda
     * @return
     */
    public double getQuantidade() {
        return this.quantidade;
    }

    /**
     * Método que modifica a quantidade do produto numa linha encomenda
     * @param quantidade
     */
    public void setQuantidade(double quantidade) {
        this.quantidade = quantidade;
    }

    /**
     * Método que devolve o preço por unidade
     * @return
     */
    public double getValorUnitario() {
        return this.valorUnitario;
    }

    /**
     * Método que modifica o preço por unidade
     * @param valorUnitario
     */
    public void setValorUnitario(double valorUnitario) {
        this.valorUnitario = valorUnitario;
    }

    /**
     * Método que devolve o peso do produto
     * @return
     */
    public double getPeso() {
        return peso;
    }

    /**
     * Metodo que devolve true se o produto for medico
     * @return
     */
    public boolean isMedico() {
        return isMedico;
    }

    /**
     * Método que define o peso
     * @param peso
     */
    public void setPeso(double peso) {
        this.peso = peso;
    }

    /**
     * Método que modifica se a linha encomenda é medica ou não
     * @param medico
     */
    public void setMedico(boolean medico) {
        isMedico = medico;
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
        LinhaEncomenda a = (LinhaEncomenda) obj;
        return this.codProduto.equals(a.getCodProduto())
                && this.descricao.equals(a.getDescricao())
                && this.quantidade == a.getQuantidade()
                && this.valorUnitario == a.getValorUnitario();
    }

    /**
     * Método que devolve o preço total da linha de encomenda
     * @return
     */
    public double precoLinhadeEncomenda(){
        return this.valorUnitario * this.quantidade;
    }

    /**
     * Método clone
     * @return
     */
    @Override
    protected LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }

    /**
     * Método toString
     * @return
     */
    @Override
    public String toString() {
        return "Código do produto: " + this.codProduto
                + " Descrição: " + this.descricao
                + " Quantidade: " + this.quantidade
                + " Valor unitário "  + this.valorUnitario + "\n";
    }
}
