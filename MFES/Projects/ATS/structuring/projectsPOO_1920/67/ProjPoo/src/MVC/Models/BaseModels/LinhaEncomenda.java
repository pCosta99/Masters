package MVC.Models.BaseModels;

import java.io.Serializable;

/**
 * Representação de uma Linha de Encomenda.
 *
 * @author 89510-89561-89501
 * @version 25/04/2020
 */
public class LinhaEncomenda implements Serializable {
    private String codigo;
    private String nome;
    private double quantidade;
    private double preco;

    /**
     * Construtor LinhaEncomenda por defeito.
     */
    public LinhaEncomenda() {
        this.codigo = "";
        this.nome = "";
        this.quantidade = 0;
        this.preco = 0;
    }

    /**
     * Construtor LinhaEncomenda parametrizado.
     * @param codigo Código do Produto.
     * @param nome Descrição do Produto.
     * @param quantidade Quantidade do Produto.
     * @param preco Preço do Produto.
     */
    public LinhaEncomenda(String codigo, String nome,double quantidade, double preco) {
        this.codigo = codigo;
        this.nome = nome;
        this.quantidade = quantidade;
        this.preco = preco;
    }

    /**
     * Construtor LinhaEncomenda por Cópia.
     * @param linha LinhaEncomenda a copiar.
     */
    public LinhaEncomenda(LinhaEncomenda linha) {
        this.codigo = linha.getCodigo();
        this.nome = linha.getNome();
        this.quantidade = linha.getQuantidade();
        this.preco = linha.getPreco();
    }

    /**
     * Método que devolve o Código do Produto.
     * @return Código do Produto.
     */
    public String getCodigo() {
        return this.codigo;
    }

    /**
     * Método que define o Código do Produto.
     * @param codigo Código do Produto.
     */
    public void setCodigo(String codigo) {
        this.codigo = codigo;
    }

    /**
     * Método que devolve a Descrição do Produto da LinhaEncomenda.
     * @return
     */
    public String getNome() {
        return this.nome;
    }

    /**
     * Método que define a Descrição do Produto da LinhaEncomenda.
     * @param nome Descrição do Produto.
     */
    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     * Método que devolve o Preço do Produto da LinhaEncomenda.
     * @return Preço do Produto.
     */
    public double getPreco() {
        return this.preco;
    }

    /**
     * Método que define o Preço do Produto da LinhaEncomenda.
     * @param preco Preço do Produto.
     */
    public void setPreco(double preco) {
        this.preco = preco;
    }

    /**
     * Método que devolve a Quantidade de Produto da LinhaEncomenda.
     * @return Quantidade de Produto.
     */
    public double getQuantidade() {
        return this.quantidade;
    }

    /**
     * Método que define a Quantidade de Produto da LinhaEncomenda.
     * @param quantidade Quantidade de Produto.
     */
    public void setQuantidade(double quantidade) {
        this.quantidade = quantidade;
    }

    /**
     * Método que Calcula o Valor Total da LinhaEncomenda.
     * @return Valor da LinhaEncomenda.
     */
    public double calculaValorLinhaEnc(){
        double valor = this.quantidade * this.preco;
        return valor;
    }

    /**
     * Método Clone.
     * @return LinhaEncomenda clonada.
     */
    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }

    /*public boolean equals(Object o) {
        if(o==this) return true;
        if(o==null || o.getClass() != this.getClass()) return false;
        LinhaEncomenda le = (LinhaEncomenda) o;
        return le.getCodigo().equals(this.codigo);
    }*/

    /**
     * Método toString.
     * @return String que contém os dados da LinhaEncomenda.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Codigo: ").append(this.codigo)
          .append(" , Produto: ").append(this.nome)
          .append(" , Quantidade: ").append(String.format("%.2f", this.quantidade))
          .append(" , Valor Unitário: ").append(String.format("%.2f",this.preco))
          .append("\n");

        return sb.toString();
    }

}