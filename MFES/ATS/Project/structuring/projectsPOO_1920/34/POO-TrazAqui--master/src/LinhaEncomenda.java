/**
 * A classe LinhaEncomenda representa uma as linhas de encomenda
 * no contexto do enunciado do projecto.
 * @author grupo60
 * @version 1.0
 */

import java.io.Serializable;

public class LinhaEncomenda implements Serializable{
    
    private String codigoProduto;
    private String descricao;
    private double quantidade;
    private double preco;

    /**
     * Construtor por omissão de LinhaEncomenda.
     */
    public LinhaEncomenda() {
        this.codigoProduto = "n/a";
        this.descricao = "n/a";
        this.quantidade = 0;
        this.preco = 0;
    }
    
    /**
     * Construtor parametrizado de LinhaEncomenda.
     */
    public LinhaEncomenda(String codigoProduto, String descricao, 
        double quantidade, double preco) {
        this.codigoProduto = codigoProduto;
        this.descricao = descricao;
        this.quantidade = quantidade;
        this.preco = preco;
    }

    /**
     * Construtor de cópia de LinhaEncomenda.
     * Aceita como parâmetro outra LinhaEncomenda e utiliza os métodos
     * de acesso aos valores das variáveis de instância.
     */
    public LinhaEncomenda(LinhaEncomenda linha) {
        this.codigoProduto = linha.getCodigoProduto();
        this.descricao = linha.getDescricao();
        this.quantidade = linha.getQuantidade();
        this.preco = linha.getPreco();
    }

    /**
     * Métodos de Instância.
     */
  
    /**
     * Calcula o valor da Linha de Encomenda.
     * 
     * @return valor da Linha de Encomenda.
     */
    public double calculaValorLinhaEnc() {
        double valor = this.quantidade * this.preco;
        return valor;
    }
    
    /**
     * Devolve o valor do código de produto.
     * 
     * @return valor do código de produto.
     */
    public String getCodigoProduto() {
        return this.codigoProduto;
    }
    
    /**
     * Actualiza o valor do código de produto.
     * 
     * @param codigoProduto novo valor do código de produto.
     */
     public void setCodigoProduto(String codigoProduto) {
        this.codigoProduto = codigoProduto;
    }

    /**
     * Devolve o valor da descrição.
     * 
     * @return valor do código da descrição.
     */
    public String getDescricao() {
        return this.descricao;
    }

    /**
     * Actualiza o valor da descrição.
     * 
     * @param descricao novo valor da descrição.
     */
    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    /**
     * Devolve o valor da quantidade.
     * 
     * @return valor da quantidade.
     */
    public double getQuantidade() {
        return this.quantidade;
    }

    /**
     * Actualiza o valor da quantidade.
     * 
     * @param quantidade novo valor da quantidade.
     */
    public void setQuantidade(int quantidade) {
        this.quantidade = quantidade;
    }

    /**
     * Devolve o valor do preço.
     * 
     * @return valor do preço.
     */
    public double getPreco() {
        return this.preco;
    }

    /**
     * Actualiza o valor do preço.
     * 
     * @param preco novo valor do preço.
     */
    public void setPreco(double preco) {
        this.preco = preco;
    }

    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }
    
    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        LinhaEncomenda le = (LinhaEncomenda) obj;
        return le.getCodigoProduto().equals(this.codigoProduto) &&
              le.getDescricao().equals(this.descricao) &&
              le.getQuantidade() == this.quantidade &&
              le.getPreco() == this.preco;
    }
    
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("CodigoProduto: ").append(this.codigoProduto)
                .append("\nDescricao: ").append(this.descricao)
                .append("\nQuantidade: ").append(this.quantidade)
                .append("\nPreco: ").append(this.preco).append("\n");
        return sb.toString();
    }            
    
}
