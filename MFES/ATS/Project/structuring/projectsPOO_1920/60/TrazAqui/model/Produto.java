package model;

import interfaces.IProduto;

import java.io.Serializable;

/**
 * Classe que implementa os produtos
 */
public class Produto implements IProduto, Serializable {
    private final String code;
	private final String produto;
	private double qtd;
    private double valor;
    private double peso;
    private final boolean medicine;

    /**
     * Construtor do produto
     * @param code código
     * @param prod produto
     * @param qtd quantidade
     * @param v valor
     * @param p peso
     * @param m é medicamento?
     */
    public Produto(String code, String prod, double qtd, double v, double p, boolean m){
        this.code = code;
        this.produto = prod;
        this.qtd = qtd;
        this.valor = v;
    	this.peso = p;
    	this.medicine = m;
    }

    /**
     * Construtor de produto
     * @param p produto
     */
    public Produto(Produto p){
        this.code = p.getCode();
        this.produto = p.getProduto();
    	this.valor = p.getValor();
    	this.peso = p.getPeso();
    	this.medicine = p.getMedicine();
        this.qtd = p.getQtd();
    }

    /**
     * Devolve o código
     * @return código
     */
    public String getCode() {
        return this.code;
    }

    /**
     * Devolve a quantidade
     * @return quantidade
     */
    public double getQtd() {
        return this.qtd;
    }

    /**
     * Devolve o produto
     * @return produto
     */
    public String getProduto() {
        return this.produto;
    }

    /**
     * Devolve o valor
     * @return valor
     */
    public double getValor() {
        return this.valor;
    }

    /**
     * Devolve o peso
     * @return peso
     */
    public double getPeso() {
        return this.peso;
    }

    /**
     * Devolve se é medicamento
     * @return se é medicamento
     */
    public boolean getMedicine() {
        return this.medicine;
    }

    /**
     * @return clone de um produto
     */
    public Produto clone() {
        return new Produto(this);
    }

    /**
     * @return representa um produto em string
     */
    public String toString() {
        StringBuilder s = new StringBuilder();

        s.append(":\n                Produto: ").append(this.produto);
        s.append("\n                Valor: ").append(this.valor);
        s.append("\n                Med? ").append(this.medicine);
        s.append("\n");

        return s.toString();
    }

    /**
     * Atualiza os valores
     * @param q quantidade
     * @param valorUnid valor unidade
     * @param pesoUnid peso unidade
     */
    public void update(double q, double valorUnid, double pesoUnid){
        this.qtd = this.qtd + q;
        this.valor = this.valor + (q * valorUnid);
        this.peso = this.peso + (q * pesoUnid);
    }


}
