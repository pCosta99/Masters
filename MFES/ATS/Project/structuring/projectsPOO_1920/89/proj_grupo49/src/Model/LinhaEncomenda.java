package Model;

import java.io.Serializable;

/**
 * Write a description of class Model.LinhaEncomenda here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */

/**
 * Classe que guarda todas a informações relativas a uma linha de encomenda
 */
public class LinhaEncomenda implements Serializable {
    private String cod;
    private String desc;
    private double quant;
    private double peso;
    private double valor;

    /**
     * Construtor sem parametros
     */
    public LinhaEncomenda() {
        this.cod = new String();
        this.desc = new String();
        this.peso =0;
        this.quant = 0;
        this.valor = 0;
    }

    /**
     * Construtor parametrizado
     * @param s     String com codigo de produto
     * @param n     String com descrição do produto
     * @param x     double com a quantidade
     * @param p     double com o peso
     * @param y     double com o preço unitario
     */
    public LinhaEncomenda(String s, String n, double x,double p, double y) {
        this.cod = s;
        this.desc = n;
        this.peso = p;
        this.quant = x;
        this.valor = y;
    }

    /**
     * Construtor por copia
     * @param u     LinhaEncomenda a copiar
     */
    public LinhaEncomenda(LinhaEncomenda u) {
        this.cod = u.getCod();
        this.desc = u.getDesc();
        this.peso = u.getPeso();
        this.quant = u.getQuant();
        this.valor = u.getValor();
    }

    /**
     * Get da variavel peso do objeto
     * @return      double com o peso
     */
    public double getPeso(){
        return this.peso;
    }

    /**
     * Get da variavel cod do objeto
     * @return      String com o cod
     */
    public String getCod() {
        return this.cod;
    }

    /**
     * Get da variavel desc do objeto
     * @return      String com a descrição
     */
    public String getDesc() {
        return this.desc;
    }

    /**
     * Get da variavel quant do objeto
     * @return      double com a quantidade
     */
    public double getQuant() {
        return this.quant;
    }

    /**
     * Get da variavel valor do objeto
     * @return      double com o valor
     */
    public double getValor() {
        return this.valor;
    }

    /**
     * Set da variavel cod do objeto
     * @param s     String com codigo
     */
    public void setCod(String s) {
        this.cod = s;
    }

    /**
     * Set da variavel desc do objeto
     * @param n     String com descrição
     */
    public void setDesc(String n) {
        this.desc = n;
    }

    /**
     * Set da variavel quant do objeto
     * @param l     double com a quantidade
     */
    public void setQuant(int l) {
        this.quant = l;
    }
    /**
     * Set da variavel quant do objeto
     * @param l     double com o valor
     */

    public void setValor(double l) {
        this.valor = l;
    }

    /**
     * Método que clona este objeto
     * @return      Clone do objeto
     */
    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }

    /**
     * Método equals do objeto
     * @param o     Objeto a comparar
     * @return      boolean
     */
    public boolean equals(Object o) {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        LinhaEncomenda u = (LinhaEncomenda) o;
        return this.cod.equals(u.getCod()) &&
                this.desc.equals(u.getDesc()) &&
                this.quant == u.getQuant() && this.valor == u.getValor();
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Model.LinhaEncomenda{");
        sb.append("cod='").append(cod).append('\'');
        sb.append(", desc='").append(desc).append('\'');
        sb.append(", quant=").append(quant);
        sb.append(", valor=").append(valor);
        sb.append('}');
        return sb.toString();
    }

    /**
     * Get preço total de uma linha
     * @return      gouble com preço
     */
    public double getPreco() {
        return quant * valor;
    }
}