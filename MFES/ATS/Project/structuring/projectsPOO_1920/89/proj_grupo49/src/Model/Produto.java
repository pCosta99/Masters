package Model;

import java.io.Serializable;

/**
 * Clase responsavel toda a informação relativa a um produto
 */
public class Produto implements Comparable<Produto>, Serializable {
            private String cod;
            private String nome;
            private boolean medico;
            private double peso;
            private double preçouni;

    /**
     * Construtor sem parametros
     */
    public Produto(){
                this.cod = new String();
                this.nome = new String();
                this.medico = false;
                this.peso = 0;
                this.preçouni = 0;
            }

    /**
     * Construtor parametrizado
     * @param c     String com codigo de produto
     * @param n     String com descrição do produto
     * @param med   boolean que verifica se é medico
     * @param peso  double com peso
     * @param p     double com preço unitario
     */
            public Produto(String c, String n,boolean med, double peso, double p){
                this.cod = c;
                this.nome = n;
                this.medico = med;
                this.peso = peso;
                this.preçouni = p;
    }

    /**
     * Get da variavel peso do objeto
     * @return      double com peso
     */
    public double getPeso() {
        return peso;
    }

    /**
     * Set da variavel peso do objeto
     * @param peso      double com peso
     */
    public void setPeso(double peso) {
        this.peso = peso;
    }

    /**
     * Get da variavel cod do objeto
     * @return      String com codigo
     */
    public String getCod() {
        return cod;
    }

    /**
     * Set da variavel cod do objeto
     * @param cod       String com o codigo
     */
    public void setCod(String cod) {
        this.cod = cod;
    }

    /**
     * Get da variavel nome do objeto
     * @return      String com nome
     */
    public String getNome() {
        return nome;
    }

    /**
     * Set da variavel nome do objeto
     * @param nome      String com nome
     */
    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     * Get da variavel preço do objeto
     * @return      double com preço
     */
    public double getPreçouni() {
        return preçouni;
    }

    /**
     * Set da variavel preço do objeto
     * @param preçouni      double com o preço
     */
    public void setPreçouni(double preçouni) {
        this.preçouni = preçouni;
    }

    /**
     * Método que compara este objeto com outro produto pelo codigo
     * @param p1    Produto a comparar
     * @return      int
     */
    public int compareTo(Produto p1){
        return cod.compareTo(p1.getCod());
    }

    /**
     * Get da variavel medico do objeto
     * @return      boolean
     */
    public boolean getMedico(){
           return this.medico;
    }

    /**
     * Set da variavel medico do objeto
     * @param s     boolean
     */
    public void setMedico(boolean s){
           this.medico = s;
    }
}