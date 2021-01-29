package MVC.Models.BaseModels;

import java.io.Serializable;

public class Produto implements Serializable {
    private String cod;
    private String nome;
    private double precoPorQuant;
    private boolean medicamento;

    /**
     * Construtor de Produto parametrizado.
     * @param c Código do Produto.
     * @param n Descrição do Produto.
     * @param p Preço por Quantidade do Produto.
     */
    public Produto(String c, String n, double p){
        this.cod = c;
        this.nome = n;
        this.precoPorQuant = p;
        this.medicamento = false;
    }

    /**
     * Construtor de Produto por Cópia.
     * @param p Produto a copiar.
     */
    public Produto(Produto p){
        this.cod = p.getCod();
        this.nome = p.getNome();
        this.precoPorQuant = p.getPrecoPorQuant();
        this.medicamento = p.isMedicamento();
    }

    /**
     * Método que retorna o Código do Produto.
     * @return Código do Produto.
     */
    public String getCod() {
        return cod;
    }

    /**
     * Método que retorna a Descrição do Produto.
     * @return Descrição do Produto.
     */
    public String getNome() {
        return nome;
    }

    /**
     * Método que define a Descrição do Produto.
     * @param nome Descrição do Produto.
     */
    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     * Método que retorna o Preço por Quantidade do Produto.
     * @return Preço por Quantidade do Produto.
     */
    public double getPrecoPorQuant() {
        return precoPorQuant;
    }

    /**
     * Método que devolve se um Produto é um Medicamento.
     * @return True caso seja, false caso contrário.
     */
    public boolean isMedicamento() {
        return this.medicamento;
    }

    /**
     * Método que define se um Produto é um Medicamento.
     * @param medicamento True caso seja, false caso contrário.
     */
    public void setMedicamento(boolean medicamento) {
        this.medicamento = medicamento;
    }

    /**
     * Método equals.
     * @param o Objecto a comparar.
     * @return True caso sejam iguais, false caso contrário.
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Produto produto = (Produto) o;
        return this.cod.equals( produto.cod);
    }

    /**
     * Método toString.
     * @return String com o dados do Produto.
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("\nCodigo: ").append(this.cod).append("\nProduto: ").append(this.nome).append("\nPreco por quantidade: ").append(this.precoPorQuant);
        return sb.toString();
    }

    /**
     * Método Clone.
     * @return Produto clonado.
     */
    public Produto clone(){
        return new Produto(this);
    }
}
