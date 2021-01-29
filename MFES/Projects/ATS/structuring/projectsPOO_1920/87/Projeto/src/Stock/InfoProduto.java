package Stock;

import Users.Loja;

import java.io.Serializable;

public class InfoProduto implements Serializable {
    private String nome;
    private double preco;
    private double peso;

    /**
     * Construtor por omissão.
     */
    public InfoProduto(){
        this.nome = "";
        this.preco = 0;
        this.peso = 0;
    }

    /**
     * Construtor por parâmetros.
     * @param n que é a descrição do produto.
     * @param p que é o valor unitário.
     */
    public InfoProduto(String n , double p, double peso){
        this.nome = n;
        this.preco = p;
        this.peso = peso;
    }

    /**
     * Construtor por cópia.
     * @param p que vai ser copiado.
     */
    public InfoProduto(InfoProduto p){
        this.nome = p.getNome();
        this.preco = p.getPreco();
        this.peso = p.getPeso();
    }

    /**
     * Método que devolve a descrição de um produto.
     * @return Descrição.
     */
    public String getNome() {
        return nome;
    }

    /**
     * Método que define a descrição de um produto.
     * @param nome que é a descrição.
     */
    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     * Método que devolve o valor unitário de um produto.
     * @return Valor unitário
     */
    public double getPreco() {
        return preco;
    }

    /**
     * Método que define o valor unitário de um produto.
     * @param preco que é o valor unitário.
     */
    public void setPreco(double preco) {
        this.preco = preco;
    }

    /**
     * Método que devolve o peso de um dado produto.
     * @return Peso do produto.
     */
    public double getPeso() {
        return peso;
    }

    /**
     * Método que define o peso de um dado produto.
     * @param peso que é o peso do produto.
     */
    public void setPeso(double peso) {
        this.peso = peso;
    }

    /**
     * Método que copia uma classe Stock.InfoProduto
     * @return Cópia.
     */
    public InfoProduto clone(){
        return  new InfoProduto(this);
    }

    /**
     * Método que converte numa String a informação sobre um produto.
     * @return String com a informação.
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.nome).append(",").append(this.peso).append(",").append(this.preco);
        return sb.toString();
    }

    /**
     * Método que verifica se um dado objeto é iguak a um InfoProduto.
     * @param o Objeto a ser comparado.
     * @return Resultado da comparação.
     */
    public boolean equals(Object o){
        if(o==this) return true;
        if(o==null || o.getClass() != this.getClass()) return false;
        InfoProduto info = (InfoProduto) o;
        return (this.nome.equals(info.getNome()) && this.peso == info.getPeso() && this.preco == info.getPreco());
    }
}
