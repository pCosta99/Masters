package Models;

import java.io.Serializable;

public class LinhaEncomenda implements Serializable {
    private String p;
    private String nome;
    private double q;
    private double u;

    /**
     * Construtor por omissão.
     */
    public LinhaEncomenda(){
        this.p = "";
        this.nome = "";
        this.q = 0;
        this.u = 0;
    }

    /**
     * Construtor parametrizado.
     * @param p String que representa o código de um produto.
     * @param n String que representa a descrição.
     * @param q Double representante da quantidade.
     * @param u Double representante do valor unitário.
     */
    public LinhaEncomenda(String p, String n, double q, double u){
        this.p = p;
        this.nome = n;
        this.q = q;
        this.u = u;
    }

    /**
     * Construtor por cópia.
     * @param l Objeto da classe Linha de Encomenda.
     */
    public LinhaEncomenda(LinhaEncomenda l){
        this.p = l.getP();
        this.nome = l.getNome();
        this.q = l.getQ();
        this.u = l.getU();
    }

    /**
     * Método que dá o código de um produto.
     * @return Devolve o código.
     */
    public String getP(){
        return this.p;
    }

    /**
     * Método que define o código de um produto.
     * @param p Devolve a String do código.
     */
    public void setP(String p){
        this.p = p;
    }

    /**
     * Método que dá o descrição.
     * @return Devolve a String da descrição.
     */
    public String getNome(){
        return this.nome;
    }

    /**
     * Método que define o descrição.
     * @return Devolve a String da descrição.
     */
    public void setNome(String n){
        this.nome = n;
    }

    /**
     * Método que dá o o valor quantidade.
     * @return Devolve um double que representa o valor da quantidade.
     */
    public double getQ(){
        return this.q;
    }

    /**
     * Método que define o o valor quantidade.
     * @return Devolve um double que representa o valor da quantidade.
     */
    public void setQ(double q){
        this.q = q;
    }

    /**
     * Método que dá o o valor quantidade.
     * @return Devolve um double que representa o valor da quantidade.
     */
    public double getU(){
        return this.u;
    }

    /**
     * Método que define o o valor unitário.
     * @return Devolve um double que representa o valor unitário.
     */
    public void setU(double u){
        this.u = u;
    }

    /**
     * Função que verifica se o objeto recebido é idêntico ao da classe LinhaEncomenda.
     * @param o Recebe um objeto.
     * @return Devolve um boolean que corresponde à verificação.
     */
    @Override
    public boolean equals(Object o){
        if(o == this) return true;
        if(o == null || o.getClass() != this.getClass()) return false;
        LinhaEncomenda le = (LinhaEncomenda) o;
        return le.getP().equals(this.p) &&
                le.getNome().equals(this.nome) &&
                le.getQ()==(this.q) &&
                le.getU()==(this.u);
    }

    /**
     * Função que traduz a classe LinhaEncomenda.
     * @return Devolve uma String com a respetiva tradução.
     */
    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Código do produto").append(this.p)
                .append("\nDescrição").append(this.nome)
                .append("\nQuantidade").append(this.q)
                .append("\nValor Unitário").append(this.u);
        return sb.toString();
    }

    /**
     * Função que faz clone da classe LinhaEncomenda.
     * @return Devolve esse clone.
     */
    @Override
    public LinhaEncomenda clone(){
        return new LinhaEncomenda(this);
    }
}
