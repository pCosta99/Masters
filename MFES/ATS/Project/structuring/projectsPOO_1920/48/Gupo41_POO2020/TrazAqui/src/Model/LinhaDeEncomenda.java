package Model;

import java.util.prefs.PreferenceChangeListener;

public class LinhaDeEncomenda implements java.io.Serializable{
    private String codigo;
    private String descricao;
    private double peso;
    private double preco;

    /**
     * Construtor vazio de LinhaDeEncomenda
     */
    public LinhaDeEncomenda (){
        this.codigo = " ";
        this.descricao = " ";
        this.peso = 0;
        this.preco = 0;
    }

    /**
     * Construtor parametrizado de LinhaDeEncomenda
     * @param codigo
     * @param descricao
     * @param peso
     * @param preco
     */
    public LinhaDeEncomenda (String codigo, String descricao, double peso, double preco){
        this.codigo = codigo;
        this.descricao = descricao;
        this.peso = peso;
        this.preco = preco;
    }

    /**
     * Construtor por cópia de LinhaDeEncomenda
     * @param linha
     */
    public LinhaDeEncomenda (LinhaDeEncomenda linha){
        setCodigo(linha.getCodigo());
        setDescricao(linha.getDescricao());
        setPeso(linha.getPeso());
        setPreco(linha.getPreco());
    }

    /**
     * Devolve o código da LinhaDeEncomenda
     * @return
     */
    public String getCodigo(){
        return this.codigo;
    }

    /**
     * Devolve o descrição da LinhaDeEncomenda
     * @return
     */
    public String getDescricao(){
        return this.descricao;
    }

    /**
     * Devolve o peso da LinhaDeEncomenda
     * @return
     */
    public double getPeso(){
        return this.peso;
    }

    /**
     * Devolve o preço da LinhaDeEncomenda
     * @return
     */
    public double getPreco(){
        return this.preco;
    }

    /**
     * Atualiza o código da LinhaDeEncomenda
     * @param codigo
     */
    public void setCodigo(String codigo){
        this.codigo = codigo;
    }

    /**
     * Atualiza a descrição de LinhaDeEncomenda
     * @param descricao
     */
    public void setDescricao(String descricao){
        this.descricao = descricao;
    }

    /**
     * Atualiza o peso de LinhaDeEncomenda
     * @param peso
     */
    public void setPeso(double peso){
        this.peso = peso;
    }

    /**
     * Atualiza o preço de LinhaDeEncomenda
     * @param preco
     */
    public void setPreco(double preco){
        this.preco = preco;
    }

    /**
     * Devolve uma cópia da instância LinhaDeEncomenda
     * @return
     */
    public LinhaDeEncomenda clone(){
        return new LinhaDeEncomenda(this);
    }

    /**
     * Devolve uma representação textual de LinhaDeEncomenda
     * @return
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();

        sb.append("Codigo: ").append(this.codigo).append(";  ");
        sb.append("Descrição: ").append(this.descricao).append(";  ");
        sb.append("Peso: ").append(this.peso).append(";  ");
        sb.append("Preço: ").append(this.preco).append("\n");

        return sb.toString();
    }

    /**
     * Verifica a igualdade com outro objeto
     * @param o
     * @return
     */
    public boolean equals(Object o){
        if(this == o) return true;

        if((o == null) || this.getClass() != o.getClass()) return false;

        LinhaDeEncomenda l = (LinhaDeEncomenda) o;
        return (this.codigo.equals(l.getCodigo()) && this.descricao.equals(l.getDescricao()) &&
                this.peso == l.getPeso() && this.preco == l.getPreco());
    }
}
