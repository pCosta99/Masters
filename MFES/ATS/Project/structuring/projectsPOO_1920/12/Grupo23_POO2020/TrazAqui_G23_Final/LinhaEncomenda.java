import java.io.*;

/**
 * Classe LinhaEncomenda :: Info específica de um dado artigo encomendado.
 */
public class LinhaEncomenda implements Serializable
{
    private String cod;
    private String descricao;
    private double quantidade; //em kgs
    private double preco; // cêntimos por kg
    
    
    public LinhaEncomenda() {
        this.cod = new String();
        this.descricao = new String();
        this.preco = 0;
        this.quantidade = 0;
    }
    
    public LinhaEncomenda(String cod, String descricao, double quantidade, double valor) {
        this.cod = cod;
        this.descricao = descricao;
        this.quantidade = quantidade;
        this.preco = valor;
    }
    
    public LinhaEncomenda(LinhaEncomenda linha) {
        this.cod = linha.getCod();
        this.descricao = linha.getDescricao();
        this.preco = linha.getPreco();
        this.quantidade = linha.getQuantidade();
    }
    
    
    public double calculaValorLinhaEnc() {
        return (this.quantidade * this.preco)/100;
    }
    
    /**
     * Métodos get
     */
    public String getCod() {
        return this.cod;
    }
    
    public String getDescricao() {
        return this.descricao;
    }
    
    public double getPreco() {
        return this.preco;
    }
    
    public double getQuantidade() {
        return this.quantidade;
    }
    
    /**
     * Métos set
     */
    public void setCod(String cod) {
        this.cod = cod;
    }
 
    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    public void setPreco(double preco) {
        this.preco = preco;
    }

    public void setQuantidade(int quantidade) {
        this.quantidade = quantidade;
    }
    
    
    /**
     * Clone
     */
    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }
    
    /**
     * Equals
     */
    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        LinhaEncomenda le = (LinhaEncomenda) obj;
        return (le.getCod().equals(this.cod) &&
               le.getDescricao().equals(this.descricao) && 
               (le.getPreco() == this.preco) &&
               (le.getQuantidade() == this.quantidade));
    }
    
    /**
     * toString
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Codigo: ").append(this.cod);
        sb.append(" / Descricao: ").append(this.descricao);
        sb.append(" / Quantidade: ").append(this.quantidade);
        sb.append(" / Preco: ").append(this.preco);
        
        return sb.toString();
    }
    
    /**
     * toFile
     */
    public String stringtoFile(){
        StringBuilder sb = new StringBuilder();
        
        sb.append(this.getCod());
        sb.append(","+this.getDescricao());
        sb.append(","+this.getQuantidade());
        sb.append(","+this.getPreco());
    
        return sb.toString();
    }
}
