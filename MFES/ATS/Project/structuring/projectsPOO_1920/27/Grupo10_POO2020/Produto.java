/**
 * Representacao de Linha de Encomenda
 *
 * @author Anabela Pereira - A87990, Fernando Lobo - A87988, Márcia Cerqueira - A87992; 
 * @version 20200611
 */
import java.util.*;
import java.io.Serializable;
public class Produto implements Serializable{
    private String codProduto;
    private String descricao; //nome do produto
    private double valorUnitario;
    private int quantidade; //quantidade de produtos que quero
    private double pesounitario;
    
    /**
     * Construtores
     */
    public Produto() {
        this.codProduto = "n/a";
        this.descricao = "n/a";
        this.valorUnitario = 0;
        this.quantidade = 0;
        this.pesounitario = 0;
    }
    
    public Produto(String codProduto, String descricao, double valorUnitario, int quantidade, double pesounitario) {
        this.codProduto = codProduto;
        this.descricao = descricao;
        this.valorUnitario = valorUnitario;
        this.quantidade = quantidade;
        this.pesounitario = pesounitario;
    }
    
    public Produto(Produto linha) {
        this.codProduto = linha.getCodProduto();
        this.descricao = linha.getDescricao();
        this.valorUnitario = linha.getValorUnitario();
        this.quantidade = linha.getQuantidade();
        this.pesounitario = linha.getPesounitario();
    }
  
    /**
     * Get's
     */
    public String getCodProduto() {
        return this.codProduto;
    }

    public String getDescricao() {
        return this.descricao;
    }
    
    public double getValorUnitario() {
        return this.valorUnitario;
    }
    
    public int getQuantidade() {
        return this.quantidade;
    }
    
    public double getPesounitario(){
        return this.pesounitario;
    }
    
    public void setCodProduto(String codProduto) {
        this.codProduto = codProduto;
    }
    
    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    public void setValorUnitario(double valorUnitario) {
        this.valorUnitario = valorUnitario;
    }

    public void setQuantidade(int quantidade) {
        this.quantidade = quantidade;
    }
    

    public void setPesounitario(double pesounitario){
        this.pesounitario = pesounitario;
    }
    
    /**
     * clone
     */
    public Produto clone() {
        return new Produto(this);
    }
    
    /**
     * equals
     */
    public boolean equals(Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Produto le = (Produto) obj;
        return le.getCodProduto().equals(this.codProduto);
    }
  
    /**
     * toString
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Referencia: ").append(this.codProduto);
        sb.append("Descrição: ").append(this.descricao);
        sb.append("Peso: ").append(this.pesounitario);
        return sb.toString();
   }
    
    public int compareTo(Produto x){
        return this.codProduto.compareTo(x.getCodProduto());  
   }
    
    /**
     * Calcula o peso total do produto
     */
    public double pesoTotal(){
        double peso = (double)this.pesounitario * this.quantidade;
        return peso;
   }
    
   /**
    * Calcula valor da encomenda
    */
   public double calculaPreco() {
        double valor = this.quantidade * this.valorUnitario;
        return valor;
   }
}
