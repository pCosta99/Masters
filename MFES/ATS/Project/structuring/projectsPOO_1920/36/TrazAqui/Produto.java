
/**
 * 
 *
 * @author Artur Drohobytskyy
 * @version 1.0
 */
import java.io.Serializable;

public class Produto implements Serializable {
    private String codigoProduto;
    private String descricao;
    private double quantidade;
    private double valorUnitario;
    
    public Produto() {
        this.codigoProduto = "";
        this.descricao = "";
        this.quantidade = 1;
        this.valorUnitario = 1;
    }
    
    public Produto(String codigoProduto, String descricao, double quantidade, double valorUnitario) {
        this.codigoProduto = codigoProduto;
        this.descricao = descricao;
        this.quantidade = quantidade;
        this.valorUnitario = valorUnitario;
    }
    
    public Produto(Produto p) {
        this.codigoProduto = p.getCodigoProduto();
        this.descricao = p.getDescricao();
        this.quantidade = p.getQuantidade();
        this.valorUnitario = p.getValorUnitario();
        
    }
    
    public String getCodigoProduto() {
        return this.codigoProduto;
    }
    
    public String getDescricao() {
        return this.descricao;
    }
    
    public double getQuantidade () {
        return this.quantidade;
    }
    
    public double getValorUnitario() {
        return this.valorUnitario;
    }
    
    public void setCodigoProduto(String codigoProduto) {
        this.codigoProduto = codigoProduto;
    }
    
    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }
    
    public void setQuantidade(double quantidade) {
        this.quantidade = quantidade;
    }
    
    public void setValorUnitario(double valorUnitario) {
        this.valorUnitario = valorUnitario;
    }
    
    public boolean equals(Object obj) {
        if(this == obj) return true;
        if((obj == null) || (this.getClass() != obj.getClass())) return false;
        Produto p = (Produto) obj;
        return this.codigoProduto.equals(p.getCodigoProduto()) && 
               this.descricao.equals(p.getDescricao()) &&
               this.quantidade == p.getQuantidade() &&
               this.valorUnitario == p.getValorUnitario();
    }
    
    public String toString() {
        return "Codigo produto: " + this.codigoProduto + "\n" + 
               "Descricao: " + this.descricao + "\n" + 
               "Quantidade: " + this.quantidade + "\n" +
               "Valor unitário: " + this.valorUnitario;
    }
    
    public Produto clone() {
        return new Produto(this);
    }
   
}
