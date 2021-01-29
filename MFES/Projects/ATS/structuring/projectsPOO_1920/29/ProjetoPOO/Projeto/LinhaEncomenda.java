import java.io.Serializable;
public class LinhaEncomenda implements Serializable
{
    //Variaveis
    
    private String codProduto;
    private String Descricao;
    private Double Quantidade;
    private Double ValorUnitario;
    
    //Instancias
    
    public LinhaEncomenda(){
        this.codProduto = "n/a";
        this.Descricao = "n/a";
        this.Quantidade = 0.0;
        this.ValorUnitario = 0.0;
    }
    
    public LinhaEncomenda(String cod, String desc, Double quant, Double valor){
        this.codProduto = cod;
        this.Descricao = desc;
        this.Quantidade = quant;
        this.ValorUnitario = valor;
    }
    
    public LinhaEncomenda (LinhaEncomenda e){
        this.codProduto = e.getCodProduto();
        this.Descricao = e.getDescricao();
        this.Quantidade = e.getQuantidade();
        this.ValorUnitario = e.getValorUnitario();
    } 
    
    //Getters
    
    public String getCodProduto(){
        return this.codProduto;
    } 
    
    public String getDescricao(){
        return this.Descricao;
    } 
    
    public Double getQuantidade(){
        return this.Quantidade;
    }  
    
    public Double getValorUnitario(){
        return this.ValorUnitario;
    }    
    
    //Setters
    
    public void setCodProduto(String cod){
        this.codProduto = cod;
    } 
    
    public void setDescricao(String desc){
        this.Descricao = desc;
    } 
    
    public void setQuantidade(Double quant){
        this.Quantidade = quant;
    }  
    
    public void setValorUnitario(Double val){
        this.ValorUnitario = val;
    }   
    
    //Equals
    
    public boolean equals (Object o){ 
      if (o == this) return true;
      if (o == null || o.getClass() != this.getClass()) return false;
      LinhaEncomenda e = (LinhaEncomenda) o;
      return this.codProduto.equals(e.getCodProduto())&&
             this.Descricao.equals(e.getDescricao())&&
             (this.Quantidade == e.getQuantidade()) &&
             (this.ValorUnitario == e.getValorUnitario());
    }
    
    //toString
    
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Codigo do Produto: ").append(this.codProduto).append(",")
          .append("Descricao: ").append(this.Descricao).append(",")
          .append("Quantidade: ").append(this.Quantidade).append(",")
          .append("Valor Unitario: ").append(this.ValorUnitario);
        return sb.toString();  
    }    
    
    //Clone
    
    public LinhaEncomenda clone(){
        return new LinhaEncomenda(this); 
    }
    
    public String toStringCSV(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.codProduto).append(",")
          .append(this.Descricao).append(",")
          .append(this.Quantidade).append(",")
          .append(this.ValorUnitario);
        return sb.toString();  
    }    
}
