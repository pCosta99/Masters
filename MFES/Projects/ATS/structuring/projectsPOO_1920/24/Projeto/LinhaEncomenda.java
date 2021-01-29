import java.io.Serializable;

public class LinhaEncomenda implements Serializable
{
    private String prodCode;
    private String desc;
    private double quant;
    private double price;
    
    public LinhaEncomenda(){
        this.prodCode = "";
        this.desc="";
        this.quant = 0;
        this.price = 0;
    }
    
    public LinhaEncomenda(String codigo, String nome, double quantidade, double preco){
        this.prodCode = codigo;
        this.desc = nome;
        this.quant = quantidade;
        this.price = preco;
    }
    
    public LinhaEncomenda (LinhaEncomenda l){
        this.prodCode = l.getProdCode();
        this.desc = l.getDesc();
        this.quant = l.getQuant();
        this.price= l.getPrice();
    }
    
    public String getProdCode(){
        return this.prodCode;
    }
    
    public String getDesc(){
        return this.desc;
    }
    
    public double getPrice(){
        return this.price;
    }
    
    public double getQuant(){
        return this.quant;
    }
    
    public void setProdCode(String code){
        this.prodCode=code;
    }
    
    public void setDesc(String descricao){
        this.desc= descricao;
    }
    
    public void setPrice(double preco){
        this.price=preco;
    }
    
    public void setQuant(double quantidade){
        this.quant=quantidade;
    }
             
    public boolean equals (Object o)
    {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        
        LinhaEncomenda l = (LinhaEncomenda) o;
        return(this.prodCode.equals(l.getProdCode()) && this.quant==l.getQuant());
    }
    
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append("\n Linha Encomenda: ");
        sb.append("\n   Codigo do Produto: "+this.prodCode);
        sb.append("\n   Descrição: "+this.desc);
        sb.append("\n   Quantidade: "+this.quant);
        sb.append("\n   Preço por unidade: "+this.price);
        sb.append("\n");
        return sb.toString();
    }
    
    public LinhaEncomenda clone()
    {
        return new LinhaEncomenda (this);
    }
    
    public String toStringCSV(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.prodCode).append(",").append(this.desc).append(",").append(this.quant).append(",").append(this.price).append(",");
        return sb.toString();
    }
}
