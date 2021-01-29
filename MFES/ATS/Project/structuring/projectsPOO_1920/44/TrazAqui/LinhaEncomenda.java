import java.io.Serializable;

public class LinhaEncomenda implements Serializable
{
    private String CodProduto;
    private String Descricao;
    private double quantidade;
    private double valorunitario;
    
    public LinhaEncomenda(){
        this.CodProduto = "n/a";
        this.Descricao = "n/a";
        this.quantidade = 0;
        this.valorunitario = 0;
    }
    
    public LinhaEncomenda(String cod,String n,double q,double val){
        this.CodProduto = cod;
        this.Descricao = n;
        this.quantidade = q;
        this.valorunitario = val;
    }
    
    public LinhaEncomenda(LinhaEncomenda l){
        this.CodProduto = l.getCodproduto();
        this.Descricao = l.getDescricao();
        this.quantidade = l.getQuantidade();
        this.valorunitario = l.getValorunitario();
    }
    
    public String getCodproduto() {return this.CodProduto;}
    
    public void setCodproduto(String c){
        this.CodProduto = c;
    }
    
    public String getDescricao() {return this.Descricao;}
    
    public void setDescricao(String d){
        this.Descricao = d;
    }
    
    public double getQuantidade() {return this.quantidade;}
    
    public void setQuantidade(double y){
        this.quantidade = y;
    }
    
    public double getValorunitario() {return this.valorunitario;}
    
    public void setValorunitario(double sv){
        this.valorunitario = sv;
    }
    
    public double precolinha(){return this.valorunitario * this.quantidade;}
    
    public LinhaEncomenda clone(){
        return new LinhaEncomenda(this);
    }
    
    public String toString(){
        StringBuilder sb = new StringBuilder();
        
        sb.append("Cod: " + this.CodProduto).append(" Info: " + this.Descricao).
        append(" Quantidade: "+this.quantidade).append(" Valor: "+this.valorunitario);
        
        return sb.toString();
    }
    
    public boolean equals(Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass()!=this.getClass()) return false;
        LinhaEncomenda l = (LinhaEncomenda) obj;
        return this.CodProduto.equals(l.getCodproduto()) &&
               this.Descricao.equals(l.getDescricao()) &&
               this.quantidade == l.getQuantidade() &&
               this.valorunitario == l.getValorunitario();
    }
}
