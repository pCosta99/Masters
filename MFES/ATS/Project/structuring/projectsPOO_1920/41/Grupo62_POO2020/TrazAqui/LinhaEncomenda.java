import java.io.*;

public class LinhaEncomenda implements Serializable
{
    private String codProd;
    private String descProd;
    private double quantProd;
    private double valorProd;
    
    public LinhaEncomenda(){
        this.codProd = "";
        this.descProd = "";
        this.quantProd = 0.0;
        this.valorProd = 0.0;
    }
    
    public LinhaEncomenda(String cp, String dp, double qp, double vp){
        this.codProd = cp;
        this.descProd = dp;
        this.quantProd = qp;
        this.valorProd = vp;
    }
    
    public LinhaEncomenda(LinhaEncomenda le){
        this.codProd = le.getCodProd();
        this.descProd = le.getDescProd();
        this.quantProd = le.getQuantProd();
        this.valorProd = le.getValorProd();
    }
    
    public String getCodProd(){
        return this.codProd;
    }

    public void setCodProd(String cp){
        this.codProd = cp;
    }
    
    public String getDescProd(){
        return this.descProd;
    }

    public void setDescProd(String dp){
        this.descProd = dp;
    }
    
    public double getQuantProd(){
        return this.quantProd;
    }

    public void setQuantProd(double qp){
        this.quantProd = qp;
    }
    
    public double getValorProd(){
        return this.valorProd;
    }

    public void setValorProd(double vp){
        this.valorProd = vp;
    }
    
    public LinhaEncomenda clone(){
        return new LinhaEncomenda(this);
    }
    
    public String toString(){
        String s = "Código: " + codProd + 
        "\nDescrição: " + descProd + 
        "\nQuantidade: " + quantProd + 
        "\nValor do Produto: " + valorProd;
        
        return s;
    }
    
    public boolean equals(Object o){
        
        boolean b = false;
        if(this == o){
            return true;
        }
        
        if(o == null || this.getClass() != o.getClass()){
            return false;
        }
        
        LinhaEncomenda le = (LinhaEncomenda) o;
        
        if(this.codProd.equals(le.getCodProd()) && this.descProd.equals(le.getDescProd()) && this.quantProd == le.getQuantProd() &&
        this.valorProd == le.getValorProd()) 
        return true;
        
        return b;
    }
}
