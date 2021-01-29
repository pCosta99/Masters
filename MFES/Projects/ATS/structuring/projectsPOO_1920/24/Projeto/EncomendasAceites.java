import java.io.Serializable;

public class EncomendasAceites implements Serializable
{
    private String codigoEncAceite;
  
    public EncomendasAceites()
    {
        this.codigoEncAceite="";
    }
    
    public EncomendasAceites(String enc)
    {
        this.codigoEncAceite=enc;
    }
    
    public EncomendasAceites(EncomendasAceites a)
    {
        this.codigoEncAceite = a.getCodigoEncAceite();
    }
    
    public String getCodigoEncAceite()
    {
        return this.codigoEncAceite;
    }
    
    public void setCodigoEncAceite(String cod)
    {
        this.codigoEncAceite=cod;
    }
    
    public boolean equals (Object o)
    {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        
        EncomendasAceites e = (EncomendasAceites) o;
        return(this.codigoEncAceite.equals(e.getCodigoEncAceite()));
    }
    
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append("\nEncomenda Aceite:");
        sb.append("\nCodigo da Encomenda:"+this.codigoEncAceite);
        return sb.toString();
    }
    
    public EncomendasAceites clone()
    {
        return new EncomendasAceites (this);
    }
    
    public String toStringCSV(){
        StringBuilder sb = new StringBuilder();
        sb.append("Aceite:").append(this.codigoEncAceite);
        return sb.toString();
    }
    
}
