import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Lojas extends AllUsers implements Serializable
{
    //Loja vai ter de implementar uma lista de espera
    
    public Lojas()
    {
        super();
    }
    
    public Lojas(String codigo, String nome, Coordenadas gps, String email, String password, List <Integer> classificacao)
    {
        super(codigo, nome, gps, email, password, classificacao);
    }
    
    public Lojas (Lojas l)
    {
        super(l);
    }
    
    public boolean equals (Object o)
    {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        
        Lojas t = (Lojas) o;
        return(super.equals(t));
    }
    
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        return sb.toString();
    }
    
    public Lojas clone()
    {
        return new Lojas (this);
    }
    
    public String tipoUtilizador()
    {
        return "Loja";
    }
    

    @Override
    public ArrayList<Integer> getClassificacao(){
        return new ArrayList<>();
    }
}