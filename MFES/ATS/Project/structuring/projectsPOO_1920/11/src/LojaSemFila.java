import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.stream.Collectors;

/**
 * Classe que trata das Lojas sem Fila de Espera
 * 
 * @author Rui Cunha
 * @version 13/04/2020
 */
public class LojaSemFila extends Loja
{
    //Construtor vazio
    public LojaSemFila()
    {
        super();
    }
    
    //Construtor por parametros
    public LojaSemFila(String username, String nome, String password,Localizacao local,
    HashMap<String,Encomenda> encomendas, HashMap<String,Integer> classificacao)
    {
        super(username,nome,password,local,encomendas,classificacao);
    }
    
    //Construtor por copia
    public LojaSemFila(LojaSemFila loj)
    {
        super(loj);
    }
    
    //Metodo toString
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString())
        .append("Fila de espera indisponivel de momento..." + "\n");
        return sb.toString();
    }
    
    //Metodo Equals
    public boolean equals(Object o)
    {
        if(this==o)
            return true;
        if((o==null) || (this.getClass() != o.getClass()))
            return false;
        
        LojaSemFila p = (LojaSemFila) o;
        return(super.equals(p));
    }
    
    //Clone
    public LojaSemFila clone()
    {
        return new LojaSemFila(this);
    }
}
