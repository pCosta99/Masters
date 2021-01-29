import java.io.BufferedWriter;
import java.io.BufferedReader;
import java.io.FileWriter;
import java.io.FileReader;
import java.util.List;
import java.util.ArrayList;
/**
 * Escreva a descrição da classe Buyer aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class Buyer extends User
{
    
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio
    /**
    * COnstrutor para objetos da classe Buyer
     */
    public Buyer()
    {
        super();
        
    }
    
    public Buyer( String username , String codigo , String password , double locationX, double locationY,String email){
            super(username,codigo,password ,locationX,locationY,email); 
            
    }

    public Buyer(Buyer p){
            super(p);
            
    }
        

    public boolean equals(Object o){
        if(o==this) return true;
        if(o== null || o.getClass()!=this.getClass()) return false;
        Buyer c= (Buyer) o;
        return super.equals(c);
    }
    
    public String toString(){
        return super.toString();
    }
    
    public Buyer clone(){
        return new Buyer(this);
    }
    
    
    
    // NOT THE DEFAULT FUNCTIONS
    public String stringtoFile(){
        StringBuilder sb = new StringBuilder();
        sb.append("Utilizador:"+this.getCodigo());
        sb.append(","+this.getPassword());
        sb.append(","+this.getUserName());
        sb.append(","+this.getLocationX());
        sb.append(","+this.getLocationY());
        sb.append(","+this.getEmail());
        return(sb.toString());
    }
    

    
}
