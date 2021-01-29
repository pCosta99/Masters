import java.io.BufferedWriter;
import java.io.BufferedReader;
import java.io.FileWriter;
import java.io.FileReader;
import java.util.List;
import java.util.ArrayList;
import java.util.HashSet;

public class Loja extends User

{
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio
    private boolean filadeEspera;
    private static double tempoAtendimento = 5; //em minutos se tiver teempo acabar 
    private ArrayList<Encomenda> queue;
    
    
    

    public Loja()
    {
        super();
       
        
    }
    
    public Loja( String username , String codigo , String password , double locationX, 
    double locationY ,String email){
            super(username,codigo,password,locationX,locationY,email); 
            //this.filadeEspera=filadeEspera;

            //this.queue= new ArrayList<>();
            
    }

    public Loja(Loja p){
            super(p.getUserName(),p.getPassword(),p.getCodigo(),p.getLocationX(),p.getLocationY(),p.getEmail());
            //this.filadeEspera= p.getfiladeEspera();
            //this.queue = p.getqueue();
    }
    
    public ArrayList<Encomenda> getqueue()
    {
        return this.queue;
    }
    
    public void setProdutos(ArrayList<Encomenda> queue){
           this.queue=queue;
    }
        
    public boolean getfiladeEspera(){
           return this.filadeEspera;
    }
    
    public void setfiladeEspera(boolean filadeEspera){
           this.filadeEspera=filadeEspera;
    }

    public boolean equals(Object o){
        if(o==this) return true;
        if(o== null || o.getClass()!=this.getClass()) return false;
        Loja c= (Loja) o;
        return super.equals(c);
    }
    
    public String toString(){
        StringBuilder res = new StringBuilder();
        res.append(super.toString());
        res.append(this.filadeEspera);
        res.append(this.tempoAtendimento);
        return res.toString();
    }
    
    public Loja clone(){
        return new Loja(this);
    }
    
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

