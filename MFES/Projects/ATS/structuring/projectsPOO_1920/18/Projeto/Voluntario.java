import java.util.List;
import java.util.Map;
import java.time.LocalDateTime;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;


public class Voluntario extends Transportador{
    
    private boolean disponivel;
    
    public Voluntario(){
        super();
        this.disponivel=false;
    }
    
    public Voluntario(GPS g, String nemail, String npassword,String nnome, String cod, float r, boolean tm
                    , GestaoEncomendas eR, GestaoEncomendas eE,Map<String,Double> m,boolean d,float k){
        super(g,nemail,npassword,nnome,cod,r,tm,eR,eE,m,k);
        this.disponivel=d;
    } 
    
    public Voluntario(Voluntario et){
        super(et);
        this.disponivel=et.getDisponibilidade();
    }
    
    public void setDisponibilidade(boolean b){
        this.disponivel=b;
    }
    
    public boolean getDisponibilidade(){
        return this.disponivel;
    }
    
    public boolean equals (Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Voluntario t = (Voluntario) obj;
        return super.equals(t) && this.disponivel==t.getDisponibilidade();
    }
    
    public Voluntario clone(){
        return new Voluntario(this);
    }
    
    public String toString (){
        StringBuilder sb=new StringBuilder();
        sb.append("Voluntario:").append("GPS:").append(this.getGps()).
                append(",").append("Email:").append(this.getEmail()).
                append(",").append("Nome:").append(this.getNome()).
                append(",").append("Password:").append(this.getPassword()).
                append(",").append("Codigo:").append(this.getCodigo()).
                append(",").append("Raio:").append(this.getRaio()).
                append(",").append("Transporta Medicamentos:").append(this.getTransportaMedicamentos()).
                append(",").append("Email:").append(this.getEmail()).
                append(",").append("Encomendas:").append(this.getGEncomendas()).
                append(",").append("Encomendas efetuadas:").append(this.getGEncomendas()).append("\n");
        return sb.toString();
    }
    
    public String paraEstado(){
        StringBuilder sb=new StringBuilder();
        sb.append("Voluntario:").append(super.paraEstado()).append(",").append(this.getRaio());
        return sb.toString();
    }
    

}
