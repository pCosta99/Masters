import java.lang.String;
import java.util.Map;
import java.util.HashMap;   
import java.util.stream.Collectors;
import java.util.List;

public class LojaComFila extends Loja{
    
    private int fila;

    public LojaComFila(){
        super();
        this.fila=0;
    }
    

    public LojaComFila(String cod, String nome, GPS gps, GestaoEncomendas ge,GestaoEncomendas gee,String email, 
    String pass,int f,List<String> l,List<String> l2,Map<String,List<List<LinhaEncomenda>>> l3){
        super(cod,nome,gps,email, pass,ge,gee,l,l2,l3);
        this.fila=f;        
    }
    
    public LojaComFila(LojaComFila l){
        this.setCodigo(l.getCodigo());
        this.setNome(l.getNome());
        this.setGEncomendas(l.getGEncomendas());
        this.setEncomendasEfetuadas(l.getEncomendasEfetuadas());
        this.setGps(l.getGps());
        this.setPassword(l.getPassword());
        this.setEmail(l.getEmail());
        this.fila=l.getFila();
        this.setEncomendasSolicitadas(l.getEncomendasSolicitadas());
        this.setEncomendasComTransportador(l.getEncomendasComTransportador());
        this.setPedidos(l.getPedidos());
    }
    
    public int getFila(){
        return this.fila;
    }
    
    public void setFila(int f){
        this.fila=f;
    }
    
    public String toString (){
        StringBuilder sb=new StringBuilder();
        sb.append("Loja com Fila:")
                .append("Nome:").append(this.getNome()).append(";")
                .append("CodLoja:").append(this.getCodigo()).append(";")
                .append("GPS:").append(this.getGps()).append(";")
                .append("Encomendas:").append(this.getGEncomendas()).append(";")
                .append("Encomendas Efetuadas:").append(this.getEncomendasEfetuadas()).append(";")
                .append("GPS:").append(this.getGps()).append(";")
                .append("Email:").append(this.getEmail()).append(";")
                .append("Password:").append(this.getPassword()).append(";")
                .append("Tamanho da fila:").append(this.fila).append(";\n");
        return sb.toString();
    }
    
    public boolean equals (Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        LojaComFila lo = (LojaComFila) obj;
        return lo.getFila()==this.fila &&
                    super.equals(lo);
    }
    
    public LojaComFila clone (){
        return new LojaComFila(this);
    }
    
   
}
