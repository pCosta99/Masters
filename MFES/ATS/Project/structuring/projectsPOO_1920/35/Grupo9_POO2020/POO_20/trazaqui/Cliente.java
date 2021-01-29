import java.util.List;
import java.util.ArrayList;
import java.util.stream.Collectors;
import java.io.Serializable;

public class Cliente extends Utilizador {
   
   //Variaveis de instância
   private List<Encomenda> historico; 
   private String nifC; //nif do cliente
   
   /**
    * Construtor vazio
    */
   public Cliente(){
       super("Cliente","",null,"","","");
       this.historico = new ArrayList<Encomenda>();
       this.nifC = "";
   }

   /**
    * Construtor parametrizado 
    */
   public Cliente(String id, String nome, Coordenadas coordenadas, String email, String password,
   String morada, ArrayList<Encomenda> hist, String nifC){
       super(id, nome, coordenadas, email, password, morada);
       this.historico = hist.stream().collect(Collectors.toList());
       this.nifC = nifC;
   }
    
   /**
    * Construtor de cópia 
    */
   public Cliente(Cliente c){
       super(c);
       this.historico = c.getHist();
       this.nifC = c.getNifC();
   }
    
   //Getters
   
   public List<Encomenda> getHist(){
       if(this.historico == null) return new ArrayList<Encomenda>();
       else return this.historico;
   }
    
   public String getNifC(){
       return this.nifC;
   }
   
   //Setters 
   
   public void setHist(ArrayList<Encomenda> hist){
       this.historico = hist.stream().collect(Collectors.toList());
   }
    
   public void setNifC(String nc){
       this.nifC = nc;
   }
   
   /**
    * Metodo Equals
    */
   public boolean equals(Object o){
       if (this == o)
           return true;
        
       if (o == null || this.getClass() != o.getClass())
           return false;
        
       Cliente c = (Cliente) o;
        
       return super.equals(c) &&
       c.getHist().equals(historico) &&
       this.nifC.equals(c.getNifC());
   }
   
   public String toString(){
       StringBuilder sb = new StringBuilder(); 
       sb.append("O histórico de Viagem é: "+ this.getHist() + "\n");
       sb.append("Nif Cliente: " + this.getNifC() + "\n");
       return sb.toString();
   }
   
   //Clone
      
   public Cliente clone (){
       return new Cliente(this);    
   }
    
}