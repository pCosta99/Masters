import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;

public class Voluntario extends Transportador implements Serializable{
    
   //Variaveis de instância

   /**
    * Construtor vazio
    */
   public Voluntario(){
        super("Voluntário","",null,"","","",0,0,null,null); 
   }
   
   /**
    * Construtor por parametrizado
    */
   public Voluntario(String id, String nome, Coordenadas coordenadas, 
   String email, String password, String morada, double classificacao, double raio, ArrayList<Avaliacao> avaliacoes,
   ArrayList<Encomenda> historico){
       super(id, nome, coordenadas, email, password, morada, classificacao, raio, avaliacoes, historico);
      
   }
   
   /**
    * Construtor por cópia
    */
   public Voluntario(Voluntario v) {
        super(v);
   }
   
   //Getters
   
   //Setters
   
   /**
    * Metodo Equals
    */
   public boolean equals(Object o){
       if (this == o)
           return true;
           
       if (o == null || this.getClass() != o.getClass())
            return false;
            
       Voluntario v = (Voluntario) o;
       
       return super.equals(v);
   }
   
   //Clone
    
   public Voluntario clone(){
       return new Voluntario(this);
   }
   
}