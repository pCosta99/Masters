import java.io.Serializable;

public class Avaliacao implements Serializable{
    
   //Variaveis de instância
   private int avaliacao; //avaliacao do serviço de entrega
    
   /**
    * Construtor vazio
    */
   public Avaliacao(){
       avaliacao = 0;
   }

   /**
    * Construtor por parametrizado
    */ 
   public Avaliacao(int avaliacao){
       this.avaliacao = avaliacao;
   }

   /**
    * Construtor por cópia
    */
   public Avaliacao(Avaliacao a){
       this.avaliacao = a.getAvaliacao();
   }

   //Getters

   public int getAvaliacao(){
       return this.avaliacao;
   }

   //Setters
   
   public void setAvaliacao(int a){
       this.avaliacao = a;
   }
   
   /**
    * Metodo Equals
    */
   public boolean equals(Object o){
       if (this == o) 
           return true;
        
       if (o == null || o.getClass() != this.getClass()) 
           return false;

       Avaliacao a = (Avaliacao) o;
        
       return this.avaliacao == a.getAvaliacao();
   }

   public String toString (){
       StringBuilder sb = new StringBuilder();
       sb.append("Avaliação:" + this.getAvaliacao() + "\n");
       return sb.toString();
    }
   
   //Clone
   
   public Avaliacao clone(){
       return new Avaliacao(this);
   }
    
}