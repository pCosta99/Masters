package Perfis;

/**
 * Escreva a descrição da classe Utilizador.Loja aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class Loja extends Perfil
{
    private int tempoEspera ;
    private String morada;
   
    public Loja(){
        super();
        this.tempoEspera=0;
        this.morada= "";
        
   }
   public Loja(String email, String nome, String password,
               Ponto2D local, int tempoEspera,
               String morada){
       super(email,nome,password,local);
       this.tempoEspera=tempoEspera;
       this.morada=morada;
     
   }
   public Loja (Loja um){
       super(um);
       this.tempoEspera=um.getTempoEspera();
       this.morada=um.getMorada();
          
   }
  
   public int getTempoEspera(){
       return this.tempoEspera;
    }
   public String getMorada(){
       return this.morada;
    }
    
   public void setTempoEspera(int tempoEspera){
       this.tempoEspera=tempoEspera;
    }
   public void setMorada(String morada){
       this.morada=morada;
    }
   
    
   @Override
   public String toString(){
       StringBuilder sb = new StringBuilder();

       sb.append( super.toString());
       sb.append("Tempo de espera: ");
       sb.append(this.tempoEspera).append("\n");
       sb.append("Morada: ");
       sb.append(this.morada);
     

       return sb.toString();
    }
   
   public Loja clone(){return new Loja(this);}
}
