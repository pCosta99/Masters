import java.io.Serializable;
/**
 * Escreva a descrição da classe Utilizador aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class Utilizador extends Perfil implements Serializable
{
   private int numEnc;//numero de encomendas
    public Utilizador(){
    super();
    this.numEnc=0;
   }
   public Utilizador(String email,String nome , String password,Ponto2D local,int numEnc){
    super(email,nome,password,local);
    this.numEnc=0;
   }
   public Utilizador (Utilizador um){
    super(um);
    this.numEnc=um.getNumEnc();
   }
   
   public int getNumEnc(){return this.numEnc;}
   
   public void setNum(int numEnc){this.numEnc=numEnc;}
   
   public void incNumEnc(){this.numEnc++;}
 
   public Utilizador clone(){return new Utilizador(this);}
   
   public String toString(){
        StringBuilder sb = new StringBuilder();
        
        sb.append( super.toString());
        
        sb.append("Numero de Encomenda: ");
        sb.append(this.numEnc + "\n");
        
        
        return sb.toString();
    }
}
