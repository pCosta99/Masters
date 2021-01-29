
/**
 * Escreva a descrição da classe Encomenda aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.util.*;
import java.time.LocalDate;
public class Encomenda extends Encomendas
{
   public Encomenda(){
       super();
    }
       
   public Encomenda(String coden,String codut,String codlo,double auxpeso,List<LinhaDeEncomenda> l,LocalDate t,LocalDate t1){
     super(coden,codut,codlo,auxpeso,l,t,t1);
    }
    
   public Encomenda(Encomenda enc) {
       super(enc);
      }
   
   public String toString() {
      StringBuilder sb = new StringBuilder();
      
      sb.append("Encomenda:");
      sb.append(this.getCodencomenda());
      sb.append(","+this.getCodutilizador()); 
      sb.append(","+this.getCodloja());
      sb.append(","+this.getPeso());
      sb.append(","+this.getTempo());
      sb.append(","+this.getTempo1());
      
      for( LinhaDeEncomenda lj:this.getLinha()){
      sb.append(","+lj.toString());
     }
     
      return sb.toString();
      
    } 
        
         
   public Encomenda clone(){
        return new Encomenda(this);
        }
        
   
}    
   