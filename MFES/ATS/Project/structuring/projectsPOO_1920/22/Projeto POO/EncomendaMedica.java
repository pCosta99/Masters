
/**
 * Escreva a descrição da classe EncomendaMedica aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.util.*;
import java.time.LocalDate;
public class EncomendaMedica extends Encomendas
{
    public EncomendaMedica(){
     super();
    }
    
    public EncomendaMedica(String coden,String codut,String codlo,double auxpeso,List<LinhaDeEncomenda> l,LocalDate t,LocalDate t1){
      super(coden,codut,codlo,auxpeso,l,t,t1);
      
    }
    
    public EncomendaMedica(EncomendaMedica em){
     super(em);
    }
    
    public String toString() {
      StringBuilder sb = new StringBuilder();
      sb.append("EncomendaMedica:");
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
         
    public EncomendaMedica clone(){
        return new EncomendaMedica(this);
        }
}
