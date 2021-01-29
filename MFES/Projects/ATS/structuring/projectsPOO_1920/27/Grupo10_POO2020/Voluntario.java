/**
 * Escreva a descrição da classe Voluntario aqui.
 * 
 * @author Anabela Pereira - A87990, Fernando Lobo - A87988, Márcia Cerqueira - A87992; 
 * @version 20200611
 */
import java.util.*;
import java.time.LocalDateTime;
import java.io.Serializable;

public class Voluntario extends Transporte implements Serializable{
   /**
    * Construtores
    */
   public Voluntario(){
       super();
   }

   public Voluntario(String codT, String nome, double kms, Coordenadas gps, double raio, double mediaclassificacao, double velocidade, boolean disponivel, List<Encomenda> registoT, boolean certificado){
       super(codT, nome, kms, gps, raio, mediaclassificacao, velocidade, disponivel,registoT, certificado);
   }
    
   public Voluntario(Voluntario v){
       super(v);
   }
    
   /**
    * clone
    */
   public Voluntario clone(){
       return new Voluntario(this);
   }
   
   /**
    * equals
    */
   public boolean equals(Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Voluntario l = (Voluntario) obj;
        return super.equals(l);
   }
   /**
    * toString
    */
   public String toString(){
        return super.toString();
   } 
   
   /**
    * Compara os codigos dos voluntários
    */
   public int compareTo(Voluntario x){
        return this.getCodT().compareTo(x.getCodT());
   }
}
