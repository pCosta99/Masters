import java.time.LocalDate;
import java.util.ArrayList;

/**
 * Escreva a descrição da classe Medicas aqui.
 * 
 * @author (seu nome)
 * @version (número de versão ou data)
 */
import java.io.*;

public class Medicas extends Encomenda implements Serializable {
   //private String especificacao;
   public Medicas(){
      // gerar encomenda vazia
      super();
   }

   public Medicas(String codU,String codEnc,String codLoja, LocalDate dataEncomenda, double peso,ArrayList <LinhaEncomenda> linhas,String entregue){
      // gerar uma encomenda
      super(codU,codEnc,codLoja,dataEncomenda,peso,linhas,entregue);
   }
   
   public Medicas ( Medicas encomenda){
        super(encomenda);
   }
    
   public Medicas clone() {
        return new Medicas(this);
   }
   
   public boolean equals(Object o) {
        if (o==this) return true;
        if (o==null || o.getClass() != this.getClass()) return false;
        Medicas v = (Medicas) o;
        
        return super.equals(v);
   }
   
   public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        return sb.toString();
    }
}
