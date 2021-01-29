import java.util.Comparator;
/**
 * Escreva a descrição da classe ComparadorUtilizacao aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class ComparadorDistancia implements Comparator<Transportadora>
{
   public int compare (Transportadora u1, Transportadora u2){
       double totalU1 = 0.0;
       double totalU2 = 0.0;
       for(Entregas e : u1.getEntregas()){
           totalU1 += e.getKm();
        }
       for(Entregas n : u2.getEntregas()){
           totalU2 += n.getKm();
        }
       
       if(totalU1 < totalU2)
            return -1;
            else if(totalU1 == totalU2)
            return 0;
            else 
                return 1;
    }
}
