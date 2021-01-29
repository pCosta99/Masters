import java.io.Serializable;
import java.util.Comparator;
/**
 * Comparador que ordena por ordem decrescente kms percurridos , 
 * caso sejam tenham os mesmos kms percorridos, vai ordenar pelo nome
*/

public class ComparatorKm implements Comparator < Transportador >, Serializable
{
   
    public int compare (Transportador a , Transportador b){ 

        int x = (int) a.getKms();
        int y = (int) b.getKms();

        if (y - x != 0) return y - x;

        else return b.getNome().compareTo(a.getNome());

    }
}
