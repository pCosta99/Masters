import java.io.Serializable;
import java.util.Comparator;
/**
 * Vai comparar por ordem decrescente numero registos (size da lista que contem
 * os registos de encomendas das varias entidades)
 * Se o tamanho for maior vai ordenar pelo nome
 */

public class ComparatorNumRegistos implements Comparator < User > , Serializable
{
   
    public int compare (User a , User b){ 

        int x = a.verRegistosGeral().size();
        int y = b.verRegistosGeral().size();

        if (y - x != 0) return y - x;

        else return b.getNome().compareTo(a.getNome());

    }
}