
/**
 * Escreva a descrição da classe UtilizadoresTop10Comparator aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.util.Comparator;
import java.io.Serializable;
public class UtilizadoresTop10Comparator implements Comparator<Utilizadores>, Serializable
{
    public int compare(Utilizadores u1, Utilizadores u2){
        if (u1.getRegistos().size() > u2.getRegistos().size()) {
            return -1;
        }
        else if (u1.getRegistos().size() < u2.getRegistos().size()){
            return 1;
        }
        else return 0;
    }
}
