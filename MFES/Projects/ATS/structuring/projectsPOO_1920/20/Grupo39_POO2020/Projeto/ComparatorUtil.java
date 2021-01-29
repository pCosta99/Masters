
/**
 * Write a description of class ComparatorUtil here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
import java.util.Comparator;
import java.io.Serializable;

public class ComparatorUtil implements Comparator <Utilizador>, Serializable
{
    public int compare(Utilizador u1, Utilizador u2){
        if(u1.getQuantas() < u2.getQuantas()){
            return 1;
        }
        
        if(u1.getQuantas() > u2.getQuantas()){
            return -1;
        }
        
        return 0;
    }
}
