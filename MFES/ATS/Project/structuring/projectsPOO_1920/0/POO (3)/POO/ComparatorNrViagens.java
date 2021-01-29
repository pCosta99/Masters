import java.util.Comparator;
/**
 * Write a description of class TransportadoraDistanciaComparator here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class ComparatorNrViagens implements Comparator<ClienteViagem> {

    public int compare(ClienteViagem c1, ClienteViagem c2){
        double nr1 = c1.getNrViagens();
        double nr2 = c2.getNrViagens();
        
        if(nr1 == nr2) return 0;
        
        if(nr1 < nr2) return 1;
        
        return -1;
    }
}
