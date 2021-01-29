import java.util.Comparator;
/**
 * Write a description of class TransportadoraDistanciaComparator here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class ComparatorDistTransportadora implements Comparator<Transportadora> {

    public int compare(Transportadora t1, Transportadora t2){
        double dist1 = t1.getTotalKms();
        double dist2 = t2.getTotalKms();
        
        if(dist1 == dist2) return 0;
        
        if(dist1 < dist2) return 1;
        
        return -1;
    }
}
