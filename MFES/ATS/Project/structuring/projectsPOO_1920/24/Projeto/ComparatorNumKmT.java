import java.io.Serializable;
import java.util.Comparator;

public class ComparatorNumKmT implements Comparator<Transportadoras>, Serializable {
    
    //Crescente
    public int compare(Transportadoras t1, Transportadoras t2){
        double numKm1 = t1.getTotalKm();
        double numKm2 = t2.getTotalKm();

        if (numKm1 < numKm2)
            return 1;
        if (numKm1 > numKm2)
            return -1;
        
        return 0;
    }
}