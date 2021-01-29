package Model;

import java.io.Serializable;
import java.util.Comparator;

public class ComparatorTransp implements Comparator<Transportadora>, Serializable {

    public int compare (Transportadora t1, Transportadora t2){
        double km1 =(double) t1.getKms();
        double km2 =(double) t2.getKms();
        if(km1<km2) return 1;
        else if(km1>km2) return -1;
        else return t1.getCod().compareTo(t2.getCod());
    }
}
