

import java.io.Serializable;
import java.util.Comparator;


public class ComparatorNumeroKmTrans implements Comparator<Transportadora>{

    public int compare(Transportadora a,Transportadora b){
        if(a.getKmTotais() < b.getKmTotais()) return 1;
        if(a.getKmTotais() > b.getKmTotais()) return -1;
        return a.getCodigo().compareTo(b.getCodigo());
    }
}