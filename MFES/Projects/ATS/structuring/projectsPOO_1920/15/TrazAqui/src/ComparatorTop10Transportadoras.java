

import java.util.Comparator;

public class ComparatorTop10Transportadoras implements Comparator<Transportadora> {

    public int compare(Transportadora t1, Transportadora t2){
        if (t2.getKm_Percorridos() > t1.getKm_Percorridos()){
            return 1;
        }else{
            return -1;
        }
    }
}
