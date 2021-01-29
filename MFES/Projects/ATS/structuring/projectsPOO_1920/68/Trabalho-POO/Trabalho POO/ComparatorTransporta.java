import java.io.Serializable;
import java.util.Comparator;

public class ComparatorTransporta implements Comparator<Transporta>, Serializable {

    public int compare (Transporta t1, Transporta t2) {
        if (t1.getKm_percorridos() == t2.getKm_percorridos()) {
            return -(t1.compareTo(t2));
        }
        if (t1.numeroEncomendas() < t2.numeroEncomendas()) return 1;
        return -1;
    }

}
