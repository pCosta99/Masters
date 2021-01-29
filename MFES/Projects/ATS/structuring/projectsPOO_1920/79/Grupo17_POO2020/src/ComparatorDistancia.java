import java.io.Serializable;
import java.util.Comparator;

public class ComparatorDistancia implements Comparator<Encomenda> {

    public int compare(Encomenda e1,Encomenda e2){
        double distE1 = e1.calculaDistancia();
        double distE2 = e2.calculaDistancia();
        if (distE1 < distE2) return -1;
        if (distE1 > distE2) return 1;
        return e1.getCodEncomenda().compareTo(e2.getCodEncomenda());
    }

}