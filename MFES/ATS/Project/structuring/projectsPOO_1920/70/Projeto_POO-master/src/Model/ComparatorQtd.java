package Model;

import java.util.Comparator;

public class ComparatorQtd implements Comparator <Encomenda>
{
    public int compare (Encomenda e1, Encomenda e2){
        return Double.compare(e1.somaPeso(), e2.somaPeso());
    }
}
