import java.util.Comparator;

public class ComparadorPesoEncomenda implements Comparator<Encomenda> {
    @Override
    public int compare(Encomenda o1, Encomenda o2) {
        return (int)(o1.getPeso()-o2.getPeso());
    }
}
