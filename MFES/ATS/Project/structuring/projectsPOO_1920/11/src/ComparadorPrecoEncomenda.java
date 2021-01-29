import java.util.Comparator;

public class ComparadorPrecoEncomenda implements Comparator<Encomenda> {
    @Override
    public int compare(Encomenda o1, Encomenda o2) {
        return (int) (o1.getPreco() - o2.getPreco());
    }
}
