import java.util.Comparator;

public class NomeLojaComparator implements Comparator<Loja> {
    @Override
    public int compare(Loja o1, Loja o2) {
        return o1.getNome().compareTo(o2.getNome());
    }
}
