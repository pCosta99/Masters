import java.util.Comparator;

public class ClassLojaComparator implements Comparator<Loja> {
    @Override
    public int compare(Loja o1, Loja o2) {
        return (int) (o1.classMedia() - o2.classMedia());
    }
}
