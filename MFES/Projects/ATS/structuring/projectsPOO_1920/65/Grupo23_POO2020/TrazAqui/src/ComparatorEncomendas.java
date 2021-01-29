import java.util.Comparator;

public class ComparatorEncomendas implements Comparator<Encomenda>{
    public int compare(Encomenda e1, Encomenda e2){
        return e1.getCodEncomenda().compareTo(e2.getCodEncomenda());
    }
}
