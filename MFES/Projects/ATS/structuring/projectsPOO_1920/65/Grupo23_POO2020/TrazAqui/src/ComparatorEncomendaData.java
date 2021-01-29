import java.util.Comparator;

public class ComparatorEncomendaData implements Comparator<Encomenda> {
    public int compare(Encomenda e1, Encomenda e2){
            return e1.getDataChegada().compareTo(e2.getDataChegada());
    }
}
