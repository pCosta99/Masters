import java.util.Comparator;

public class ComparadorDataEncomenda implements Comparator<Encomenda> {
    @Override
    public int compare(Encomenda o1, Encomenda o2) {
        if(o1.getHoraF().isBefore(o2.getHoraF())){
            return -1;
        }
        if(o1.getHoraF().isAfter(o2.getHoraF())){
            return 1;
        }
        if(o1.getHoraF().isEqual(o2.getHoraF())){
            return 0;
        }
        return 0;
    }
}
