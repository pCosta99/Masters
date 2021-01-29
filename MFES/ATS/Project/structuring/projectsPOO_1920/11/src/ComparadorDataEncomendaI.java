import java.util.Comparator;

public class ComparadorDataEncomendaI implements Comparator<Encomenda> {
    @Override
    public int compare(Encomenda o1, Encomenda o2) {
        if(o1.getHoraI().isBefore(o2.getHoraI())){
            return -1;
        }
        if(o1.getHoraI().isAfter(o2.getHoraI())){
            return 1;
        }
        if(o1.getHoraI().isEqual(o2.getHoraI())){
            return 0;
        }
        return 0;
    }
}
