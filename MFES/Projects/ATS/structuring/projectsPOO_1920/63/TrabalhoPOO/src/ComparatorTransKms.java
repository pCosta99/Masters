import java.util.Comparator;

public class ComparatorTransKms implements Comparator<Transportadora> {
    @Override
    public int compare(Transportadora t1, Transportadora t2) {
        if (t1.totalKms()==t2.totalKms()) return t1.getCodTransportadora().compareTo(t2.getCodTransportadora());
        return (int) (t2.totalKms() - t1.totalKms());
    }
}
