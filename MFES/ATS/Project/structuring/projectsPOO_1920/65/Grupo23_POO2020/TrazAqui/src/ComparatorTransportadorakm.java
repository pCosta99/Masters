import java.util.Comparator;

public class ComparatorTransportadorakm implements Comparator<Transportadora> {
    public int compare(Transportadora t1, Transportadora t2){
        if(t1.getKms() - t2.getKms() < 0) return -1;
        if(t1.getKms() - t2.getKms() > 0) return 1;
        return 0;
    }
}
