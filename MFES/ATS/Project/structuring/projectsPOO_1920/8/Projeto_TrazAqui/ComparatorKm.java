import java.util.Comparator;

public class ComparatorKm implements Comparator<Empresaentrega> {
    public int compare(Empresaentrega e1, Empresaentrega e2){
        if (e1.getDistancia() == e2.getDistancia()){
            return e1.getUsername().compareTo(e2.getUsername());
        }
        if (e2.getDistancia() > e1.getDistancia()){
            return 1;
        }
        return -1;
    }
}