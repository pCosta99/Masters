import java.util.Comparator;
import java.io.Serializable;

public class ComparadorT implements Comparator<Transportadora>, Serializable {
    public int compare(Transportadora a, Transportadora b){
        int r = 0;
        if(a.getKms() < b.getKms()) {
            return 1;
        }
        else if(a.getKms() > b.getKms()){
            return -1;
        }
        return 0;
    }
}