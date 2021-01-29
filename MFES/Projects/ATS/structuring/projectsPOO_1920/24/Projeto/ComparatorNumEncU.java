import java.io.Serializable;
import java.util.Comparator;

public class ComparatorNumEncU implements Comparator<Utilizadores>, Serializable {
    
    //Crescente
    public int compare(Utilizadores u1, Utilizadores u2){
        double numenc1 = u1.getHistorico().size();
        double numenc2 = u2.getHistorico().size();

        if (numenc1 < numenc2)
            return 1;
        if (numenc1 > numenc2)
            return -1;
        
        return 0;
    }
}