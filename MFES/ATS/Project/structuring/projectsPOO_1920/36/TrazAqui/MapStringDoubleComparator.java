
/**
 * 
 *
 * @author Artur Drohobytskyy
 * @version 1.0
 */
import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.Comparator;
import java.io.Serializable;

public class MapStringDoubleComparator implements Comparator, Serializable {
    
    Map<String, Double> base;

    public MapStringDoubleComparator(Map<String, Double> base) {
        this.base = base;
    }

    public int compare(Object a, Object b) {
        if ((Double) base.get(a) == (Double) base.get(b)) {
            return 0;
        } else if((Double) base.get(a) < (Double) base.get(b)) {
            return 1;
        }else{
            return -1;
        }
    }
}


