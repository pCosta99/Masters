import java.io.Serializable;
import java.util.Comparator;

/**Comparator utilizado para ordenar as datas dos registos*/
public class ComparatorData implements Comparator<Registos>, Serializable {
        public int compare (Registos a, Registos b){
           return a.getData().compareTo(b.getData());
        }
}

