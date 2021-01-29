
import javax.sql.RowSetWriter;
import java.io.Serializable;
import  java.util.*;

public class ComparadorNrVezes implements Comparator<Cliente>, Serializable {

    public int compare(Cliente c1,Cliente c2){
        if (c1.getRe().size()+c1.getRv().size()> c2.getRe().size()+c2.getRv().size()) return -1;
        else return 1;
    }
}

