
/**
 * Write a description of class ComparatorNumProds here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
import java.util.Comparator;
import java.io.Serializable;

public class ComparatorNumProds implements Comparator <Encomenda>, Serializable
   {
    public int compare(Encomenda e1, Encomenda e2)
    {
        if (e1.getSize() < e2.getSize()){
            return -1;
        }
        if (e1.getSize() > e2.getSize()){
            return 1;
        }
        return 0;
        
    }
}

/*
 * 
 */