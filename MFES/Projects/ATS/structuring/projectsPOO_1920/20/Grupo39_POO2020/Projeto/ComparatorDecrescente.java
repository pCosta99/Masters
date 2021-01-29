
/**
 * Write a description of class ComparatorDecrescente here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
import java.util.Comparator;
import java.io.Serializable;

public class ComparatorDecrescente implements Comparator <Encomenda>, Serializable
   {
    public int compare(Encomenda e1, Encomenda e2)
    {
        if (e1.calculaValorTotal() < e2.calculaValorTotal()){
            return 1;
        }
        if (e1.calculaValorTotal() > e2.calculaValorTotal()){
            return -1;
        }
        return 0;
        
    }
}

/*
 * 
 */
