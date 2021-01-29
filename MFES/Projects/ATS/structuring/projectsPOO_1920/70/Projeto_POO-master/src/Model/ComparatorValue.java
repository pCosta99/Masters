package Model; /**
 * Write a description of class Model.ComparatorValue here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */

import java.util.Comparator;

public abstract class ComparatorValue implements Comparator<Encomenda>
{
    public int compare(Encomenda e1, Encomenda e2){
        return ((int) (e2.calculaValorTotal() - e1.calculaValorTotal()));
    }
}
