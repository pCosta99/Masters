import java.util.Comparator;

public class ComparatorCT implements Comparator<Transportadora>{
    public int compare(Transportadora t1, Transportadora t2){
        if(t1.calculaCF()-t2.calculaCF() < 0) return 1;
        if(t1.calculaCF()-t2.calculaCF() > 0) return -1;
        else return 0;
    }
}
