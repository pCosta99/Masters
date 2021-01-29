import java.util.Comparator;

public class ComparatorCV implements Comparator<Voluntario> {
    public int compare(Voluntario v1, Voluntario v2){
        if(v1.calculaCF()-v2.calculaCF() < 0) return 1;
        if(v1.calculaCF()-v2.calculaCF() > 0) return -1;
        else return 0;
    }
}
