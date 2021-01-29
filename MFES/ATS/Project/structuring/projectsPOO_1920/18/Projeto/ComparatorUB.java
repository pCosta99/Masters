import java.util.Comparator;

public class ComparatorUB implements Comparator<UtilizadorBasico>
{
    public int compare(UtilizadorBasico u1,UtilizadorBasico u2){
        if (u1.getTotalEncomendasEfetuadas()>u2.getTotalEncomendasEfetuadas()) return -1;
        if (u1.getTotalEncomendasEfetuadas()<u2.getTotalEncomendasEfetuadas()) return 1;
        return 1;
    }
  
}

